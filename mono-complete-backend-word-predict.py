# SPDX-License-Identifier: GPL-3.0-or-later

"""
Run this in the current working directory to extract words
from comments in code & markup.

Arguments:

"--cache"
   The directory to store cache files.

--input-paths
   Input paths separated by the systems path separator (';' on MS-Windows, otherwise ':').
   Files will be scanned, directories will be included recursively.

"--text"
   Input text, leave blank to generate cache for all N-GRAMS.
"--partial-text" (optional)

"--input-paths-size-limit"
   When scanning paths recursively, skip files exceeding this size.

"--update" (optional, defaults to "check-manifest")
   Method used to check for updates.
   Valid values include: ("check-manifest", "when-missing").

Output text is printed.
"""

import sys
import os
import re

# To store an `input_path` directory as a file-name.
from urllib.parse import quote


from typing import (
    Callable,
    Dict,
    Generator,
    List,
    Optional,
    Sequence,
    Tuple,
)

ModelType = Dict[str, Dict[str, int]]
ManifestType = Tuple[List[str], List[float]]

# Gets overwritten by command line argument.
CACHE_DIRECTORY = ""

# Ignore files over this size (zero to ignore).
TEXT_EXTRACT_SIZE_LIMIT = 0

MULTI_PROCESS = True

# Avoid over-large models, split into parts.
SPLIT_BUCKET_COUNT = 8192

# Generate N-Grams with this many words or more,
# WARNING: each level adds significant overhead,
# 4 may be useful but values such as 10 or more are likely to explode storage...
# although it may be interesting to test.
NGRAM_MAX = 4

FILE_TYPE_SOURCE = 0
FILE_TYPE_TEXT = 1


# ------------------------------------------------------------------------------
# Generic Utilities


# Include all files recursively.
def files_recursive_with_ext(path: str, ext: Optional[Tuple[str, ...]]) -> Generator[Tuple[str, str], None, None]:
    for dirpath, dirnames, filenames in os.walk(path):
        # Skip `.git` and other dot-files.
        dirnames[:] = [d for d in dirnames if not d.startswith(".")]
        for filename in filenames:
            if filename.startswith("."):
                continue
            if not ext or filename.endswith(ext):
                yield (dirpath, filename)


_clear_words_trans = {
    ord("-"): None,
    ord("'"): None,
}


def words_from_file_with_fancy_lexer_support(filepath: str, file_type: int) -> List[List[str]]:
    """
    Fill ``bag_of_words`` with comments from ``filepath``.
    """

    try:
        with open(filepath, encoding="utf-8") as fh:
            data = fh.read()
    except:
        # Possibly a missing symbolic link, skip the file.
        # print("Unable to read")
        return []

    if not data:
        return []

    def clean_words(words: List[str]) -> List[List[str]]:
        words_list: List[List[str]] = []
        this_word_list: List[str] = []
        last_split = False
        for i, w in enumerate(words):
            if (
                # Allow literal digits because they may be used in text,
                # in particular 1 or 2.
                (not w.isdigit()) and
                (not w.isalpha()) and
                (not w.translate(_clear_words_trans).isalpha())
            ):
                if not words_list:
                    words_list.append(words[0:i])
                last_split = True
                continue

            if last_split:
                this_word_list = []
                words_list.append(this_word_list)

            if words_list:
                this_word_list.append(w)
            last_split = False

        if not words_list:
            words_list.append(words)

        # Ensure word lists have at least 3 words, fewer are not useful for prediction.
        for i in reversed(range(len(words_list))):
            if len(words_list[i]) < 3:
                del words_list[i]
        return words_list

    print(filepath)

    re_word = re.compile(r"[\w]+[\w'-]*")
    re_split = re.compile("[\\.?!;()[]{}]")

    # Don't parse some files as plain-text.
    if file_type == FILE_TYPE_TEXT:
        words_list = []
        for block in re.split(re_split, data):
            if not block:
                continue
            words_list.extend(clean_words(re.findall(re_word, block)))
        return words_list

    # Fancy parsing using `pygments`.
    import pygments
    from pygments.lexers import guess_lexer_for_filename
    try:
        lexer = guess_lexer_for_filename(filepath, data)
    except pygments.util.ClassNotFound:
        return []

    from pygments.token import Token
    extract_tokens = {
        Token.Comment,
        Token.Comment.Single,
        Token.Comment.Multiline,
    }

    bag_of_words: List[List[str]] = []
    is_first = True
    assert hasattr(lexer, "get_tokens")
    for ty, token_text in lexer.get_tokens(data):
        # Skip the first comment as it's very often a license block.
        if is_first:
            is_first = False
            if ty in extract_tokens:
                continue
        if ty in extract_tokens:
            for block in re.split(re_split, token_text):
                if not block:
                    continue
                bag_of_words.extend(clean_words(re.findall(re_word, block)))

    return bag_of_words


def words_from_files(files_and_types: List[Tuple[str, int]]) -> List[List[str]]:
    bag_of_words = []
    if MULTI_PROCESS:
        import multiprocessing
        job_total = multiprocessing.cpu_count()
        # As some files.
        job_total = job_total + (job_total // 2)

        with multiprocessing.Pool(processes=job_total) as pool:
            for bag_of_words_for_file in pool.starmap(words_from_file_with_fancy_lexer_support, files_and_types):
                bag_of_words.extend(bag_of_words_for_file)
    else:
        for filepath, ty in files_and_types:
            bag_of_words.extend(words_from_file_with_fancy_lexer_support(filepath, ty))
    return bag_of_words


# ------------------------------------------------------------------------------
# Top Level Model Utilities

from hashlib import sha1


def model_word_to_index(word_key: str) -> int:
    m = sha1()
    m.update(word_key.encode())
    return int.from_bytes(m.digest(), 'little') % SPLIT_BUCKET_COUNT


def directory_from_params(input_path: str) -> str:
    return os.path.join(CACHE_DIRECTORY, quote(input_path.lstrip(os.sep), safe=''))


def model_filpath_from_params(n: int, split_index: int, input_path: str) -> str:
    directory = directory_from_params(input_path)
    return os.path.join(directory, "{:d}_{:04d}.pickle".format(n, split_index))


def model_generate_all(n: int, bag_of_words: List[List[str]]) -> List[ModelType]:
    """
    Return a list of models.
    """
    if n < 2:
        raise Exception("`n` must be 2 or more.")

    model_list: List[ModelType] = [{} for _ in range(SPLIT_BUCKET_COUNT)]
    for words in bag_of_words:
        for i in range(n, len(words)):
            word_key = " ".join(words[j].lower() for j in range(i - n, i))
            word_val = words[i]

            model = model_list[model_word_to_index(word_key)]

            # Avoid `defaultdict` because this should be pickled and reused with high efficiency.
            try:
                d = model[word_key]
            except:
                d = model[word_key] = {}
            try:
                d[word_val] += 1
            except:
                d[word_val] = 1
    return model_list


def model_file_read(filepath: str) -> ModelType:
    import pickle
    import lzma
    with lzma.open(filepath, 'rb') as fh:
        model: ModelType = pickle.load(fh)
    return model


def model_file_write(filepath: str, model: ModelType) -> None:
    import pickle
    import lzma
    with lzma.open(filepath, 'wb') as fh:
        pickle.dump(model, fh)


def model_ensure_for_split_index(
        n: int,
        split_index: int,
        input_path: str,
        word_generate_on_demand_fn: Callable[[], List[List[str]]],
) -> ModelType:
    filepath = model_filpath_from_params(n, split_index, input_path)
    if os.path.exists(filepath):
        return model_file_read(filepath)

    # Always generate all models.
    bag_of_words = word_generate_on_demand_fn()
    model_list = model_generate_all(n, bag_of_words)
    for i, model in enumerate(model_list):
        model_file_write(model_filpath_from_params(n, i, input_path), model)

    return model_list[i]


def model_files_generate(
        input_path: str,
        input_paths_match_source_re: re.Pattern[str],
        input_paths_match_text_re: re.Pattern[str],
) -> List[Tuple[str, int]]:

    files_and_types = []
    is_file = not os.path.isdir(input_path)

    for dirpath, f in (
            (os.path.split(input_path),) if is_file else
            files_recursive_with_ext(input_path, None)
    ):
        if input_paths_match_source_re.match(f):
            ty = FILE_TYPE_SOURCE
        elif input_paths_match_text_re.match(f):
            ty = FILE_TYPE_TEXT
        elif is_file:
            ty = FILE_TYPE_TEXT
        else:
            continue

        f = os.path.join(dirpath, f)

        # Don't apply limits to individual files passed in
        # because the user explicitly asked for them.
        if not is_file and TEXT_EXTRACT_SIZE_LIMIT > 0:
            try:
                size = os.path.getsize(f)
            except Exception as ex:
                # Ignore files that can't be read.
                print("Unable to access the size of:", f, str(ex))
                continue

            if size >= TEXT_EXTRACT_SIZE_LIMIT:
                continue

        files_and_types.append((f, ty))

    # Quickly test a subset.
    # files_and_types[64:] = []

    # Sort for trivial comparison.
    files_and_types.sort()
    return files_and_types


def manifest_file_read(filepath: str) -> ManifestType:
    import pickle
    with open(filepath, 'rb') as fh:
        manifest: ManifestType = pickle.load(fh)
    return manifest


def manifest_file_write(filepath: str, manifest: ManifestType) -> None:
    import pickle
    with open(filepath, 'wb') as fh:
        pickle.dump(manifest, fh)


def model_ensure_up_to_date_or_clear(
        input_path: str, files_and_types: List[Tuple[str, int]], update_method: str) -> None:
    directory = directory_from_params(input_path)

    # Quick check, for simple case.
    if update_method == "when-missing":
        if os.path.isdir(directory):
            return

    manifest_path = os.path.join(directory, "manifest.pickle")

    do_clear = False
    if not os.path.exists(manifest_path):
        do_clear = True
    else:
        manifest = manifest_file_read(manifest_path)
        files = [ft[0] for ft in files_and_types]
        if manifest[0] != files:
            do_clear = True
        else:
            for filepath, mtime in zip(manifest[0], manifest[1]):
                if mtime != os.path.getmtime(filepath):
                    do_clear = True
                    break

    if do_clear:
        import shutil
        shutil.rmtree(directory)
        os.makedirs(directory)


def manifest_write_from_files(input_path: str, files_and_types: List[Tuple[str, int]]) -> None:
    files = [ft[0] for ft in files_and_types]
    mtime = [0.0] * len(files)
    for i, filepath in enumerate(files):
        mtime[i] = os.path.getmtime(filepath)

    manifest = files, mtime

    directory = directory_from_params(input_path)
    manifest_path = os.path.join(directory, "manifest.pickle")

    manifest_file_write(manifest_path, manifest)


def complete_word_with_root(
        input_path: str,
        words: List[str],
        partial_word: str,
        input_paths_match_source: List[str],
        input_paths_match_text: List[str],
        update_method: str,
        generate_all: bool,
) -> bool:

    bag_of_words = None
    files_and_types = None

    def files_generage_on_demand() -> List[Tuple[str, int]]:
        import fnmatch
        nonlocal files_and_types

        if files_and_types is not None:
            return files_and_types

        input_paths_match_source_value = "({:s})".format("|".join([
            fnmatch.translate(exclude_glob)
            for exclude_glob in input_paths_match_source
        ]))
        input_paths_match_text_value = "({:s})".format("|".join([
            fnmatch.translate(exclude_glob)
            for exclude_glob in input_paths_match_text
        ]))

        try:
            input_paths_match_source_re = re.compile(input_paths_match_source_value)
        except Exception as ex:
            print(ex)
            sys.exit(0)
        try:
            input_paths_match_text_re = re.compile(input_paths_match_text_value)
        except Exception as ex:
            print(ex)
            sys.exit(0)

        files_and_types = model_files_generate(input_path, input_paths_match_source_re, input_paths_match_text_re)

        return files_and_types

    def word_generator_on_demand() -> List[List[str]]:
        nonlocal bag_of_words
        if bag_of_words is not None:
            return bag_of_words

        files_and_types = files_generage_on_demand()

        bag_of_words = words_from_files(files_and_types)
        manifest_write_from_files(input_path, files_and_types)

        return bag_of_words

    # The size of the N-grams (2 or more).
    if generate_all:
        files_and_types = files_generage_on_demand()
        model_ensure_up_to_date_or_clear(input_path, files_and_types, update_method)

        n_total = NGRAM_MAX
        for n in reversed(range(2, n_total + 1)):
            # Requesting the first split-index ensures all are properly generated.
            model_ensure_for_split_index(n, 0, input_path, word_generator_on_demand)

        return False
    else:
        n_total = min(NGRAM_MAX, len(words))

    # If extending the partial word fails, check
    for pass_number in range(2 if partial_word else 1):
        # Nothing found in the first pass, run a second pass.
        add_space = False
        if pass_number == 1:
            words.append(partial_word)
            partial_word = ""
            add_space = True

        for n in reversed(range(2, n_total + 1)):
            word_key = " ".join(tuple(words[-n:]))
            split_index = model_word_to_index(word_key)
            model = model_ensure_for_split_index(n, split_index, input_path, word_generator_on_demand)

            # Only generating, not looking for a result.
            if not words:
                continue

            v_best = -1
            k_best = ""
            for k, v in model.get(word_key, {}).items():

                if partial_word:
                    k_lower = k.lower()
                    if not k_lower.startswith(partial_word):
                        continue
                    # Unlikely but possible.
                    # Skip this as it effectively completes to an empty string.
                    # Even if that result has the highest probability, it's not useful to propose nothing.
                    if k_lower == partial_word:
                        continue

                if v_best < v:
                    v_best = v
                    k_best = k
                elif v_best == v:
                    # So the order is not random.
                    if k < k_best:
                        k_best = k
            if k_best:
                if partial_word:
                    k_best = k_best[len(partial_word):]
                elif add_space:
                    # Adding a second word.
                    k_best = " " + k_best
                sys.stdout.write(k_best)
                return True
    return False


# ------------------------------------------------------------------------------
# Main Function Main

def main() -> None:
    global CACHE_DIRECTORY
    global TEXT_EXTRACT_SIZE_LIMIT

    args = sys.argv[1:]

    def extract_arg_value_pair(
        arg_id: str,
        *,
        default: Optional[str] = None,
        valid_values: Optional[Sequence[str]] = None,
    ) -> str:
        try:
            i = args.index(arg_id)
        except ValueError:
            if default is not None:
                return default
            sys.stderr.write('Argument "{:s}" missing, abort!\n'.format(arg_id))
            sys.exit(1)

        if len(args) == i + 1:
            sys.stderr.write('Argument "{:s}" missing value, abort!\n'.format(arg_id))
            sys.exit(1)
        value = args[i + 1]
        if (valid_values is not None) and (value not in valid_values):
            sys.stderr.write(
                'Argument "{:s}" has unexpected value "{:s}", not found in: {:s}, abort!\n'.format(
                    arg_id, value, ", ".join(['"' + word + '"' for word in valid_values])
                )
            )
            sys.exit(1)

        del args[i: i + 2]
        return value

    CACHE_DIRECTORY = extract_arg_value_pair("--cache")
    TEXT_EXTRACT_SIZE_LIMIT = int(extract_arg_value_pair("--input-paths-size-limit"))

    input_paths = extract_arg_value_pair("--input-paths").split(os.pathsep)
    input_paths_match_source = extract_arg_value_pair("--input-paths-match-source").split(os.pathsep)
    input_paths_match_text = extract_arg_value_pair("--input-paths-match-text").split(os.pathsep)
    words = extract_arg_value_pair("--text").lower().split()
    partial_word = extract_arg_value_pair("--partial-text", default="").lower()
    update_method = extract_arg_value_pair(
        "--update",
        default="check-manifest",
        valid_values=("check-manifest", "when-missing"),
    )

    if args:
        sys.stderr.write("Found unknown arguments: {!r}, abort!")

    if not input_paths:
        return

    # Blank is for cache generation (only).
    if words and len(words) < (1 if partial_word else 2):
        sys.stderr.write("'--text' must contain at least two words!\n")
        return

    generate_all = not bool(words)
    for input_path in input_paths:

        # Prevent trailing slash impacting the name of temporary paths.
        input_path = input_path.rstrip(os.sep)
        directory = directory_from_params(input_path)
        os.makedirs(directory, exist_ok=True)

        if complete_word_with_root(
                input_path,
                words,
                partial_word,
                input_paths_match_source,
                input_paths_match_text,
                update_method,
                generate_all,
        ):
            return


if __name__ == "__main__":
    main()

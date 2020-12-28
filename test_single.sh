#!/bin/bash
set -euo pipefail

# Exit 0 when correct
# Exit 1 when wrong answer
# Exit 2 when unexpected things happened

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <input.c>"
    exit 2
fi

ROOT=$(realpath $(dirname "$0"))
INPUT_C=$(realpath "$1")

if [[ ! -f "$INPUT_C" ]]; then
    echo "'$INPUT_C' is not a file"
    exit 2
fi

PARSER_EXE="${ROOT}/src/parser"
MAIN_S="${ROOT}/run_and_main/main.S"
HEADER_H="${ROOT}/run_and_main/header.h"
HELPER_C="${ROOT}/run_and_main/helper.c"

EXEC_TIMEOUT=1

GCC="riscv64-unknown-linux-gnu-gcc"
QEMU="qemu-riscv64"

WORKING_DIR="${ROOT}/test_zone"

exit_with_msg() {
    # $1 = msg, $2 = exit code
    echo "$1"
    exit $2
}

make -C src -j > /dev/null 2>&1 || exit_with_msg "Failed to make parser" 2

if [[ -d "$WORKING_DIR" ]] && ! rm -rf "$WORKING_DIR"; then
    echo "Cannot clean up '$WORKING_DIR'"
    exit 2
fi

mkdir -p "${WORKING_DIR}"

cd ${WORKING_DIR} # Change Directory

cp "$INPUT_C" input.c
cp "$MAIN_S" main.S
cp "$HEADER_H" header.h
cp "$HELPER_C" helper.c

echo '#include "header.h"' > input_gcc.c
cat input.c >> input_gcc.c


"$GCC" -O0 -static helper.c input_gcc.c -o gcc_exec > /dev/null 2>&1 || exit_with_msg "Cannot compile input c file with gcc" 2

EXIT_STATUS=0
{ timeout "$EXEC_TIMEOUT" "$QEMU" gcc_exec < /dev/null > gcc_output ; } > /dev/null 2>&1 || EXIT_STATUS=$?
if [[ $EXIT_STATUS -eq 124 ]]; then
    exit_with_msg "GCC's executable: execution timeout" 2
elif [[ $EXIT_STATUS -ne 0 ]]; then
    exit_with_msg "GCC's executable: runtime error" 2
fi

{ "$PARSER_EXE" input.c ; } > /dev/null 2>&1 || exit_with_msg "The parser exited abnormally" 1
"$GCC" -O0 -static main.S -o our_exec > /dev/null 2>&1 || exit_with_msg "Cannot compile main.S" 1

EXIT_STATUS=0
{ timeout "$EXEC_TIMEOUT" "$QEMU" our_exec < /dev/null > our_output ; } > /dev/null 2>&1 || EXIT_STATUS=$?
if [[ $EXIT_STATUS -eq 124 ]]; then
    exit_with_msg "Our executable: execution timeout" 1
elif [[ $EXIT_STATUS -ne 0 ]]; then
    exit_with_msg "Our executable: runtime error" 1
fi

diff our_output gcc_output > /dev/null 2>&1 || exit_with_msg "Output different!!!" 1

exit_with_msg "Correct." 0

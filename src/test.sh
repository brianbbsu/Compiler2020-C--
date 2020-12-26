#!/usr/bin/env bash

make clean
make -j $(nproc)

cp ../run_and_main/main.S .

for file in ../sample/hw5-pattern/*.c; do
    name=`basename $file .c`
    ./parser ../sample/hw5-pattern/${name}.c \
    && ./riscv64-unknown-linux-gnu-gcc main.S \
    && diff <(qemu-riscv64 a.out) ../sample/hw5-pattern/${name}.output
done

#!/usr/bin/env bash

# usage: fuzzytar.sh <tarfile>
OUT=_$1
cp $1 $OUT

# randomize all fields
i=0
length=(100 8 8 8 12 12 8 1 100 6 2 32 32 8 8 155)
for offset in 0 100 108 116 124 136 148 156 157 257 263 265 297 329 337 345; do
	dd if=/dev/urandom of=$OUT bs=1 count=${length[i]} seek=$offset conv=notrunc status=none
	i=$((i + 1))
done

# forge magic
printf '\x1F\x8B\x00' | dd of=$OUT bs=1 count=3 seek=0 conv=notrunc status=none

# recompute checksum
printf '        ' | dd of=$OUT bs=1 count=8 seek=148 conv=notrunc status=none
chksum=$(head -c 500 $OUT | sum -s)
printf "%06o\x00\x20" $chksum | dd of=$OUT bs=1 count=8 seek=148 conv=notrunc status=none

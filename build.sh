#!/bin/sh
./rgbasm -o main.o main.asm \
    && ./rgblink -O ffl2-orig.gb -n ffl2.sym -o ffl2.gb main.o \
    && ./rgbfix -v ffl2.gb
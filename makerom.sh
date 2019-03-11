#!/bin/bash

#set -e

# compile Z80 code
rm rom.bin
z80asm rom.asm -o rom.bin --list=rom.lst
#cat rom.lst

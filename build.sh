#!/bin/sh
set -e
python3 platform_config.py
gcc -c ./cli/tty_ifo.c
gcc -c *.c
gnatmake -j0 -I./cli/ -I./hex/ -I./unicode -I./modular_hashing aura.adb
rm platform_info.*
rm *.o *.ali


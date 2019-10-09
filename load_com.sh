#!/bin/sh
set -e

my_file="$1"

if [ -z "$my_file" ] ; then
    echo "needs file"
    exit 1
fi

hex_file=$(mktemp -t "$(basename "$my_file")")

echo "Writing to hex file: $hex_file"

python bin2hex.py -r 2 -b "0x100,$my_file" -o "$hex_file"

python echo_serial.py "$hex_file"








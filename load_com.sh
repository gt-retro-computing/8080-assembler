#!/bin/sh
set -e

my_file="$1"

if [ -z "$my_file" ] ; then
    echo "needs file"
    exit 1
fi

hex_file=$(mktemp)

echo "Writing to hex file: $hex_file"

python2 bin2hex.py -r 2 -b "0x100,$my_file" -o "$hex_file"

python2 echo_serial.py "$hex_file"








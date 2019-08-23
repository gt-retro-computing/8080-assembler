#!/bin/sh

hex_data=$(mktemp)

node combined.js "$@" | tee "$hex_data"
if [ ! $? -eq 0 ]; then
    echo 'Not echoing to serial'
    exit 1
fi

python2 echo_serial.py "$hex_data"



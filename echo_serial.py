from __future__ import print_function

import serial
import glob
import sys

hexes = []
with open(sys.argv[1]) as f:
    for line in f:
        if line.startswith(':'):
            hexes.append(line.strip())


if sys.platform == 'darwin':
    # mac_irl
    serial_ifs = glob.glob('/dev/cu.usbserial*')
else:
    serial_ifs = glob.glob('/dev/ttyUSB0')

if not serial_ifs:
    print('No serial interfaces found!')
    exit(1)

print('Using serial:', serial_ifs[0])

with serial.Serial(serial_ifs[0], 115200, timeout=0.1) as ser:
    print('Serial Name:', ser.name)
    for chunk in hexes:
        ser.write(chunk)
        ser.write('\r\n')
        print(ser.read(size=10000))




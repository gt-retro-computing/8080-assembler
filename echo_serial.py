
import serial
import glob
import time
import sys

hexes = []
with open(sys.argv[1]) as f:
    for line in f:
        if line.startswith(':'):
            hexes.append(line.strip())


# mac_irl
serial_ifs = glob.glob('/dev/cu.usbserial*')

if not serial_ifs:
    print 'No serial interfaces found!'
    exit(1)

print 'Using serial:', serial_ifs[0]

with serial.Serial(serial_ifs[0], 115200, timeout=0.05) as ser:
    for chunk in hexes:
        ser.write(chunk)
        ser.write('\r\n')
        print ser.read(size=300)




import base64

a = bytearray()
b = bytearray()

for i in range(40):
    a.append(0x00)
for i in range(6):
    a.append(0x0)

a.append(0xFC) # Index Mark

for i in range(26):
    a.append(0x00)

for i in range(26):
    for j in range(6):
        a.append(0)
    a.append(0xFE)
    # a.append(0)
    b.append(0)
    b.append(i+1)
    b.append(0x0)
    b.append(0xF7)
    for j in range(11):
        b.append(0x00)
    for j in range(6):
        b.append(0)
    b.append(0xFB)
    for j in range(128):
        b.append(0x69)
    b.append(0xF7)
    for i in range(27):
        b.append(0x00)
for i in range(300):
    b.append(0x00)

print(base64.standard_b64encode(a))
print(base64.standard_b64encode(b))

print(len(a))
c = a
a.append(0)
a += b
print(base64.standard_b64encode(c))

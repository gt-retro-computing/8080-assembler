global_label = 1


def next_label(prefix):
    global global_label

    label = global_label
    global_label += 1
    lbl = ''
    while label != 0:
        lbl += 'abcdefghijklmnopqrstuvwxyz'[label % 26]
        label = label // 26

    return prefix + '_' + lbl


class Loop:
    def __init__(self, var, start, end, dbg=''):
        self.var = var
        self.start = start
        self.end = end
        self.dbg = dbg
        self.loop_label = None

    def __enter__(self):
        self.loop_label = next_label('loop')
        print('\tmvi {}, {}'.format(self.var, self.start))
        print('{}_begin:'.format(self.loop_label))

        print('\tmvi a, {}'.format(self.end))
        print('\tcmp {}'.format(self.var))
        print('\tjz {}_end'.format(self.loop_label))

    def __exit__(self, exc_type, exc_val, exc_tb):
        print('\tinr {}'.format(self.var))
        print('\tjmp {}_begin'.format(self.loop_label))
        print('{}_end:'.format(self.loop_label))


def out(data):
    print('\tin DWAIT')
    if type(data) == str:
        print('\tmov a, {}'.format(data))
    else:
        print('\tmvi a, {}'.format(data))
    print('\tout DDATA')


print('''
DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah
''')

print('\tmvi a, 0xf4')
print('\tout DCOM')

with Loop('b', 0, 46):
    out(0x00)

out(0xfc)

with Loop('b', 0, 26):
    out(0x00)

with Loop('b', 1, 27):
    with Loop('c', 0, 6):
        out(0x00)

    out(0xfe)
    out(0)  # track number

    out(0)
    out('b')
    out(0)
    out(0xF7)

    with Loop('c', 0, 17):
        out(0x00)

    out(0xFB)

    with Loop('c', 0, 128):
        out(0xE5)

    out(0xF7)

    with Loop('c', 0, 27):
        out(0x00)

print('end_lbl:')
print('\tin DWAIT')
print('\tora a')
print('\tjp end')
print('\tmvi a, 0')
print('\tout DDATA')
print('\tjmp end_lbl')

print('end:')
print('\thlt')

with Loop('b', 0, 10):
    print('foo')


import sys
print(sys.platform)
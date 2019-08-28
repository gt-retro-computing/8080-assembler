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
        """
        This loop construct always clobbers the 'a' register during comparison.

        :param var: which register to use for the loop counter
        :param start: start value
        :param end: end value (exclusive)
        :param dbg:
        """
        self.var = var
        self.start = start
        self.end = end
        self.dbg = dbg
        self.loop_label = None

    def __enter__(self):
        self.loop_label = next_label('loop')
        print('\tmvi {}, {}'.format(self.var, self.start))
        if self.dbg:
            print('{}_begin: ; {} <= {} < {} - {}'.format(self.loop_label, self.start, self.var, self.end, self.dbg))
        else:
            print('{}_begin: ; {} <= {} < {}'.format(self.loop_label, self.start, self.var, self.end))

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


with Loop('d', 0, 20, 'Track Counter'):
    print('\tmvi a, 0xf4')
    print('\tout DCOM')

    with Loop('b', 0, 46):
        out(0x00)

    out(0xfc)

    with Loop('b', 0, 26):
        out(0x00)

    with Loop('b', 1, 27, 'Segment Counter'):
        with Loop('c', 0, 6):
            out(0x00)

        out(0xfe)
        out('d')  # track number

        out(0)
        out('b')  # segment number
        out(0)
        out(0xF7)

        with Loop('c', 0, 17):
            out(0x00)

        out(0xFB)

        # Init Sector Data, 0xE5 is IBM standard.
        with Loop('c', 0, 128):
            out(0xE5)

        out(0xF7)

        with Loop('c', 0, 27):
            out(0x00)

    # --- Send 0x00 until floppy controller says to stop ---
    end_seq_begin_label = next_label('end_seq') + '_begin'
    end_seq_end_label = next_label('end_seq') + '_end'
    print('{}:'.format(end_seq_begin_label))
    print('\tin DWAIT')
    print('\tora a')
    print('\tjp {}'.format(end_seq_end_label))
    print('\tmvi a, 0')
    print('\tout DDATA')
    print('\tjmp {}'.format(end_seq_begin_label))
    print('{}:'.format(end_seq_end_label))
    # ---

print('\thlt')

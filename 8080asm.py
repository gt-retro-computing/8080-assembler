#!/usr/bin/env python

import struct

def preprocess_table(table):
	new_table = {}
	for k,v in table.iteritems():
		k = filter(bool, k.replace(',', ' ').split(' '))
		if k[0] not in new_table:
			new_table[k[0]] = {}
		new_table[k[0]][tuple(k[1:])] = v
	return new_table

keywords = ['db', 'dw', 'dd', 'dq']

instructionTable = preprocess_table({
    'nop': 0,
    'stax b': 2,
    'inx b': 3,
    'inr b': 4,
    'dcr b': 5,
    'rlc': 7,
    'dad b': 9,
    'ldax b': 10,
    'dcx b': 11,
    'inr c': 12,
    'dcr c': 13,
    'rrc': 15,
    'stax d': 18,
    'inx d': 19,
    'inr d': 20,
    'dcr d': 21,
    'ral': 23,
    'dad d': 25,
    'ldax d': 26,
    'dcx d': 27,
    'inr e': 28,
    'dcr e': 29,
    'rar': 31,
    'rim': 32,
    'inx h': 35,
    'inr h': 36,
    'dcr h': 37,
    'daa': 39,
    'dad h': 41,
    'dcx h': 43,
    'inr l': 44,
    'dcr l': 45,
    'cma': 47,
    'sim': 48,
    'inx sp': 51,
    'inr m': 52,
    'dcr m': 53,
    'stc': 55,
    'dad sp': 57,
    'dcx sp': 59,
    'inr a': 60,
    'dcr a': 61,
    'push b': 197,
    'push be': 197,
    'rst 0': 199,
    'rz': 200,
    'ret': 201,
    'rst 1': 207,
    'rnc': 208,
    'pop d': 209,
    'pop de': 209,
    'push d': 213,
    'push de': 213,
    'rst 2': 215,
    'rc': 216,
    'rst 3': 223,
    'rpo': 224,
    'pop h': 225,
    'pop hl': 225,
    'xthl': 227,
    'push h': 229,
    'push hl': 229,
    'rst 4': 231,
    'rpe': 232,
    'pchl': 233,
    'xchg': 235,
    'rst 5': 239,
    'rp': 240,
    'pop psw': 241,
    'di': 243,
    'push psw': 245,
    'rst 6': 247,
    'rm': 248,
    'sphl': 249,
    'ei': 251,
    'rst 7': 255,
    'cmc': 63,
    'mov b,b': 64,
    'mov b,c': 65,
    'mov b,d': 66,
    'mov b,e': 67,
    'mov b,h': 68,
    'mov b,l': 69,
    'mov b,m': 70,
    'mov b,a': 71,
    'mov c,b': 72,
    'mov c,c': 73,
    'mov c,d': 74,
    'mov c,e': 75,
    'mov c,h': 76,
    'mov c,l': 77,
    'mov c,m': 78,
    'mov c,a': 79,
    'mov d,b': 80,
    'mov d,c': 81,
    'mov d,d': 82,
    'mov d,e': 83,
    'mov d,h': 84,
    'mov d,l': 85,
    'mov d,m': 86,
    'mov d,a': 87,
    'mov e,b': 88,
    'mov e,c': 89,
    'mov e,d': 90,
    'mov e,e': 91,
    'mov e,h': 92,
    'mov e,l': 93,
    'mov e,m': 94,
    'mov e,a': 95,
    'mov h,b': 96,
    'mov h,c': 97,
    'mov h,d': 98,
    'mov h,e': 99,
    'mov h,h': 100,
    'mov h,l': 101,
    'mov h,m': 102,
    'mov h,a': 103,
    'mov l,b': 104,
    'mov l,c': 105,
    'mov l,d': 106,
    'mov l,e': 107,
    'mov l,h': 108,
    'mov l,l': 109,
    'mov l,m': 110,
    'mov l,a': 111,
    'mov m,b': 112,
    'mov m,c': 113,
    'mov m,d': 114,
    'mov m,e': 115,
    'mov m,h': 116,
    'mov m,l': 117,
    'hlt': 118,
    'mov m,a': 119,
    'mov a,b': 120,
    'mov a,c': 121,
    'mov a,d': 122,
    'mov a,e': 123,
    'mov a,h': 124,
    'mov a,l': 125,
    'mov a,m': 126,
    'mov a,a': 127,
    'add b': 128,
    'add c': 129,
    'add d': 130,
    'add e': 131,
    'add h': 132,
    'add l': 133,
    'add m': 134,
    'add a': 135,
    'adc b': 136,
    'adc c': 137,
    'adc d': 138,
    'adc e': 139,
    'adc h': 140,
    'adc l': 141,
    'adc m': 142,
    'adc a': 143,
    'sub b': 144,
    'sub c': 145,
    'sub d': 146,
    'sub e': 147,
    'sub h': 148,
    'sub l': 149,
    'sub m': 150,
    'sub a': 151,
    'sbb b': 152,
    'sbb c': 153,
    'sbb d': 154,
    'sbb e': 155,
    'sbb h': 156,
    'sbb l': 157,
    'sbb m': 158,
    'sbb a': 159,
    'ana b': 160,
    'ana c': 161,
    'ana d': 162,
    'ana e': 163,
    'ana h': 164,
    'ana l': 165,
    'ana m': 166,
    'ana a': 167,
    'xra b': 168,
    'xra c': 169,
    'xra d': 170,
    'xra e': 171,
    'xra h': 172,
    'xra l': 173,
    'xra m': 174,
    'xra a': 175,
    'ora b': 176,
    'ora c': 177,
    'ora d': 178,
    'ora e': 179,
    'ora h': 180,
    'ora l': 181,
    'ora m': 182,
    'ora a': 183,
    'cmp b': 184,
    'cmp c': 185,
    'cmp d': 186,
    'cmp e': 187,
    'cmp h': 188,
    'cmp l': 189,
    'cmp m': 190,
    'cmp a': 191,
    'rnz': 192,
    'pop b': 193,
    'pop be': 193,
})

# Instruction table dictionary that expects a secondary parameter (8-bit)
varInstructionTable_EigthBit = preprocess_table({
    'mvi b,': 6,
    'mvi c,': 14,
    'mvi d,': 22,
    'mvi e,': 30,
    'mvi h,': 38,
    'mvi l,': 46,
    'sta': 50,
    'mvi m,': 54,
    'lda': 58,
    'mvi a,': 62,
    'adi': 198,
    'aci': 206,
    'out': 211,
    'sui': 214,
    'in': 219,
    'sbi': 222,
    'ani': 230,
    'xri': 238,
    'ori': 246,
    'cpi': 254
})
# Instruction table dictionary that expects a secondary parameter (16-bit)
varInstructionTable_SixteenBit = preprocess_table({
    'lxi b,': 1,
    'lix bc,': 1,
    'lxi d,': 17,
    'lxi de,': 17,
    'lxi h,': 33,
    'lxi hl,': 33,
    'shld': 34,
    'lhld': 42,
    'lxi sp,': 49,
    'jnz': 194,
    'jmp': 195,
    'cnz': 196,
    'jz': 202,
    'cz': 204,
    'call': 205,
    'jnc': 210,
    'cnc': 212,
    'cc': 220,
    'jc': 218,
    'jpo': 226,
    'cpo': 228,
    'jpe': 234,
    'cpe': 236,
    'jp': 242,
    'cp': 244,
    'jm': 250,
    'cm': 252
})

class Token(object):
	TYPE_INSN = 0
	TYPE_OPERAND = 1
	TYPE_LABEL = 2
	TYPE_DIRECTIVE = 3
	TYPE_STMT_DELIM = 4 # delimiter between statements
	def __init__(self, text, tok_type, source_ptr):
		self.text = text
		self.tok_type = tok_type
		self.source_ptr = source_ptr

	def __repr__(self):
		return '%s<%s>'% (self.text, self.tok_type)

class ASTNode(object):
	TYPE_ROOT = 0
	TYPE_INSN = 1
	TYPE_LABEL = 2
	TYPE_DIRECTIVE = 3
	def __init__(self, token, node_type):
		self.children = []
		self.token = token
		self.node_type = node_type

	def add_child(self, child):
		self.children.append(child)

	def __repr__(self):
		return '[%s]<%d>(%s)' % (self.token, self.node_type, self.children)

class Lexer(object):
	def __init__(self):
		self.ptr = 0
		self.ptr_tok_start = 0
		self.tokens = []
		self.aliases = {'dec': 'dcr'}
		self.token = ''

	def add_token(self, tok):
		if tok.text in self.aliases:
			tok.text = self.aliases[tok.text]
		# print 'add token ' + repr(tok)
		self.tokens.append(tok)

	def add_char(self, c):
		if not self.token:
			self.ptr_tok_start = self.ptr
		self.token += c

	def lex(self, asm_in):
		delimiters = ' \n\t,;'
		reserved_names = ['a','b','c','d','e','f','g','h','i','l','m','bc','de','hl','sp']
		STATE_INITIAL, STATE_COMMENT, STATE_DIRECTIVE, STATE_OPERAND, STATE_LABEL, STATE_STRING = 0, 1, 2, 3, 4, 5
		appending_states = [STATE_INITIAL, STATE_DIRECTIVE, STATE_OPERAND, STATE_LABEL]

		# HACK; eof is a delimiter
		if not asm_in or asm_in[-1] != '\n':
			asm_in += '\n'

		self.state = STATE_INITIAL
		while self.ptr < len(asm_in):
			cur_char = asm_in[self.ptr]
			# print '"', cur_char, '"', self.state
			if self.state == STATE_INITIAL:
				if cur_char == '.':
					if self.token:
						raise SyntaxError('invalid directive at %d\n' % (self.ptr,))
					self.state = STATE_DIRECTIVE
					continue
				
				if cur_char == ':':
					self.state = STATE_LABEL
					continue
				
				if cur_char == ';':
					self.state = STATE_COMMENT
					continue
				
				if cur_char in delimiters and self.token:
					self.token = self.token.lower()
					if self.token not in instructionTable and self.token not in varInstructionTable_SixteenBit and self.token not in varInstructionTable_EigthBit and self.token not in keywords:
						raise SyntaxError('invalid instruction %s at %d' % (self.token, self.ptr_tok_start))
					self.state = STATE_OPERAND
					self.add_token(Token(self.token, Token.TYPE_INSN, self.ptr_tok_start))
					self.token = ''
					continue

				if cur_char == '\n':
					self.add_token(Token('\n', Token.TYPE_STMT_DELIM, self.ptr))
					self.ptr += 1
					self.state = STATE_INITIAL
					continue

				if cur_char in delimiters:
					self.ptr += 1
					continue

				self.add_char(cur_char)
				self.ptr += 1
				continue

			if self.state == STATE_COMMENT:
				if cur_char == '\n':
					self.add_token(Token('\n', Token.TYPE_STMT_DELIM, self.ptr))
					self.ptr += 1
					self.state = STATE_INITIAL
					continue

				self.ptr += 1
				continue

			if self.state == STATE_LABEL:
				assert cur_char == ':'
				if not self.token:
					raise SyntaxError('invalid label name at %d\n' % (self.ptr,))
				self.state = STATE_INITIAL
				self.token = self.token.lower()
				if self.token in reserved_names:
					raise SyntaxError('label %s at %d is a reserved name' % (self.token, self.ptr_tok_start))
				self.add_token(Token(self.token, Token.TYPE_LABEL, self.ptr_tok_start))
				self.token = ''
				self.ptr += 1
				continue

			if self.state == STATE_OPERAND:
				if cur_char == "'":
					if self.token:
						raise SyntaxError('invalid operand at %d\n' % (self.ptr,))
					self.ptr += 1
					self.state = STATE_STRING
					continue
				
				if cur_char == ';':
					self.state = STATE_COMMENT
					continue
				
				if cur_char in delimiters and self.token:
					self.state = STATE_OPERAND
					self.token = self.token.lower()
					self.add_token(Token(self.token, Token.TYPE_OPERAND, self.ptr_tok_start))
					self.token = ''
					continue

				if cur_char == '\n':
					self.add_token(Token('\n', Token.TYPE_STMT_DELIM, self.ptr))
					self.ptr += 1
					self.state = STATE_INITIAL
					continue

				if cur_char in delimiters:
					self.ptr += 1
					continue

				self.add_char(cur_char)
				self.ptr += 1
				continue

			if self.state == STATE_DIRECTIVE:
				if cur_char == ';':
					self.state = STATE_COMMENT
					continue

				if cur_char in delimiters and self.token:
					if len(self.token) < 2:
						raise SyntaxError('invalid directive at %d' % (self.ptr_tok_start,))
					self.state = STATE_OPERAND
					self.token = self.token[1:].lower()
					self.add_token(Token(self.token, Token.TYPE_DIRECTIVE, self.ptr_tok_start))
					self.token = ''
					continue

				if cur_char == '\n':
					self.add_token(Token('\n', Token.TYPE_STMT_DELIM, self.ptr))
					self.ptr += 1
					self.state = STATE_INITIAL
					continue

				if cur_char in delimiters:
					self.ptr += 1
					continue

				self.add_char(cur_char)
				self.ptr += 1
				continue

			if self.state == STATE_STRING:
				if cur_char == "'":
					self.state = STATE_OPERAND
					self.ptr += 1
					continue
				else:
					self.add_char(cur_char)
					self.ptr += 1
					continue

		return self.tokens

def parse_constant(c):
	if not c:
		raise SyntaxError('empty constant')
	base = 10
	if c[-1] == 'h':
		if len(c) == 1:
			raise SyntaxError('invalid constant')
		base = 16
		c = c[:-1]
	if len(c) > 2 and c.startswith('0x'):
		base = 16
		c = c[2:]
	if len(c) > 2 and c.startswith('0b'):
		base = 2
		c = c[2:]
	try:
		return int(c, base)
	except ValueError:
		raise SyntaxError('invalid constant ' + c)

def parse(token_list):
	ast = ASTNode(None, ASTNode.TYPE_ROOT)
	cur_node = None
	for token in tokens:
		if token.tok_type == Token.TYPE_DIRECTIVE:
			if cur_node:
				ast.add_child(cur_node)
			cur_node = ASTNode(token, ASTNode.TYPE_DIRECTIVE)
		elif token.tok_type == Token.TYPE_LABEL:
			if cur_node:
				ast.add_child(cur_node)
			ast.add_child(ASTNode(token, ASTNode.TYPE_LABEL))
			cur_node = None
		elif token.tok_type == Token.TYPE_INSN:
			if cur_node:
				ast.add_child(cur_node)
			cur_node = ASTNode(token, ASTNode.TYPE_INSN)
		elif token.tok_type == Token.TYPE_OPERAND:
			if not cur_node:
				raise SyntaxError('floating operand at %d' % (token.source_ptr))
			cur_node.add_child(token)
		elif token.tok_type == Token.TYPE_STMT_DELIM:
			if cur_node:
				ast.add_child(cur_node)
			cur_node = None
	return ast

class Codegen(object):
	def __init__(self, ast):
		self.binary = []
		self.mappings = {}
		self.labels = {}
		self.aliases = {}
		self.ptr = 0

	def write(self, b):
		if type(b) == str:
			for char in b:
				self.write(ord(char))
			return

		if len(self.binary) <= self.ptr:
			self.binary.extend([0] * (self.ptr - len(self.binary) + 1))
		self.binary[self.ptr] = b
		self.ptr += 1

	def parse_operand(self, imm_text):
		if imm_text in self.aliases:
			imm_text = self.aliases[imm_text]
		if imm_text in self.labels:
			return self.labels[imm_text]
		else:
			return parse_constant(imm_text)

	def generate(self):
		for astnode in ast.children:
			if astnode.node_type == ASTNode.TYPE_LABEL:
				labelname = astnode.token.text
				# print '%s at %d' % (labelname, self.ptr)
				self.labels[labelname] = self.ptr
		for astnode in ast.children:
			if astnode.node_type == ASTNode.TYPE_DIRECTIVE:
				dirname = astnode.token.text
				if dirname == 'org':
					if len(astnode.children) != 1:
						raise SyntaxError('wrong argument count for directive %s at %d' % (dirname, astnode.token.source_ptr))
					org_val = parse_constant(astnode.children[0].text)
					# print 'org %d' % (org_val,)
					self.ptr = org_val
				if dirname == 'equ':
					if len(astnode.children) != 2:
						raise SyntaxError('wrong argument count for directive %s at %d' % (dirname, astnode.token.source_ptr))
					self.aliases[astnode.children[0].text] = astnode.children[1].text
				if dirname == 'ascii':
					if len(astnode.children) != 1:
						raise SyntaxError('wrong argument count for directive %s at %d' % (dirname, astnode.token.source_ptr))
					text = astnode.children[0].text
					self.write(text)
			elif astnode.node_type == ASTNode.TYPE_INSN:
				opcode = astnode.token.text
				if opcode in keywords:
					if opcode == 'db' or opcode == 'dw' or opcode == 'dd' or opcode == 'dq':
						if len(astnode.children) != 1:
							raise SyntaxError('wrong argument count for %s at %d' % (opcode, astnode.token.source_ptr))
						imm = self.parse_operand(astnode.children[-1].text)
						if opcode == 'db':
							self.write(struct.pack('<B', imm))
						elif opcode == 'dw':
							self.write(struct.pack('<H', imm))
						elif opcode == 'dd':
							self.write(struct.pack('<I', imm))
						elif opcode == 'dq':
							self.write(struct.pack('<Q', imm))
				if opcode in instructionTable:
					operands = tuple(map(lambda tok: tok.text, astnode.children))
					if not operands in instructionTable[opcode]:
						raise SyntaxError('invalid operands %s for instruction %s at %d' % (operands, opcode, astnode.token.source_ptr))
					self.write(instructionTable[opcode][operands])
				if opcode in varInstructionTable_EigthBit:
					operands = tuple(map(lambda tok: tok.text, astnode.children[:-1]))
					if not operands in varInstructionTable_EigthBit[opcode]:
						raise SyntaxError('invalid operands %s for instruction %s at %d' % (operands, opcode, astnode.token.source_ptr))
					self.write(varInstructionTable_EigthBit[opcode][operands])
					imm_text = astnode.children[-1].text
					imm8 = self.parse_operand(imm_text)
					if imm8 > 0xFF or imm8 < 0:
						raise SyntaxError('invalid immediate %s at %d (%d not in 0-255)' % (imm_text, astnode.token.source, imm8))
					self.write(imm8)
				if opcode in varInstructionTable_SixteenBit:
					operands = tuple(map(lambda tok: tok.text, astnode.children[:-1]))
					if not operands in varInstructionTable_SixteenBit[opcode]:
						raise SyntaxError('invalid operands %s for instruction %s at %d' % (operands, opcode, astnode.token.source_ptr))
					self.write(varInstructionTable_SixteenBit[opcode][operands])
					imm_text = astnode.children[-1].text
					imm16 = self.parse_operand(imm_text)
					if imm16 > 0xFFFF or imm16 < 0:
						raise SyntaxError('invalid immediate %s at %d (%d not in 0-65535)' % (imm_text, astnode.token.source, imm16))
					self.write(imm16 & 0xFF) # low byte
					self.write((imm16 >> 8) & 0xFF) # hi byte

		return self.binary


class IntelHexEncoder:
    def __init__(self, binary):
        self.binary = binary

    def _gethex(self, num):
        a = hex(num)[2:]
        while len(a) < 2:
            a = '0' + a
        return a
    
    def _chunkify(self):
        chunks = []
        i = 0
        while i < len(self.binary):
            chunks.append(self.binary[i:i+32])
            i += 32
        return chunks
    
    def _encode_chunk(self, chunk, addr=0):
        intel_hex = ':' + self._gethex(len(binary)) + self._gethex(addr / 256) + self._gethex(addr % 256) + '00'
        checksum = (len(binary) + int(addr / 256) + addr % 256) % 256

        for b in binary:
            intel_hex += self._gethex(b)
            checksum = (checksum + b) % 256

        checksum = (256 - checksum) % 256
        intel_hex += self._gethex(checksum)

        return intel_hex.upper()
    
    def encode(self, addr=0):
        chunks = []
        for bin_chunk in self._chunkify():
            chunks.append(self._encode_chunk(bin_chunk, addr=addr))
            addr += 255

        return chunks


import sys

asm_in = open(sys.argv[1], 'r').read()

tokens = Lexer().lex(asm_in)
# print tokens
ast = parse(tokens)
# print ast
binary = Codegen(ast).generate()

chunks = IntelHexEncoder(binary).encode()

for chunk in chunks:
    print chunk


import serial
import glob
import time

# mac_irl
serial_ifs = glob.glob('/dev/cu.usbserial*')

if not serial_ifs:
    print 'No serial interfaces found!'
    exit(1)

print 'Using serial:', serial_ifs[0]

with serial.Serial(serial_ifs[0], 115200, timeout=0.05) as ser:
    for chunk in chunks:
        ser.write(chunk)
        ser.write('\r\n')
        print ser.read(size=300)


# output = '{ '
# for b in binary:
#  	output += '0x%02x, ' % (b,)
# output += '};'
# print output

# bin_out = ''.join(map(chr, binary))
# print bin_out.encode('hex')
# with open('rom.bin', 'wb') as f:
# 	f.write(bin_out)

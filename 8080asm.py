#!/usr/bin/env python

def preprocess_table(table):
	new_table = {}
	for k,v in table.iteritems():
		k = filter(bool, k.replace(',', ' ').split(' '))
		if k[0] not in new_table:
			new_table[k[0]] = {}
		new_table[k[0]][tuple(k[1:])] = v
	return new_table

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
    'rst 0': 199,
    'rz': 200,
    'ret': 201,
    'rst 1': 207,
    'rnc': 208,
    'pop d': 209,
    'push d': 213,
    'rst 2': 215,
    'rc': 216,
    'rst 3': 223,
    'rpo': 224,
    'pop h': 225,
    'xthl': 227,
    'push h': 229,
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
    'lxi d,': 17,
    'lxi h,': 33,
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

def lex(asm_in):
	asm_in += '\n' # HACK; eof is a delimiter
	delimiters = ' \n\t,;'
	reserved_names = ['a','b','c','d','e','f','g','h','i','l','m','bc','de','hl','sp']

	STATE_INITIAL, STATE_COMMENT, STATE_DIRECTIVE, STATE_OPERAND, STATE_LABEL = 0, 1, 2, 3, 4
	appending_states = [STATE_INITIAL, STATE_DIRECTIVE, STATE_OPERAND, STATE_LABEL]
	state = STATE_INITIAL
	ptr = 0
	ptr_tok_start = 0
	tokens = []
	aliases = {'dec': 'dcr'}
	token = ''
	while ptr < len(asm_in):
		cur_char = asm_in[ptr]
		if state == STATE_INITIAL:
			if cur_char == '.':
				if token:
					raise SyntaxError('invalid directive start at %d\n' % (ptr,))
				state = STATE_DIRECTIVE
				continue
			if cur_char == ':':
				state = STATE_LABEL
				continue

		if state == STATE_LABEL:
			if cur_char == ':':
				if not token:
					raise SyntaxError('invalid label name at %d\n' % (ptr,))

		if state == STATE_COMMENT:
			if cur_char == '\n':
				state = STATE_INITIAL
			else:
				ptr += 1
			continue

		if state in appending_states:
			if cur_char in delimiters: # end of token
				if token:
					if token in aliases: # apply preprocessor
						token = aliases[token]
					if state == STATE_DIRECTIVE:
						if len(token) < 2:
							raise SyntaxError('invalid directive at %d' % (ptr_tok_start,))
						tok_type = Token.TYPE_DIRECTIVE
						state = STATE_OPERAND
						token = token[1:].lower()
					elif state == STATE_INITIAL:
						token = token.lower()
						if token not in instructionTable and token not in varInstructionTable_SixteenBit and token not in varInstructionTable_EigthBit:
							raise SyntaxError('invalid instruction %s at %d' % (token, ptr_tok_start))
						tok_type = Token.TYPE_INSN
						state = STATE_OPERAND
					elif state == STATE_OPERAND:
						tok_type = Token.TYPE_OPERAND
						state = STATE_OPERAND
						token = token.lower()
					elif state == STATE_LABEL:
						tok_type = Token.TYPE_LABEL
						state = STATE_INITIAL
						token = token[:-1].lower()
						if token in reserved_names:
							raise SyntaxError('label %s at %d is a reserved name' % (token, ptr_tok_start))
					tokens.append(Token(token, tok_type, ptr_tok_start))
					token = ''
				else:
					ptr_tok_start = ptr
				if cur_char == ';':
					state = STATE_COMMENT
				elif cur_char == '\n':
					state = STATE_INITIAL
					tokens.append(Token('\n', Token.TYPE_STMT_DELIM, ptr))
			else:
				token += cur_char
			ptr += 1

	# print tokens
	return tokens

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
		self.ptr = 0

	def write(self, b):
		if len(self.binary) <= self.ptr:
			self.binary.extend([0] * (self.ptr - len(self.binary) + 1))
		self.binary[self.ptr] = b
		self.ptr += 1

	def generate(self):
		for astnode in ast.children:
			if astnode.node_type == ASTNode.TYPE_LABEL:
				labelname = astnode.token.text
				# print '%s at %d' % (labelname, self.ptr)
				self.labels[labelname] = self.ptr
			elif astnode.node_type == ASTNode.TYPE_DIRECTIVE:
				dirname = astnode.token.text
				if dirname == 'org':
					if len(astnode.children) != 1:
						raise SyntaxError('wrong argument count for directive %s at %d' % (dirname, astnode.token.source_self.ptr))
					org_val = parse_constant(astnode.children[0].text)
					# print 'org %d' % (org_val,)
					self.ptr = org_val
			elif astnode.node_type == ASTNode.TYPE_INSN:
				opcode = astnode.token.text
				if opcode in instructionTable:
					operands = tuple(map(lambda tok: tok.text, astnode.children))
					if not operands in instructionTable[opcode]:
						raise SyntaxError('invalid operands %s for instruction %s at %d' % (operands, opcode, astnode.token.source_self.ptr))
					self.write(instructionTable[opcode][operands])
				if opcode in varInstructionTable_EigthBit:
					operands = tuple(map(lambda tok: tok.text, astnode.children[:-1]))
					if not operands in varInstructionTable_EigthBit[opcode]:
						raise SyntaxError('invalid operands %s for instruction %s at %d' % (operands, opcode, astnode.token.source_self.ptr))
					self.write(varInstructionTable_EigthBit[opcode][operands])
					imm_text = astnode.children[-1].text
					imm8 = parse_constant(imm_text)
					if imm8 > 0xFF or imm8 < 0:
						raise SyntaxError('invalid immediate %s at %d (%d not in 0-255)' % (imm_text, astnode.token.source, imm8))
					self.write(imm8)
				if opcode in varInstructionTable_SixteenBit:
					operands = tuple(map(lambda tok: tok.text, astnode.children[:-1]))
					if not operands in varInstructionTable_SixteenBit[opcode]:
						raise SyntaxError('invalid operands %s for instruction %s at %d' % (operands, opcode, astnode.token.source_self.ptr))
					self.write(varInstructionTable_SixteenBit[opcode][operands])
					imm_text = astnode.children[-1].text
					if imm_text in self.labels:
						imm16 = self.labels[imm_text]
					else:
						imm16 = parse_constant(imm_text)
					if imm16 > 0xFFFF or imm16 < 0:
						raise SyntaxError('invalid immediate %s at %d (%d not in 0-65535)' % (imm_text, astnode.token.source, imm16))
					self.write(imm16 & 0xFF) # low byte
					self.write((imm16 >> 8) & 0xFF) # hi byte

		return self.binary

import sys

asm_in = open('fib.s','r').read()
# asm_in = sys.stdin.read()

tokens = lex(asm_in)
ast = parse(tokens)
# print ast
binary = Codegen(ast).generate()

output = '{ '
for b in binary:
	output += '0x%02x, ' % (b,)
output += '};'
print output

bin_out = ''.join(map(chr, binary))
print bin_out.encode('hex')
with open('rom.bin', 'wb') as f:
	f.write(bin_out)

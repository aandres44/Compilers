import ply.yacc as yacc
import ply.lex as lex

reserved = { 
    'int': 'INT',
    'float': 'FLOAT',
    'string': 'STRING',
    'bool': 'BOOL',
    'print': 'PRINT',
    'true': 'TRUE',
    'false': 'FALSE',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',
    'for': 'FOR',
    'while': 'WHILE',
    'do': 'DO',
    'and': 'AND',
    'or': 'OR'
}


tokens = tuple(['ID', 'FLOAT_VAL', 'INT_VAL', 'STR_VAL', 'PLUS', 'MINUS', 'MULT', 'DIV', 'EXP', 'ASSIGN', 'NOT_EQUALS',
                'EQ_MORE', 'EQ_LESS', 'MORE', 'LESS', 'EQUALS', 'LPAREN', 'RPAREN', 'LKEY', 'RKEY', 'FINISH'] + list(reservedWordDict.values()))


t_ignore = ' \t'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_EXP = r'\^'
t_ASSIGN = r'='
t_EQUALS = r'=='
t_NOT_EQUALS = r'!='
t_EQ_MORE = r'>='
t_EQ_LESS = r'<='
t_MORE = r'>'
t_LESS = r'<'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LKEY = r'{'
t_RKEY = r'}'
t_FINISH = r';'




# Token
def t_FLOAT_VAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INT_VAL(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_STR_VAL(t):
    r'\"[^\n]+\"'
    t.value = t.value.replace("\"", "")
    return t


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing rules

precedence = (
    ('left', 'AND', 'OR'),
    ('nonassoc', 'EQUALS', 'NOT_EQUALS', 'EQ_MORE', 'EQ_LESS', 'MORE', 'LESS'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIV'),
    ('left', 'EXP'),
    ('right', 'UMINUS'),  
)

# dictionary of names
instructions = ()

def p_start(p):
    '''start : statement'''
    global instructions
    instructions = p[1]


def p_statement(p):
    '''statement : print_stmt FINISH statement
                | register_stmt FINISH statement
                | condition_stmt statement
                | for_stmt statement
                | while_stmt statement
                | empty'''
    if len(p) > 2:
        if p[2] == ';':
            p[2] = p[3]
        p[0] = (p[1],) + p[2]
    else:
        p[0] = ()

def p_expression_operation(p):
    '''expression : expression AND expression
                | expression OR expression
                | expression PLUS expression
                | expression MINUS expression
                | expression MULT expression
                | expression DIV expression
                | expression EXP expression
                | expression EQUALS expression
                | expression NOT_EQUALS expression
                | expression EQ_MORE expression
                | expression EQ_LESS expression
                | expression MORE expression
                | expression LESS expression'''
    p[0] = ('operation', p[1], p[2], p[3])

def p_expression_id(p):
    "expression : ID"
    p[0] = p[1]


def p_expression_val(p):
    '''expression : FLOAT_VAL
                 | INT_VAL
                 | STR_VAL
                 | bool_val'''
    p[0] = p[1]


def p_bool_val(p):
    '''bool_val : TRUE
              | FALSE'''
    if p[1] == "true":
        p[0] = True
    elif p[1] == "false":
        p[0] = False

def p_error(p):
    if p:
        print(p)
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

yacc.yacc()
file = open("input.txt", "r")
s = file.read()
yacc.parse(s)

#File
in_data = []
with open('code2.txt') as file:
    in_data = file.readlines()

for data in in_data:
    yacc.parse(data)
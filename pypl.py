# -*-coding: utf8 -*- 

"""A compiler for PL/0 language
"""

from pyparsing import *

ident      = Word(alphas, alphanums)
integer    = Word(nums, nums)
aop        = Literal('+') ^ Literal('-')
mop        = Literal('*') ^ Literal('/') 
lop        = Literal('=') ^ Literal('<>') ^ Literal('<') ^ Literal('<=') ^ \
             Literal('>') ^ Literal('>=')
uop        = Literal('+') ^ Literal('-')
expression = Forward()
factor     = ident ^ integer ^ Suppress('(') + expression + Suppress(')')
term       = factor + ZeroOrMore(mop + factor)
expression << Optional(uop) + term + ZeroOrMore(aop + term)
condition  = expression + lop + expression ^ Keyword('odd') + expression
body       = Forward()
statement  = Forward()
statement  <<(ident + Suppress(':=') + expression ^ \
              Keyword('if') + condition + Keyword('then') + statement + \
                  Optional(Keyword('else') + statement) ^ \
              Keyword('while') + condition + Keyword('do') + statement ^ \
              Keyword('call') + ident + Optional(Suppress('(') + expression + \
                  ZeroOrMore(Suppress(',') + expression) + Suppress(')')) ^ \
              body | \
              Keyword('read') + Suppress('(') + ident + 
                  ZeroOrMore(Suppress(',') + ident) + Suppress(')') ^ \
              Keyword('write') + Suppress('(') + expression + \
                  ZeroOrMore(Suppress(',') + expression) + Suppress(')'))
body       << Keyword('begin') + statement +  \
                  ZeroOrMore(Suppress(';') + statement) + Keyword('end')
procedure  = Forward()
vardecl    = Keyword('var') + ident + ZeroOrMore(Suppress(',') + ident) + \
                  Suppress(';')
const      = ident + Suppress('=') + integer
constdecl  = Keyword('const') + const + ZeroOrMore(Suppress(',') + const) + \
             Suppress(';')
block      = Optional(constdecl) + Optional(vardecl) + Optional(procedure) + \
             body
procedure  << Keyword('procedure') + ident + Group(Suppress('(') + ident + \
                  ZeroOrMore(Suppress(',') + ident) + Suppress(')')) + \
                  Suppress(';') + block + ZeroOrMore(Suppress(';') + procedure)
program    = Keyword('program') + ident + Suppress(';') + block + Suppress('.')
comment    = Regex(r"\{[^}]*?\}")
program.ignore(comment)

import ast

def _i(loc, s):
    return {'lineno': lineno(loc, s), 'col_offset': col(loc, s)}

def _list(t):
    return t if isinstance(t, list) else [t]

def do_ident(s, loc, toks):
    ident = ast.Name(**_i(loc, s))
    ident.id = toks[0]
    # ident.ctx = None, here we do not know the context
    return [ident]

def do_integer(s, loc, toks):
    return [ast.Num(int(toks[0]), **_i(loc, s))]

def do_aop(s, loc, toks):
    return [ast.Add(**_i(loc, s)) if toks[0] == '+' else ast.Sub(**_i(loc, s))]

def do_uop(s, loc, toks):
    return [ast.UAdd(**_i(loc, s)) if toks[0] == '+' else ast.USub(**_i(loc, s))]

def do_mop(s, loc, toks):
    return [ast.Mult(**_i(loc, s)) if toks[0] == '*' else ast.Div(**_i(loc, s))]

def do_lop(s, loc, toks):
    _mapping = { 
        '=': ast.Eq, '<>': ast.NotEq, '<': ast.Lt, '<=': ast.LtE,
        '>': ast.Gt, '>=': ast.GtE
    }
    return [_mapping[toks[0]](**_i(loc, s))]

def do_const(s, loc, toks):
    toks[0].ctx = ast.Store()
    return [ast.Assign([toks[0],], toks[1], **_i(loc, s))]

def do_constdecl(s, loc, toks):
    return toks[1:] # ignore the leading 'const' keyword

def do_factor(s, loc, toks):
    for t in toks:
        if isinstance(t, ast.Name):
            # FIXME: validate this symbol
            t.ctx = ast.Load()

def do_term(s, loc, toks):
    left = toks[0]
    for i in range(1, len(toks), 2):
        left = ast.BinOp(left, toks[i], toks[i+1], **_i(loc, s))
    return [left]

def do_expression(s, loc, toks):
    if isinstance(toks[0], ast.USub) or isinstance(toks[0], ast.UAdd):
        left = ast.UnaryOp(toks[0], toks[1], **_i(loc, s))
        toks.pop(0)
    else:
        left = toks[0]
    for i in range(1, len(toks), 2):
        left = ast.BinOp(left, toks[i], toks[i+1], **_i(loc, s))
    return [left]

def do_condition(s, loc, toks):
    if len(toks) == 2:
        # odd integer: integer % 2 != 0
        return ast.Compare(ast.BinOp(toks[1], ast.Mod(), ast.Num(2)), 
                           [ast.NotEq()], [ast.Num(0)], **_i(loc, s))
    else:
        return ast.Compare(toks[0], [toks[1]], [toks[2]], **_i(loc, s))

def do_statement(s, loc, toks):
    if isinstance(toks[0], ast.Name):
        toks[0].ctx = ast.Store()
        return [ast.Assign([toks[0],], toks[1], **_i(loc, s))]
    elif toks[0] == 'if':
        return [ast.If(toks[1], _list(toks[3]), \
                _list(toks[5]) if len(toks) == 6 else [], **_i(loc, s))]
    elif toks[0] == 'while':
        return [ast.While(toks[1], _list(toks[3]), [], **_i(loc, s))]
    elif toks[0] == 'call':
        toks[1].ctx = ast.Load()
        return [ast.Call(toks[1], toks[2:], [], [], [], **_i(loc, s))]
    elif toks[0] == 'read':
        pass
    elif toks[0] == 'write':
        return [ast.Print(None, toks[1:], True, **_i(loc, s))]
    else:
        return toks

def do_vardecl(s, loc, toks):
    return []

def do_body(s, loc, toks):
    return [toks[1:-1],]   # ignore keyword `begin` and `end`

def do_procedure(s, loc, toks):
    pass

def do_block(s, loc, toks):
    return [toks[:-1] + toks[-1]]

def do_program(s, loc, toks):
    return ast.Module(body=toks[2], **_i(loc, s))

for k in locals().keys():
    if k.startswith('do_'):
        name = k[3:]
        expr = vars()[name]
        action = vars()[k]
        expr.setName(name)
        expr.setParseAction(action)
        #expr.setDebug()
    
def compile(source, filename, mode):
    """Compile PL/0 source into PyCodeObject"""
    print source

if __name__ == '__main__':
    t1 = """
        {comment1}
        program main;
        var i, j, max, num;
        begin
            i := 0; max := 1000;
            num := 0;
            while i <= max do
            begin
                j := 2;
                while j < i do
                    {comment2}
                    if ( i-i/j*j ) = 0 then
                        j := i+1
                    else
                        j := j+1;
                if j = i then
                begin
                    write( i );
                    num := num +1
                end;
                i := i+1
            end
        end.
    """

    a = program.parseString(t1)[0]

    from astpp import dump
    import pdb
    #pdb.set_trace()
    print dump(a)
    #print ast.dump(a, True, True)
    exec(__builtins__.compile(a, "<ast>", "exec"))

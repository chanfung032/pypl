# -*-coding: utf8 -*- 

"""A compiler for PL/0 language
"""

from pyparsing import *

LPAR, RPAR, SEMI, COMMA, PERIOD = map(Suppress, '();,.')

keyword = ('program', 'procedure', 'var', 'const', 'begin', 'end',
           'while', 'do', 'if', 'then', 'else', 'odd', 'call', 'read', 'write')
__dict__ = locals()
for k in keyword:
    __dict__[k.upper()] = Keyword(k)

ident     = Word(alphas, alphanums)
integer   = Word(nums, nums)

aop       = Literal('+') ^ Literal('-')
mop       = Literal('*') ^ Literal('/')
lop       = Literal('=') ^ Literal('<>') ^ Literal('<') ^ Literal('<=') ^ \
            Literal('>') ^ Literal('>=')
uop       = Literal('+') ^ Literal('-')

expr      = Forward()
factor    = ident ^ integer ^ LPAR + expr + RPAR
term      = factor + ZeroOrMore(mop + factor)
expr      << Optional(uop) + term + ZeroOrMore(aop + term)
cond      = expr + lop + expr ^ ODD + expr

body      = Forward()
stmt      = Forward()
stmt      <<(ident + ':=' + expr ^ \
             IF + cond + THEN + stmt + Optional(ELSE + stmt) ^ \
             WHILE + cond + DO + stmt ^ \
             ident + LPAR + Optional(delimitedList(expr)) + RPAR ^ \
             body ^ \
             WRITE + LPAR + delimitedList(expr) + RPAR)
body      << BEGIN + stmt + ZeroOrMore(SEMI + stmt) + END

vardecl   = VAR + ident + ZeroOrMore(COMMA + ident) + SEMI
const     = ident + Suppress('=') + integer
constdecl = CONST + const + ZeroOrMore(COMMA + const) + SEMI

proc      = Forward()
block     = Optional(constdecl) + Optional(vardecl) + Optional(proc) + body

proc      << PROCEDURE + ident + LPAR + Group(Optional(delimitedList(ident))) + RPAR + \
             SEMI + block + ZeroOrMore(SEMI + proc)

program   = PROGRAM + ident + SEMI + block + PERIOD

comment   = Regex(r"\{[^}]*?\}")
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

def do_expr(s, loc, toks):
    if isinstance(toks[0], ast.USub) or isinstance(toks[0], ast.UAdd):
        left = ast.UnaryOp(toks[0], toks[1], **_i(loc, s))
        start = 2
    else:
        left = toks[0]
        start = 1
    for i in range(start, len(toks), 2):
        left = ast.BinOp(left, toks[i], toks[i+1], **_i(loc, s))
    return [left]

def do_cond(s, loc, toks):
    if len(toks) == 2:
        # odd integer: integer % 2 != 0
        return ast.Compare(ast.BinOp(toks[1], ast.Mod(), ast.Num(2)), 
                           [ast.NotEq()], [ast.Num(0)], **_i(loc, s))
    else:
        return ast.Compare(toks[0], [toks[1]], [toks[2]], **_i(loc, s))

def do_stmt(s, loc, toks):
    if len(toks) == 3 and toks[1] == ':=':
        toks[0].ctx = ast.Store()
        return [ast.Assign([toks[0],], toks[2], **_i(loc, s))]
    elif toks[0] == 'if':
        return [ast.If(toks[1], _list(toks[3]), \
                _list(toks[5]) if len(toks) == 6 else [], **_i(loc, s))]
    elif toks[0] == 'while':
        return [ast.While(toks[1], _list(toks[3]), [], **_i(loc, s))]
    elif isinstance(toks[0], ast.Name):
        # use primitive `print`
        if toks[0].id == 'write':
            return [ast.Print(None, toks[1:], True, **_i(loc, s))]
        toks[0].ctx = ast.Load()
        call = ast.Call(toks[0], toks[1:], [], None, None, **_i(loc, s))
        return [ast.Expr(call, **_i(loc, s))]
    else:
        return toks

def do_vardecl(s, loc, toks):
    return []

def do_body(s, loc, toks):
    return [toks[1:-1],]   # ignore keyword `begin` and `end`

def do_proc(s, loc, toks):
    for t in toks[2]:
        t.ctx = ast.Param()
    args = ast.arguments(args=list(toks[2]), vararg=None, kwarg=None,
                         defaults=[], kw_defaults=[])
    return [ast.FunctionDef(toks[1].id, args, toks[3], [], **_i(loc, s))] + toks[4:]

def do_block(s, loc, toks):
    return [toks[:-1] + toks[-1]]

def do_program(s, loc, toks):
    return ast.Module(body=toks[2], **_i(loc, s))

def _dis(name, s, loc, toks):
    print '>', name, toks
    rc = globals()[name](s, loc, toks)
    print '<', name, rc
    return rc

for k in locals().keys():
    if k.startswith('do_'):
        name = k[3:]
        element = vars()[name]
        action = vars()[k]
        element.setName(name)
        import functools
        element.setParseAction(functools.partial(_dis, k))
        #element.setParseAction(action)
        #expr.setDebug()
    
def compile(source, fname):
    """Compile PL/0 source into PyCodeObject"""
    try:
        a = program.parseString(source)[0]
    except ParseException, e:
        # see Objects/exceptions.c:SyntaxError_init
        raise SyntaxError(e.msg, (fname,
                                  lineno(e.loc, e.pstr),
                                  col(e.loc, e.pstr),
                                  line(e.loc, e.pstr)))

    return __builtins__.compile(a, fname, 'exec')

if __name__ == '__main__':
    tests = """
        {comment1}
        program main;

        procedure test1();
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
        end;

        procedure test2();
        const i = 8;
        begin
            write(-i+4)
        end;

        procedure test3(a);
        begin write(a) end;

        procedure test4(a, b);
        begin write(a + b) end

        begin
            test1();
            test2();
            test3(8);
            test4(1, 2)
        end.
    """

    tests1 = """
    program main;
    procedure add(a, b);
    begin write(a + b) end

    begin add(1, 2) end.
    """

    exec(compile(tests, '<none>'))

    from astpp import dump
    import pdb
    #a = program.parseString(t3)[0]
    #pdb.set_trace()
    #print dump(a)
    #print ast.dump(a, True, True)
    #exec(__builtins__.compile(a, "<ast>", "exec"))

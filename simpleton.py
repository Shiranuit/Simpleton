import string
import time

OPS = [
    '==',
    '>=',
    '<=',
    '>>',
    '<<',
    '&&',
    '||',
    '**',
    '>',
    '<',
    '|',
    '&',
    '^',
    '=',
    '~',
    '*',
    '+',
    '-',
    '/',
    '%',
]

OPPrio = {
    '=': 0,

    '&&': 1,
    '||': 1,

    '==': 2,
    '>': 2,
    '<': 2,
    '>=': 2,
    '<=': 2,

    '<<': 3,
    '>>': 3,

    '+': 4,
    '-': 4,
    '&': 4,
    '|': 4,
    '^': 4,

    '*': 5,
    '/': 5,
    '%': 5,
    '**': 6,
}

BinOPFunc = {
    '+': lambda x, y: x + y,
    '-': lambda x, y: x - y,
    '*': lambda x, y: x * y,
    '/': lambda x, y: x / y,
    '%': lambda x, y: x % y,
    '**': lambda x, y: x ** y,
    '<<': lambda x, y: x << y,
    '>>': lambda x, y: x >> y,
    '&': lambda x, y: x & y,
    '|': lambda x, y: x | y,
    '^': lambda x, y: x ^ y,
    '>': lambda x, y: x > y,
    '<': lambda x, y: x < y,
    '>=': lambda x, y: x >= y,
    '<=': lambda x, y: x <= y,
    '==': lambda x, y: x == y,
    '&&': lambda x, y: x and y,
    '||': lambda x, y: x or y,
}

UnaryOPFunc = {
    '+': lambda x: +x,
    '-': lambda x: -x,
    '~': lambda x: ~x,
}

Keywords = [
    'if',
    'else',
    'while',
]

class TokenType:
    NUMBER = 'NUMBER'
    OP = 'OP'
    PARANTHESIS = 'PARANTHESIS'
    STRING = 'STRING'
    BRACKET = 'BRACKET'
    NAME = 'NAME'
    COMMA = 'COMMA'
    SEMICOLON = 'SEMICOLON'
    KEYWORD = 'KEYWORD'

class Token():
    def __init__(self, token, type, start):
        self.token = token
        self.start = start
        self.end = start + len(token)
        self.type = type

class Lexer():
    NAMEFULLSET = string.ascii_letters + string.digits
    def __init__(self, text):
        self.text = text
        self.textlen = len(text)
        self.cur = 0

    def eof(self):
        return self.cur >= self.textlen

    def eat(self):
        self.cur += 1
        return self.text[self.cur - 1]
    
    def curr(self):
        return self.text[self.cur]

    def skipWhiteSpace(self):
        while not self.eof() and self.curr() in ' \t\n\r':
            self.eat()

    def parseNumber(self):
        start = self.cur
        num = self.eat()
        decimal = False
        while not self.eof() and self.curr() in '.0123456789':
            if self.curr() == '.':
                if decimal:
                    raise Exception("Unexpected token '%s'" % self.curr())
                decimal = True
            num += self.eat()
        return Token(num, TokenType.NUMBER, start)

    def parseString(self, quote="'"):
        start = self.cur
        str = self.eat()
        while not self.eof() and self.curr() != quote:
            str += self.eat()
        if self.eof():
            self.UnexpectedEOF()
        self.eat()
        return Token(str, TokenType.STRING, start)

    def parseName(self):
        start = self.cur
        name = self.eat()
        while not self.eof() and self.curr() in self.NAMEFULLSET:
            name += self.eat()
        if name in Keywords:
            return Token(name, TokenType.KEYWORD, start)    
        return Token(name, TokenType.NAME, start)

    def parseOP(self):
        start = self.cur
        op = self.eat()
        while not self.eof() and op+self.curr() in OPS:
            op += self.eat()
        return Token(op, TokenType.OP, start)
    
    def nextToken(self):
        if self.eof():
            return
        self.skipWhiteSpace()
        if self.eof():
            return
        if self.curr() in '0123456789':
            return self.parseNumber()
        elif self.curr() in '"\'':
            return self.parseString(quote=self.eat())
        elif self.curr() in '()':
            token = Token(self.curr(), TokenType.PARANTHESIS, self.cur)
            self.eat()
            return token
        elif self.curr() in '{}':
            token = Token(self.curr(), TokenType.BRACKET, self.cur)
            self.eat()
            return token
        elif self.curr() == ',':
            token = Token(self.curr(), TokenType.COMMA, self.cur)
            self.eat()
            return token
        elif self.curr() == ';':
            token = Token(self.curr(), TokenType.SEMICOLON, self.cur)
            self.eat()
            return token
        elif self.curr() in OPS:
            return self.parseOP()
        elif self.curr() in string.ascii_letters:
            return self.parseName()
        raise Exception("Unexpected token '%s'" % self.curr())

class Node():
    pass

class NodeExpr(Node):
    def __init__(self, expr:Node):
        self.expr = expr

class NodeBinOP(Node):
    def __init__(self, op, left:Node, right:Node):
        self.op = op
        self.left = left
        self.right = right

class NodeUnaryOP(Node):
    def __init__(self, op, right:Node):
        self.op = op
        self.right = right

class NodeName(Node):
    def __init__(self, name):
        self.name = name

class NodeConst(Node):
    def __init__(self, value):
        self.value = value

class NodeCall(Node):
    def __init__(self, name:NodeName, args:list):
        self.name = name
        self.args = args

class NodeBlock(Node):
    def __init__(self, instructions:list):
        self.instr = instructions

class NodeIf(Node):
    def __init__(self, cond:NodeExpr, block:NodeBlock, elseCond=None):
        self.cond = cond
        self.block = block
        self.elseCond = elseCond

class NodeElse(Node):
    def __init__(self, node):
        self.node = node

class NodeWhile(Node):
    def __init__(self, cond:NodeExpr, block:NodeBlock):
        self.cond = cond
        self.block = block

class NodeConstString(NodeConst):
    def __init__(self, value):
        super().__init__(value)

class Parser():

    def curr(self):
        return self.tokens[self.cur]

    def next(self, offset=1):
        if self.cur + offset < self.totalTokens:
            return self.tokens[self.cur + offset]
    
    def eat(self):
        self.cur += 1
        return self.tokens[self.cur - 1]
    
    def eof(self):
        return self.cur >= self.totalTokens

    def expect(self, token, eatToken=True):
        if self.curr().token != token:
            raise Exception("Expected '%s', got '%s'" % (token, self.curr().token))
        if eatToken:
            return self.eat()
    
    def expectType(self, type, eatToken=True):
        if self.curr().type != type:
            raise Exception("Expected '%s', got '%s'" % (type, self.curr().type))
        if eatToken:
            return self.eat()

    def UnexpectedEOF(self):
        raise Exception('Unexpected End Of File')

    def UnexpectedEOE(self):
        raise Exception('Unexpected End Of Expression')
    
    def UnexpectedToken(self, token):
        raise Exception("Unexpected token '%s'" % token.token)

    def parseUnary(self):
        op = self.eat()
        if self.eof():
            self.UnexpectedEOE()
        return NodeUnaryOP(op.token, self.parseExpr())

    def parseName(self):
        return NodeName(self.eat().token)

    def parseNumber(self):
        token = self.eat().token
        if '.' in token:
            return NodeConst(float(token))
        return NodeConst(int(token))

    def parseString(self):
        token = self.eat().token
        return NodeConstString(str(token))
    
    def parseBinOp(self, left, prio=0):
        self.expectType(TokenType.OP,eatToken=False)
        op = self.curr()
        opPriority = OPPrio[op.token]
        if prio >= opPriority:
            return
        self.eat()
        right = self.parseExpr(opPriority)
        if not right:
            self.UnexpectedEOE()
        return NodeBinOP(op.token, left, right)

    def parseCall(self):
        name = self.eat()
        self.expect('(')
        args = [self.parseExpr()]
        while not self.eof() and self.curr().token == ',':
            self.eat()
            args.append(self.parseExpr())
        self.expect(')')
        return NodeCall(name.token, args)

    def parseElse(self):
        self.eat()
        if self.curr().token == 'if':
            return NodeElse(self.parseIf())
        return NodeElse(self.parseBlock())

    def parseIf(self):
        self.eat()
        self.expect('(', eatToken=False)
        cond = self.parseExpr()
        self.expect('{', eatToken=False)
        block = self.parseBlock()
        if self.curr().token == 'else':
            return NodeIf(cond, block, self.parseElse())
        return NodeIf(cond, block)

    def parseWhile(self):
        self.eat()
        self.expect('(', eatToken=False)
        cond = self.parseExpr()
        self.expect('{', eatToken=False)
        block = self.parseBlock()
        return NodeWhile(cond, block)
        

    def parseExpr(self, prio=-1):
        left = None

        if self.curr().token == '(':
            self.eat()
            left = self.parseExpr()
            self.expect(')')
        
        elif self.curr().token == '{':
            return self.parseBlock()
        
        elif self.curr().token == 'if':
            return self.parseIf()
        
        elif self.curr().token == 'while':
            return self.parseWhile()
        
        elif self.curr().token == 'func':
            return self.parseFunc()

        elif self.curr().type == TokenType.NAME:
            if self.next() and self.next().token == '(':
                left = self.parseCall()
            else:
                left = self.parseName()
        elif self.curr().type == TokenType.NUMBER:
            left = self.parseNumber()
        elif self.curr().type == TokenType.STRING:
            left = self.parseString()
            
        if not self.eof() and self.curr().type == TokenType.OP:
            if not left and self.curr().token in '-+~':
                left = self.parseUnary()
        
        while not self.eof() and self.curr().type == TokenType.OP:
            binop = self.parseBinOp(left, prio=prio)
            if binop:
                left = binop
            else:
                break
        
        return left

    def parseBlock(self):
        expr = []
        bracket = False
        if self.curr().token == '{':
            bracket = True
            self.eat()
        while not self.eof() and (not bracket or not self.curr().token == '}'):
            expr.append(NodeExpr(self.parseExpr()))
            if not self.eof() and self.curr().token == ';':
                self.eat()
        if bracket:
            self.expect('}')
        return NodeBlock(expr)

    def parse(self, text):
        lexer = Lexer(text)
        self.cur = 0
        self.tokens = []
        while token := lexer.nextToken():
            self.tokens.append(token)
        self.totalTokens = len(self.tokens)

        return self.parseBlock()

def dump(node, indent=4,level=0):
    space = ' ' * (indent * level)
    if type(node) is NodeBlock:
        print(space + '[Block]')
        for expr in node.instr:
            dump(expr, level = level + 1, indent=indent)
    elif type(node) is NodeIf:
        print(space + '[If]')
        print(space + '| Condition')
        dump(node.cond, level = level + 1, indent=indent)
        print(space + '| Block')
        dump(node.block, level = level + 1, indent=indent)
        if node.elseCond:
            dump(node.elseCond, level = level, indent = indent)
    elif type(node) is NodeElse:
        print(space + '[Else]')
        dump(node.node, level = level + 1, indent = indent)
    elif type(node) is NodeWhile:
        print(space + '[While]')
        print(space + '| Condition')
        dump(node.cond, level = level + 1, indent=indent)
        print(space + '| Block')
        dump(node.block, level = level + 1, indent=indent)
    elif type(node) is NodeExpr:
        print(space + '[Expression]')
        dump(node.expr, level=level+1, indent=indent)
    elif type(node) is NodeName:
        print(space + '[Name]: %s' % node.name)
    elif type(node) is NodeConst:
        print(space + '[Const]: %s' % node.value)
    elif type(node) is NodeConstString:
        print(space + '[Const String]: "%s"' % node.value)
    elif type(node) is NodeUnaryOP:
        print(space + '[Unary]')
        print(space + '| OP: %s' % node.op)
        print(space + '| Left:')
        dump(node.right, level = level + 1, indent = indent)
    elif type(node) is NodeBinOP:
        print(space + '[BinOp]')
        print(space + '| OP: %s' % node.op)
        print(space + '| Left:')
        dump(node.left, level = level + 1, indent = indent)
        print(space + '| Rigth:')
        dump(node.right, level = level + 1, indent = indent)
    elif type(node) is NodeCall:
        print(space + '[Call]')
        print(space + '| Name: %s' % node.name)
        print(space + '| Params:')
        for arg in node.args:
            dump(arg, indent=indent, level = level + 1)
    
def ASTToExpr(node):
    if type(node) is NodeBlock:
        exprs = []
        for expr in node.instr:
            exprs.append(ASTToExpr(expr))
        return '{\n%s\n}' % ('\n'.join(exprs))
    elif type(node) is NodeExpr:
        return ASTToExpr(node.expr)
    elif type(node) is NodeName:
        return node.name
    elif type(node) is NodeConst:
        return str(node.value)
    elif type(node) is NodeConstString:
        return '"%s"' % str(node.value)
    elif type(node) is NodeIf:
        return 'if (%s) %s%s' % (ASTToExpr(node.cond), ASTToExpr(node.block), node.elseCond and ' ' + ASTToExpr(node.elseCond) or '')
    elif type(node) is NodeWhile:
        return 'while (%s) %s' % (ASTToExpr(node.cond), ASTToExpr(node.block))
    elif type(node) is NodeElse:
        return 'else %s' % ASTToExpr(node.node)
    elif type(node) is NodeUnaryOP:
        return '%s(%s)' % (node.op, ASTToExpr(node.right))
    elif type(node) is NodeBinOP:
        return '(%s) %s (%s)' % (ASTToExpr(node.left), node.op, ASTToExpr(node.right))
    elif type(node) is NodeCall:
        params = []
        for arg in node.args:
            if eval := ASTToExpr(arg):
                params.append(eval)
        return '%s(%s)' % (node.name, ', '.join(params))

def getConst(node:NodeBinOP):
    if type(node.left) is NodeConst:
        return node.left.value
    elif type(node.right) is NodeConst:
        return node.right.value

def getNonConst(node:NodeBinOP):
    if not type(node.left) is NodeConst:
        return node.left
    elif not type(node.right) is NodeConst:
        return node.right

def OptimizeAST(node):
    if type(node) is NodeBlock:
        for i in range(len(node.instr)):
            node.instr[i] = OptimizeAST(node.instr[i])
        return node
    if type(node) is NodeIf:
        node.cond = OptimizeAST(node.cond)
        node.block = OptimizeAST(node.block)
        node.elseCond = OptimizeAST(node.elseCond)
        return node
    if type(node) is NodeElse:
        node.node = OptimizeAST(node.node)
        return
    if type(node) is NodeWhile:
        node.cond = OptimizeAST(node.cond)
        node.block = OptimizeAST(node.block)
        return node
    if type(node) is NodeExpr:
        node.expr = OptimizeAST(node.expr)
        return node
    if type(node) is NodeName:
        return node
    if type(node) is NodeConst:
        return node
    if type(node) is NodeConstString:
        return node
    if type(node) is NodeCall:
        for i in range(len(node.args)):
            node.args[i] = OptimizeAST(node.args[i])
        return node
    if type(node) is NodeUnaryOP:
        node.right = OptimizeAST(node.right)
        if type(node.right) is NodeConst:
            return NodeConst(UnaryOPFunc[node.op](node.right.value))
        return node
    if type(node) is NodeBinOP:
        node.left = OptimizeAST(node.left)
        node.right = OptimizeAST(node.right)

        if type(node.left) is NodeConst and type(node.right) is NodeConst:
            return NodeConst(BinOPFunc[node.op](node.left.value, node.right.value))
        return node

def EvalAST(node, env={}):
    if type(node) is NodeBlock:
        for expr in node.instr:
            EvalAST(expr, env=env)
        return
    if type(node) is NodeIf:
        if (EvalAST(node.cond, env=env)):
            EvalAST(node.block, env=env)
        elif node.elseCond:
            EvalAST(node.elseCond.node,env=env)
        return
    if type(node) is NodeWhile:
        while (EvalAST(node.cond, env=env)):
            EvalAST(node.block, env=env)
    if type(node) is NodeExpr:
        return EvalAST(node.expr, env=env)
    if type(node) is NodeConst:
        return node.value
    if type(node) is NodeConstString:
        return node.value
    if type(node) is NodeName:
        if not (value := env.get(node.name)) is None:
            return value
        raise Exception("Undefined variable '%s'" % node.name)
    if type(node) is NodeUnaryOP:
        return UnaryOPFunc[node.op](EvalAST(node.right, env=env))
    if type(node) is NodeBinOP:
        if node.op == '=':
            env[node.left.name] = EvalAST(node.right, env=env)
            return env[node.left.name]
        return BinOPFunc[node.op](EvalAST(node.left, env=env), EvalAST(node.right, env=env))
    if type(node) is NodeCall:
        if func := env.get(node.name):
            params = []
            for i in range(len(node.args)):
                if node.args[i]:
                    params.append(EvalAST(node.args[i], env=env))
            return func(*params)
        raise Exception("Undefined function '%s'" % node.name)

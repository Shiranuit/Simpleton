from simpleton import *

def Eval(text, env={
    'print': print,
    'int': int,
    'float': float,
    'input': input,
    'time': time.time,
}):
    parser = Parser()
    AST = parser.parse(text)
    AST = OptimizeAST(AST)
    return EvalAST(AST, env)


code = """
{
    i = 0
    start = time()
    while (time() - start < 1) {
        i = i + 1
    }
    print(i)
}
"""

# Print base AST
parser = Parser()
AST = parser.parse(code)
dump(AST)
print()

# Print Optimized AST
AST = parser.parse(code)
dump(OptimizeAST(AST))
print()

# Print Optimized Code
print('-'*50)
print()
print(ASTToExpr(OptimizeAST(AST)))
print()

# Run Code
print('-'*50)
print()
Eval(code)

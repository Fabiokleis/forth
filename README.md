# forth
Simple implementation for very few limited subset of [forth](https://en.wikipedia.org/wiki/Forth_(programming_language)) in haskell


feel free to add more feature by contributing. 

## Structure
Program parses and evaluates all expressions using the following structure:
```haskell
type Program = [Expr]
type Stack = [Int]
type Row = (String, Program)
type Table = [Row]

data Evaluator = Evaluator {
  program :: Program,
  stack :: Stack,
  table :: Table
} deriving Show
```
A full compiled code will populate `Program` and `Table` with expressions:
1. Program: All expressions compiled, a list of expressions
2. Table: Store compiled symbols, a list of tuples string expressions

## Expressions
```haskell
data Expr =
  Lit Int
  | Sym String -- symbol defs
  | Add
  | Sub
  | Mul
  | Div
  | Dup
  | Swap
  | Over
  | Drop
  | Dot
  | Emit
  | Equal
  | Cr
  | Lf
  deriving Show
```


## Parser
The result `Program` and `Table` are result of parsing:
```haskell
parseTokens :: String -> Either (String, Program, Table) (Program, Table)
parse :: [String] -> Program -> Table -> Either (String, Program, Table) (Program, Table)
parseExpr :: String -> Either String Expr
```
1. parseTokens: Expects the source code as an input and break it into tokens
2. parse: Get a list of tokens and populates `Program` and `Table` based on parsed expressions
    - checks for definitions: Recursively match the current token to definition `Sym`  
    - parseExpr: Match the current token to `Expr` 
## Evaluator
To evaluate a full compiled `Program` and `Table` 
```haskell
eval :: Evaluator -> IO Evaluator
```
eval get a single `Evalutator` data and recursively runs all expressions using `Program` and `Table`
by pushing/poping values from top of the `Stack`, and produces side effects for commands like `Dot` and `Emit`.

## Example
This following program outputs 'forth' in the standard output:
```haskell
ghci> :l forth.hs
ghci> let source = ": f 102 ; : o 111 ; : r 114 ; : t 116 ; : h 104 ; : out f emit o emit r emit t emit h emit ; out 10 emit"
ghci> let Right (prog, table) = parseTokens source
ghci> eval (Evaluator prog [] table)
forth
Evaluator {program = [], stack = [], table = [("out",[Emit,Sym "h",Emit,Sym "t",Emit,Sym "r",Emit,Sym "o",Emit,Sym "f"]),("h",[Lit 104]),("t",[Lit 116]),("r",[Lit 114]),("o",[Lit 111]),("f",[Lit 102])]}
```


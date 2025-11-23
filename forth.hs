module Forth where

-- simple evaluator for subset forth 

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
  deriving (Show, Eq)

type Program = [Expr]
type Stack = [Int]
type Row = (String, Program)
type Table = [Row]

data Evaluator = Evaluator {
  program :: Program,
  stack :: Stack,
  table :: Table
} deriving Show

parseExpr :: String -> Either String Expr
parseExpr "+" = Right Add 
parseExpr "-" = Right Sub
parseExpr "*" = Right Mul
parseExpr "/" = Right Div
parseExpr "." = Right Dot
parseExpr "=" = Right Equal
parseExpr "dup"  = Right Dup  
parseExpr "swap" = Right Swap 
parseExpr "over" = Right Dup
parseExpr "drop" = Right Drop
parseExpr "emit" = Right Emit
parseExpr "DUP" = Right Dup
parseExpr "SWAP" = Right Swap
parseExpr "OVER" = Right Over
parseExpr "DROP" = Right Drop
parseExpr "CR" = Right Cr
parseExpr "LF" = Right Lf
parseExpr any = Left ("undefined expression: " ++ "'" ++ any ++ "'")

parse :: [String] -> Program -> Table -> Either (String, Program, Table) (Program, Table)
parse [] prog table = Right (prog, table)
parse (":":symbol:rest) prog table =
  case lookup symbol table of
    Just _ -> Left ("duplicate name: " ++ "'" ++ symbol ++ "'", prog, table)
    Nothing ->
      case parse exprs [] table of
        Left err -> Left err
        Right (iexprs, itable) ->
          parse next prog ((symbol, iexprs):itable) 
      where
        exprs = takeWhile (\x -> x /= ";") rest -- check for nested defs
        next = tail (dropWhile (\x -> x /= ";") rest) -- fails on unfinished defs 
parse (token:rest) prog table =
    case reads token :: [(Int, String)] of
      [(lit, "")] -> parse rest ((Lit lit) : prog) table
      _ ->
        case parseExpr token of
             Right expr -> parse rest (expr : prog) table
             Left err ->
               case lookup token table of
                 Just _ -> parse rest ((Sym token) : prog) table
                 Nothing -> Left (err, prog, table)

parseTokens :: String -> Either (String, Program, Table) (Program, Table)
parseTokens code =
  case parse (words code) [] [] of
    Right (prog, table) -> Right ((reverse prog), table)
    Left e -> Left e

stackUnderflow :: Expr -> String
stackUnderflow expr = "stack underflow: " ++ show expr

eval :: Evaluator -> IO Evaluator
eval state@(Evaluator [] _ _) = return state
eval (Evaluator (expr:rest) stack table) =
  case expr of
    Lit lit -> eval (Evaluator rest (lit:stack) table)
    Add -> binaryOp (+)
    Sub -> binaryOp (-)
    Mul -> binaryOp (*)
    Div -> binaryOp div
    Lf -> undefined -- todo
    Cr -> undefined -- todo
    Dot ->
      case stack of
        (x:xs) -> do
          putStr (show x ++ " ")
          eval (Evaluator rest xs table)
        _ -> error $ stackUnderflow expr
    Emit ->
      case stack of
        (x:xs) -> do
          putChar (toEnum x :: Char)
          eval (Evaluator rest xs table)
        _ -> error $ stackUnderflow expr
    Dup ->
      case stack of
        (x:xs) -> eval (Evaluator rest (x:x:xs) table)
        _ -> error $ stackUnderflow expr
    Swap ->
      case stack of
        (y:x:xs) -> eval (Evaluator rest (x:y:xs) table)
        _ -> error $ stackUnderflow expr
    Drop ->
      case stack of
        (_:xs) -> eval (Evaluator rest xs table)
        _ -> error $ stackUnderflow expr
    Over ->
      case stack of
        (y:x:xs) -> eval (Evaluator rest (x:y:x:xs) table)
        _ -> error $ stackUnderflow expr
    Equal ->
      case stack of
        (y:x:xs) ->
          let res = if x == y then -1 else 0 
          in eval (Evaluator rest (res:xs) table)
        _ -> error $ stackUnderflow expr
    Sym key ->
      case lookup key table of
        Just defs -> eval (Evaluator ((reverse defs) ++ rest) stack table)
        Nothing -> error $ "undefined word: " ++ "'" ++ key ++ "'"
  where
    binaryOp f =
      case stack of
        (y:x:xs) -> eval (Evaluator rest (f x y : xs) table)
        _ -> error $ "stack underflow binary operator: " ++ show expr

progs :: [String]
progs = [
      "1",
      "3 4 + 9 -",
      "10 5 - 2 *",
      "20 5 / 3 +",
      "1 2 3 * -",
      "5 2 10 /",
      "1 2 3 +",
      "1 + 3",
      "1 2 + : defined 10 ; 1",
      ": empty ;",
      ": def1 1 ; : def2 2 ; : def3 3 ;  def1 def2 def3 def2 +",
      ": f 1 ; f dup +",
      ": r 5 ; : l 10 ; l r =",
      ": l 5 ; : r l 5 + ; r 10 =",
      ": l 10 ; : r dup l + ; 2 l * r =",
      ": l 1 ; : r dup dup ; l r",
      ": f 102 ; : o 111 ; : r 114 ; : t 116 ; : h 104 ; : out f emit o emit r emit t emit h emit ; out 10 emit"
      ]
        
main :: IO ()
main = do
  let prog = (!!) progs 16
  putStrLn "------------- Forth ------------"
  putStrLn $ "source: " ++ "'" ++ prog ++ "'"
  case parseTokens prog of
    Left (err, p, t) -> do
      putStrLn $ "program: " ++ show p
      putStrLn $ "table: " ++ show t
      putStrLn $ "error: " ++ err
    Right (iprog, itable) -> do
      putStr "stdout: "
      (Evaluator program stack table) <- eval (Evaluator iprog [] itable)
      putStrLn $ "table: " ++ show table
      putStrLn $ "program: " ++ show iprog
      putStrLn $ "stack: " ++ show stack
  putStrLn "--------------------------------" 

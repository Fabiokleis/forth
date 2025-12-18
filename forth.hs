module Forth where

-- simple evaluator for forth subset

data Expr =
  Lit Int
  | Word String -- word definition
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
parseExpr any = Left ("**undefined expression**: " ++ "'" ++ any ++ "'")

parse :: [String] -> Program -> Table -> Either (String, Program, Table) (Program, Table)
parse [] prog table = Right (prog, table)
parse (":":word:rest) prog table =
  case lookup word table of
    Just _ -> Left ("**duplicate name**: " ++ "'" ++ word ++ "'", prog, table)
    Nothing ->
      case parse exprs [] table of
        Left err -> Left err
        Right (iexprs, itable) ->
          parse next prog ((word, iexprs):itable) 
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
                 Just _ -> parse rest ((Word token) : prog) table
                 Nothing -> Left (err, prog, table)

parseTokens :: String -> Table -> Either (String, Program, Table) (Program, Table)
parseTokens code reftable =
  case parse (words code) [] reftable of
    Right (prog, table) -> Right ((reverse prog), table)
    Left e -> Left e

stackUnderflow :: Expr -> String
stackUnderflow expr = "**stack underflow**: " ++ show expr

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
    Word key ->
      case lookup key table of
        Just defs -> eval (Evaluator ((reverse defs) ++ rest) stack table)
        Nothing -> error $ "**undefined word**: " ++ "'" ++ key ++ "'"
  where
    binaryOp f =
      case stack of
        (y:x:xs) -> eval (Evaluator rest (f x y : xs) table)
        _ -> error $ "**stack underflow binary operator**: " ++ show expr
  
interpreter :: Evaluator -> IO ()
interpreter state@(Evaluator _ estack etable) =
  do
    line <- getLine
    case parseTokens line etable of
      Left (err, program, table) -> do
        putStrLn $ "program: " ++ show program
        putStrLn $ "table: " ++ show table
        putStrLn $ "error: " ++ err
        interpreter state
      Right (iprog, itable) -> do
        evaluated <- eval (Evaluator iprog estack itable)
        putStrLn "  ok"
        interpreter evaluated

main :: IO ()
main = interpreter (Evaluator [] [] [])

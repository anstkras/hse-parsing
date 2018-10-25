module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | APow AST AST
         | AUnMinus AST
         | AAssign String AST
         | ANum Integer
         | AIdent String
         | ACode AST AST -- ACode expression code


-- TODO: Rewrite this without using Success and Error
parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _ ->  Just $ map' fst $ (code >>=- \code -> empty |>- return code) input

code :: Parser AST
code =
  (spaces |>- expression) >>=- \expr ->
  ( ( semicolon |>- code >>=- \code -> return (ACode expr code) )
  <|> return expr
  )

expression :: Parser AST
expression =
  ( identifier >>=- \(AIdent i) ->
    assignment |>-
    expression >>=- \e -> return (AAssign i e)
  )
  <|> ( term       >>=- \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>=- \op ->
        expression >>=- \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  factor >>=- \l ->
  ( ( divMult >>=- \op ->
      term    >>=- \r  -> return (AProd op l r)
    )
    <|> return l
  )

factor :: Parser AST
factor =
  final >>=- \l ->
  ( ( pow >>=- \_ ->
    factor >>=- \r -> return (APow l r)
    )
    <|> return l
  )


final :: Parser AST
final =
  ( minus >>=- \_ ->
    final >>=- \fin -> return (AUnMinus fin)
  )
  <|>
  ( lparen |>-
    expression >>=- \e ->
    rparen |>- return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number

digit :: Parser Char
digit = sat T.isDigit elem

number :: Parser AST
number = map (ANum . read) (many1 digit)

alpha :: Parser Char
alpha = sat T.isAlpha elem

identifier :: Parser AST
identifier =  map (AIdent) (many1 alpha)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

semicolon :: Parser Char
semicolon = char ';'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

minus :: Parser T.Operator
minus = map T.operator (char '-')

pow :: Parser T.Operator
pow = map T.operator (char '^')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')




instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> show i
                  AUnMinus e   -> showOp T.Minus : "\n" ++ show' (ident n) e
                  APow l r     -> showOp T.Pow : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  ACode expr code -> show expr ++ "\n" ++  show code)
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Pow   = '^'
module Level2StatetAst where

import Control.Applicative
import Control.Monad.State
import Data.Char
import System.IO

-------------------------------------------------------------------------------
-- AST
-------------------------------------------------------------------------------

data Expr
    = EVal Double
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EPow Expr Expr
    | EFac Expr 
    deriving (Show)

eval :: Expr -> Double
eval (EVal x) = x
eval (EAdd e1 e2) = eval e1 + eval e2
eval (ESub e1 e2) = eval e1 - eval e2
eval (EMul e1 e2) = eval e1 * eval e2
eval (EDiv e1 e2) = eval e1 / eval e2
eval (EPow e1 e2) = eval e1 ** eval e2
eval (EFac e1) = fact $ eval e1

evalLisp :: Expr -> String
evalLisp (EVal x) = show x
evalLisp (EAdd e1 e2) = "(+ " ++ evalLisp e1 ++ " " ++ evalLisp e2 ++ ")"
evalLisp (ESub e1 e2) = "(- " ++ evalLisp e1 ++ " " ++ evalLisp e2 ++ ")"
evalLisp (EMul e1 e2) = "(* " ++ evalLisp e1 ++ " " ++ evalLisp e2 ++ ")"
evalLisp (EDiv e1 e2) = "(/ " ++ evalLisp e1 ++ " " ++ evalLisp e2 ++ ")"
evalLisp (EPow e1 e2) = "(^ " ++ evalLisp e1 ++ " " ++ evalLisp e2 ++ ")"
evalLisp (EFac e1) = "(! " ++ evalLisp e1 ++ ")"

-------------------------------------------------------------------------------
-- type parser
-------------------------------------------------------------------------------

type Parser = StateT String Maybe

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

itemP :: Parser Char
itemP = do
    c:cs <- get
    put cs
    return c

-------------------------------------------------------------------------------
-- calculatrice
-------------------------------------------------------------------------------

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP p = do
    x <- itemP
    if p x then return x else empty

charP :: Char -> Parser Char
charP c = satisfyP (==c)

spacesP :: Parser String
spacesP = many (charP ' ')

numberP :: Parser Expr
numberP = do
    let digitsP = some (satisfyP isDigit)
    s <- charP '-' <|> return ' '
    x <- digitsP
    y <- (charP '.' *> digitsP) <|> return "0"
    return $ EVal$ read $ [s] ++ x ++ "." ++ y

chainLeftP :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeftP parseE parseOp = 
    let go x = (do f <- parseOp
                   y <- parseE
                   go (f x y))
               <|> return x
    in parseE >>= go

infixP :: Char -> a -> Parser a
infixP x op = op <$ charP x

additiveP :: Parser Expr
additiveP = chainLeftP multitiveP (infixP '+' EAdd <|> infixP '-' ESub)

multitiveP :: Parser Expr
multitiveP = chainLeftP powerP (infixP '*' EMul <|> infixP '/' EDiv)

powerP :: Parser Expr
powerP = (EPow <$> factP <* charP '^' <*> powerP) <|> factP

factP :: Parser Expr
factP = (EFac <$> parensP <* charP '!') <|> parensP

parensP :: Parser Expr
parensP 
    =   (spacesP *> charP '(' *> spacesP *> additiveP <* spacesP <* charP ')' <* spacesP)
    <|> spacesP *> numberP <* spacesP

fact :: Double -> Double
fact n = fromIntegral $ product [1 .. (truncate n :: Integer)]

main :: IO ()
main = do
    putStr "\n> "
    hFlush stdout
    input <- getLine
    when (input /= "") $ do
        let exprM = runParser additiveP input
        print exprM
        case exprM of
            Nothing -> putStrLn "no parse"
            Just (expr, _) -> do
                print $ eval expr
                print $ evalLisp expr
        main



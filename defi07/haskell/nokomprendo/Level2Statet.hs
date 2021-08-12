-------------------------------------------------------------------------------
-- Calculatrice infix, avec combinateurs de parseurs implémentés par un
-- StateT (transformateur de monades).
--
-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
-------------------------------------------------------------------------------

module Level2Statet where

import Control.Applicative
import Control.Monad.State
import Data.Char
import System.IO

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

numberP :: Parser Double
numberP = do
    let digitsP = some (satisfyP isDigit)
    s <- charP '-' <|> return ' '
    x <- digitsP
    y <- (charP '.' *> digitsP) <|> return "0"
    return $ read $ [s] ++ x ++ "." ++ y

chainLeftP :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeftP parseE parseOp = 
    let go x = (do f <- parseOp
                   y <- parseE
                   go (f x y))
               <|> return x
    in parseE >>= go

infixP :: Char -> (a -> a -> a) -> Parser (a -> a -> a)
infixP x op = op <$ charP x

additiveP :: Parser Double
additiveP = chainLeftP multitiveP (infixP '+' (+) <|> infixP '-' (-))

multitiveP :: Parser Double
multitiveP = chainLeftP powerP (infixP '*' (*) <|> infixP '/' (/))

powerP :: Parser Double
powerP = ((**) <$> factP <* charP '^' <*> powerP) <|> factP

factP :: Parser Double
factP = (fact <$> parensP <* charP '!') <|> parensP

parensP :: Parser Double
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
        print (runParser additiveP input)
        main


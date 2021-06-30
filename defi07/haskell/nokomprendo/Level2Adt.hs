-------------------------------------------------------------------------------
-- Calculatrice infix, avec combinateurs de parseurs implémentés par des types
-- agébriques (ADT) + instances (Functor, Applicative, Monad, Altermative).
-------------------------------------------------------------------------------

module Level2Adt where

import Control.Applicative
import Control.Monad
import Data.Char
import System.IO

-------------------------------------------------------------------------------
-- type parser
-------------------------------------------------------------------------------

type Result v = Maybe (v, String)

newtype Parser v = Parser { runParser :: String -> Result v }

itemP :: Parser Char 
itemP = 
    let f "" = Nothing
        f (x:xs) = Just (x, xs)
    in Parser f

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance Functor Parser where

    -- (a -> b) -> f a -> f b
    fmap f (Parser p) = 
        Parser $ \d0 -> do
            (x, d1) <- p d0
            return (f x, d1)

instance Applicative Parser where

    -- a -> f a
    pure x = Parser $ \d -> return (x, d)

    -- f (a -> b) -> f a -> f b
    (Parser p1) <*> (Parser p2) =
        Parser $ \d0 -> do
            (f, d1) <- p1 d0
            (a, d2) <- p2 d1
            return (f a, d2)

instance Monad Parser where

    -- m a -> (a -> m b) -> m b
    (Parser p1) >>= f2 = 
        Parser $ \d0 -> do
            (a, d1) <- p1 d0
            (b, d2) <- runParser (f2 a) d1
            return (b, d2)

instance Alternative Parser where

    -- empty :: f a 
    empty = Parser $ const empty

    -- (<|>) :: f a -> f a -> f a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

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


{-# LANGUAGE DeriveFunctor #-}

module Main where

import           Control.Monad.Free
import           Data.Char
import qualified Data.Vector.Unboxed    as V
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.String

type Program = Free Brainfuck ()

data Brainfuck next
  = IncPtr next
  | DecPtr next
  | IncByte next
  | DecByte next
  | ByteOut next
  | ByteIn next
  | Loop Program next
  deriving (Functor, Show)

incPtr :: Program
incPtr = liftF $ IncPtr ()

decPtr :: Program
decPtr = liftF $ DecPtr ()

incByte :: Program
incByte = liftF $ IncByte ()

decByte :: Program
decByte = liftF $ DecByte ()

byteOut :: Program
byteOut = liftF $ ByteOut ()

byteIn :: Program
byteIn = liftF $ ByteIn ()

loop :: Program -> Program
loop body = liftF $ Loop body ()

usedChars :: String
usedChars = "><+-.,[]"

parseProgram :: Parser Program
parseProgram =
  (>>) <$> parseOneInstr
       <*> parseProgram <|> pure (Pure ())

parseOneInstr :: Parser Program
parseOneInstr = skip *> parseBf
  where
    skip = skipMany $ noneOf usedChars
    parseBf =
          incPtr <$ char '>'
      <|> decPtr <$ char '<'
      <|> incByte <$ char '+'
      <|> decByte <$ char '-'
      <|> byteOut <$ char '.'
      <|> byteIn <$ char ','
      <|> loop <$> between (char '[') (char ']') parseProgram

-- hello world
simpleExampleStr :: String
simpleExampleStr = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

runBrainfuck :: Program -> (Int, Int, V.Vector Int) -> IO ()
runBrainfuck (Free (ByteOut next)) st@(_, ptr, mem) =
    putChar (chr $ mem V.! ptr) >> hFlush stdout >> runBrainfuck next st
runBrainfuck (Free (ByteIn next)) (n, ptr, mem) =
    getChar >>= \c -> runBrainfuck next (n, ptr, mem V.// [(ptr, ord c)])
runBrainfuck (Free (IncPtr next)) (n, ptr, mem) = runBrainfuck next
    (n, min (n-1) (ptr+1), mem)
runBrainfuck (Free (DecPtr next)) (n, ptr, mem) = runBrainfuck next
    (n, max 0 (ptr-1), mem)
runBrainfuck (Free (IncByte next)) (n, ptr, mem) = runBrainfuck next
    (n, ptr, mem V.// [(ptr, (mem V.! ptr) + 1)])
runBrainfuck (Free (DecByte next)) (n, ptr, mem) = runBrainfuck next
    (n, ptr, mem V.// [(ptr, (mem V.! ptr) - 1)])
runBrainfuck lp@(Free (Loop body next)) st@(_, ptr, mem) =
    if mem V.! ptr == 0
        then runBrainfuck next st
        else runBrainfuck (body >> lp) st
runBrainfuck (Pure _) _ = return ()

-- todo read bf from file or stdin
main :: IO ()
main =
  case parse parseProgram "" simpleExampleStr of
    Right prg -> runBrainfuck prg (3000, 0, V.replicate 3000 0)
    Left  err -> print err

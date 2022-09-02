import qualified Data.ByteString as B
import Data.Word
import Data.Char
import Data.Maybe (catMaybes)
import qualified Data.Sequence as S
import Control.Monad.State

type Pointer = Int
type Code = String
type Tape = S.Seq Word8

data Command = IncPointer | DecPointer | IncByte | DecByte | Output | Input | JmpFwd | JmpBwd deriving (Eq,Show)

data Program = Program {
  code :: [Command],
  pointer :: Pointer,
  tape :: Tape
}


initializeProgram :: String -> Program 
initializeProgram s = Program (parseCode s) 0 (S.fromList $ take 30000 (repeat 0))

parseCommand :: Char -> Maybe Command
parseCommand c 
  | c == '>' = Just IncPointer
  | c == '<' = Just DecPointer
  | c == '+' = Just IncByte 
  | c == '-' = Just DecByte 
  | c == '.' = Just Output 
  | c == ',' = Just Input 
  | c == '[' = Just JmpFwd
  | c == ']' = Just JmpBwd
  | otherwise = Nothing

parseCode :: String -> [Command]
parseCode code = catMaybes $ parseAllCommands code
  where 
    parseAllCommands [] = []
    parseAllCommands (c:rest) = parseCommand c : parseAllCommands rest

wordToInt :: Word8 -> Int
wordToInt = fromIntegral

charToWord :: Char -> Word8
charToWord = fromIntegral . ord

runProgram :: Program -> IO Program
runProgram prog@(Program [] p t) = return prog
runProgram prog@(Program (c:cs) p t) =
  case c of 
    IncPointer -> runProgram (Program cs (p+1) t)
    DecPointer -> runProgram (Program cs (p-1) t)
    IncByte -> runProgram (Program cs p (S.adjust succ p t))
    DecByte -> runProgram (Program cs p (S.adjust pred p t))
    Output -> putChar (chr $ wordToInt (t `S.index` p)) >> runProgram (Program cs p t)
    Input -> do 
      c <- getChar
      runProgram (Program cs p (S.insertAt p (charToWord c) t))
    _ -> putStrLn "Unknown Command" >> runProgram (Program cs p t)
    
main :: IO ()
main = do
  code <- getLine
  let prog = initializeProgram code

  return ()
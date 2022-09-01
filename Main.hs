import qualified Data.ByteString as B
import Data.Word
import qualified Data.Sequence as S

type Pointer = Int
type Code = String
type Tape = S.Seq Word8
data Program = Program {
  code :: Code,
  pointer :: Pointer,
  tape :: Tape
}

data Command = IncPointer | DecPointer | IncByte Pointer | DecByte Pointer | Output Pointer | Input Pointer deriving (Show)

initializeProgram :: String -> Program 
initializeProgram s = Program s 0 (S.fromList $ take 30000 (repeat 0))


parseCommand :: Pointer -> Char -> Maybe Command
parseCommand p c 
  | c == '>' = Just IncPointer
  | c == '<' = Just DecPointer
  | c == '+' = Just $ IncByte p
  | c == '-' = Just $ DecByte p
  | c == '.' = Just $ Output p
  | c == ',' = Just $ Input p
  |otherwise = Nothing

main :: IO ()
main = do
  code <- getLine
  let prog = initializeProgram code
  return ()
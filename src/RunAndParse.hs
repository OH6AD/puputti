module RunAndParse (runAndParse) where

import System.Process
import Data.Attoparsec.ByteString.Char8 (eitherResult, parseWith)
import Data.ByteString (hGetSome, empty)

runAndParse parser cmd args = do
  (_,Just hIn,_,hProc) <- createProcess (proc cmd args){std_out = CreatePipe}
  out <- parseWith (hGetSome hIn 1024) parser empty
  waitForProcess hProc
  return $ eitherResult out

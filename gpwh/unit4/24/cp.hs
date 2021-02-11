import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  args <- getArgs
  let orgFile = head args 
  let cpFile = args !! 1
  input <- TI.readFile orgFile
  TI.writeFile cpFile input
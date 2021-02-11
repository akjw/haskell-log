import System.IO
import System.Environment (getArgs)

-- openFile :: FilePath -> IOMode -> IO Handle

-- type FilePath = String

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- openFile "stuff.txt" ReadMode

main1 :: IO ()
main1 = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine <- hIsEOF helloFile
  firstLine <-  if not hasLine
                then hGetLine helloFile -- getLineis hGetLine where the handle is stidn.
                else return "empty"
  putStrLn firstLine
  hasSndLine <- hIsEOF helloFile
  secondLine <- if not hasSndLine
                then hGetLine helloFile
                else return ""
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine -- putStrLn = instance of hPutStrLn; hPutStrLn handle assumed to be stdout. 
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"

-- readFile, writeFile, and appendFile hide 
-- many details of reading, writing, and appending files
-- -> don't have to deal w/ file handles directly

-- readFile :: FilePath -> IO String
  -- readFile doesn’t close the file handle
-- writeFile :: FilePath -> String -> IO ()
-- appendFile :: FilePath -> String -> IO ()

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   input <- readFile fileName
--   let summary = (countsText . getCounts) input
--   appendFile "stats.dat" (mconcat [fileName, " ",summary, "\n"])
--   putStrLn summary

getCounts :: String -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int,Int,Int) -> String
countsText (cc,wc,lc) = unwords ["chars: " -- unwords is preferable to (++) since it also handles Text 
                                , show cc
                                , " words: "
                                , show wc
                                , " lines: "
                                , show lc]


-- Revised main with expanded readFile function: (broken bc of lazy evaluation)
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   file <- openFile fileName ReadMode
--   input <- hGetContents file 
      -- * input not used until summary is defined
--   hClose file 
      -- * closes the file immediately because it is an IO action
--   let summary = (countsText . getCounts) input 
      -- *  summary not used until appendFile called 
--   appendFile "stats.dat" (mconcat [fileName, " ",summary, "\n"]) 
      -- *  forces evaluation of appendFile & summary; impossible due to hClose
--   putStrLn summary


main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary -- forces summary to be evaluated; input must be read to be used by summary.
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ",summary, "\n"])

-- strict version: see strict.hs

-- When to use lazy vs. strict
-- simple program (e.g. reading 1 file, negligible I/0 actions) = lazy
-- more complex = stick to strict
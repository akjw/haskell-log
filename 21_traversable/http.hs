{-HLINT Ignore -}

module HttpStuff where

import Data.ByteString.Lazy hiding (map)
-- import Network.Wreq

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query -- a :: [String]
  traverse makeIoOnlyObj 
    (mapM decodeFn a) 
    -- 1) decodeFn :: String -> Either Err SomeObj
    -- 2) mapM decodeFn a 
      -- :: (String -> Either Err SomeObj) -> [String] -> Either Err [SomeObj]
    -- 3) traverse makeIoOnlyObj (mapM decodeFn a)
      -- :: ([SomeObj] -> IO [(SomeObj, IoOnlyObj)])
      -- -> Either Err [SomeObj] -> IO (Either Err [(SomeObj, IoOnlyObj)])

urls :: [String]
urls = [ "http://httpbin.org/ip"
        , "http://httpbin.org/bytes/5"
        ]

-- mappingGet :: [IO (Response ByteString)]
-- mappingGet = map get urls

-- traversedUrls :: IO [Response ByteString]
-- traversedUrls = traverse get urls
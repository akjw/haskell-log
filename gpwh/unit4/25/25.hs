{-# LANGUAGE OverloadedStrings #-}
-- allows string literals to be assigned T.Text type

import qualified Data.ByteString as B
-- import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC

-- tatsuhikoTakimoto :: T.Text
-- tatsuhikoTakimoto = "⁪⁪滝本 竜彦"

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

-- sampleInt :: BC.ByteString  -> Int
-- sampleInt = read . BC.unpack

sampleInt :: Int
sampleInt = (read . BC.unpack) bcInt



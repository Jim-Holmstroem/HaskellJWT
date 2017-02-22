{-# LANGUAGE OverloadedStrings #-}
module Lib (
    someFunc,
    header_payload,
) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString (ByteString(..))
import qualified Data.Char
import qualified Data.ByteArray as BA
import           Data.ByteArray (ByteArray(..))

import Crypto.Hash
import Crypto.MAC.HMAC

import qualified Data.ByteString.Base64 as Base64


secret :: ByteString
secret = "secret"

getHmac :: ByteString -> HMAC SHA256
getHmac = hmac secret

equalSign = fromIntegral . Data.Char.ord $ '='

encode = BS.takeWhile (/= equalSign) . Base64.encode

header_payload message = BS.concat [ encode "{\"alg\":\"HS256\"}"
                                   , "."
                                   , encode message
                                   ]

encoded message = BS.concat [ hashee
                            , "."
                            , BSC.pack . show . hmacGetDigest $ (hmac secret hashee :: HMAC SHA256)
                            ]
    where hashee = header_payload message


someFunc :: IO ()
someFunc = print $ encoded "{}"

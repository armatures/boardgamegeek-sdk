{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ResponseParser (parse) where

import           Import
import qualified RIO.ByteString.Lazy as BL
import           Text.XML

parse :: BL.ByteString -> Either SomeException Document
parse = parseLBS def

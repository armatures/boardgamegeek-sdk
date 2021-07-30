{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ResponseParser where

import           Import
import qualified RIO.ByteString.Lazy as BL
import           RIO.List
import           RIO.List.Partial    (head)
import           RIO.Partial         (read)
import qualified RIO.Text            as T
import           Text.XML
import           Text.XML.Cursor

parseMarketplace :: BL.ByteString -> Either SomeException MarketplaceResponse
parseMarketplace xml =
   do
   cursor <- fromDocument <$> parseLBS def xml
   let
     item :: [Cursor] = child >=> -- into "items"
                        child $ -- into "item"
                        cursor
     name = T.pack . trimQuotes . show . head $ attribute "value" =<< foldMap (laxElement "name") item
     yearPublished = read . trimQuotes . show . head $ attribute "value" =<< foldMap (laxElement "yearpublished") item

   pure $ MarketplaceResponse { marketplaceName = name
                              , marketplaceYearPublished = yearPublished}

data MarketplaceResponse =
  MarketplaceResponse { marketplaceName          :: Text
                      , marketplaceYearPublished :: Int
                      }

trimQuotes :: String -> String
trimQuotes s' =
  let
    removeLast s = if lastMaybe s == Just '\"' then initMaybe s else Just s
    removeFirst s = if headMaybe s == Just '\"' then tailMaybe s else Just s
  in
    fromMaybe "" $ removeLast =<< removeFirst s'

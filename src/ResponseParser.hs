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
     itemId = ItemId <$> read . trimQuotes . show . head $ foldMap (laxAttribute "id") (child cursor)
     name = T.pack . trimQuotes . show . head $ attribute "value" =<< foldMap (laxElement "name") item
     yearPublished = read . trimQuotes . show . head $ attribute "value" =<< foldMap (laxElement "yearpublished") item
   pure $ MarketplaceResponse
     { marketplaceType = Boardgame
     , marketplaceId = itemId
     , marketplaceName = name
     , marketplaceYearPublished = yearPublished}

data MarketplaceResponse =
  MarketplaceResponse
    { marketplaceType          :: ItemType
    , marketplaceId            :: ItemId
    , marketplaceName          :: Text
    , marketplaceYearPublished :: Int
    }
  deriving (Show, Eq)

data ItemType = Boardgame
  deriving (Show, Eq)

newtype ItemId = ItemId Int
  deriving (Read, Show, Eq)

trimQuotes :: String -> String
trimQuotes s' =
  let
    removeLast s = if lastMaybe s == Just '\"' then initMaybe s else Just s
    removeFirst s = if headMaybe s == Just '\"' then tailMaybe s else Just s
  in
    fromMaybe "" $ removeLast =<< removeFirst s'

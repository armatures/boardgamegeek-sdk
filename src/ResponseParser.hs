{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ResponseParser (MarketplaceResponse(..), parseMarketplace) where

import           Import
import qualified RIO.ByteString.Lazy as BL
import           RIO.List.Partial    (head)
import qualified RIO.Text            as T
import           Text.XML
import           Text.XML.Cursor

parseMarketplace :: BL.ByteString -> Either SomeException MarketplaceResponse
parseMarketplace xml =
 -- let
   -- cursor :: Either SomeException Cursor
   -- cursor = fromDocument <$> parseLBS def xml
 -- in
   do
   cursor <- fromDocument <$> parseLBS def xml
   let
     nameNode :: [Cursor] = child >=> -- into "items"
       child >=> -- into "item"
       laxElement "name" $ cursor
     c = attribute "value" =<< nameNode
   pure $ MarketplaceResponse . T.pack . show . head $ c
   -- pure $ MarketplaceResponse $ T.pack $ show $ (foldMap content c)

data MarketplaceResponse =
  MarketplaceResponse {marketplaceName :: Text}

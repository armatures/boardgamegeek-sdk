{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run (run) where

import           Import
import           Network.HTTP.Simple


-- searchRequest = parseRequest "https://www.boardgamegeek.com/xmlapi2/search?query=paper+tales&type=boardgame&exact=1"
-- it might be nice to translate all the stuff from the BGG docs into actual types.

marketplaceRequest :: MonadThrow m => m Request
marketplaceRequest =
  let
  url = "https://www.boardgamegeek.com/xmlapi2/thing"
  query :: Query
  query =
    [ ("id", Just "217861" )
    ,  ("marketplace", Just "1")
    ]
  in
  setRequestQueryString query <$> parseRequest url

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  request <- marketplaceRequest
  logInfo $ "request: " <> displayShow request
  response :: Response ByteString <- httpBS request
  let body = getResponseBody response
  logInfo $ "body: " <> displayShow body


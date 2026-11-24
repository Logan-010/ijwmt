{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.BitTorrent.WebSeed(fetchFile) where

import Control.Exception (try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import Network.HTTP.Client (HttpException, Manager, Request (decompress, requestHeaders), Response (responseBody, responseHeaders, responseStatus), httpLbs, parseRequest)
import Network.HTTP.Types (status206)
import Network.URL (URL (url_path), exportURL)

resolveFile :: URL -> Bool -> String -> URL
resolveFile url multiFile file
  | multiFile || "/" `isSuffixOf` path =
      url {url_path = prefix ++ file}
  | otherwise = url
  where
    path = url_path url
    prefix
      | null path = ""
      | "/" `isSuffixOf` path = path
      | otherwise = path ++ "/"

fetch ::
  Manager ->
  URL ->
  Integer ->
  Int ->
  IO (Maybe ByteString)
fetch manager url offset len
  | offset < 0 || len <= 0 = pure Nothing
  | otherwise = do
      result <- try $ do
        initReq <- parseRequest $ exportURL url

        let end = offset + fromIntegral len - 1
            range =
              CBS.pack $
                "bytes=" ++ show offset ++ "-" ++ show end

            req =
              initReq
                { requestHeaders =
                    [ ("Range", range),
                      ("Accept-Encoding", "identity")
                    ]
                      ++ requestHeaders initReq,
                  decompress = const False
                }

        res <- httpLbs req manager

        let body = LBS.toStrict $ responseBody res
            expectedRange =
              CBS.pack $
                "bytes " ++ show offset ++ "-" ++ show end ++ "/"

            validRange =
              maybe
                False
                (BS.isPrefixOf expectedRange)
                (lookup "Content-Range" $ responseHeaders res)

        pure $
          if responseStatus res == status206
            && BS.length body == len
            && validRange
            then Just body
            else Nothing

      case result of
        Left (_ :: HttpException) -> pure Nothing
        Right body -> pure body

fetchFile ::
  Manager ->
  URL ->
  Bool ->
  String ->
  Integer ->
  Int ->
  IO (Maybe ByteString)
fetchFile manager url multiFile file offset len =
  fetch manager (resolveFile url multiFile file) offset len

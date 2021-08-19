module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, jsonEmptyString, stringify, (.!=), (.:), (.:?))
import Data.Argonaut.Core (stringify, fromString)
import Data.Argonaut.Decode.Error(printJsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.HTTP.Method (Method(..))
import Data.Int (decimal, toStringAs)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff, Aff)
import Effect.Class.Console (log)

data NewsItem = 
  NewsItem { id :: Int
  , by :: String
  , title :: String
  -- , url :: String
  }

instance decodeJsonNewsItem :: DecodeJson NewsItem where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    by <- obj .: "by"
    title <- obj .: "title"
    pure $ NewsItem { id, by, title }

decodeNewsItem :: Json -> Either JsonDecodeError NewsItem
decodeNewsItem = decodeJson


hnAddress :: String
hnAddress = "https://hacker-news.firebaseio.com/v0"

decodeNews :: Json -> Array Int
decodeNews json = 
  case decodeJson json of
    Left _ -> []
    Right array -> array

fetchArticle :: Int -> Aff Unit
fetchArticle id = do
  result <- AX.request (AX.defaultRequest { url = hnAddress <> "/item/" <> (toStringAs decimal id) <> ".json?print=pretty", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> do
      case decodeNewsItem response.body of
        Left err -> log $ printJsonDecodeError err
        Right (NewsItem { title }) -> log $ title <> "\r\n"

doShit :: Int -> Aff Unit
doShit id = fetchArticle id
-- doShit id = log $ toStringAs decimal id
-- doShit = log <<< toStringAs decimal

main :: Effect Unit
main = void $ launchAff $ do
  result <- AX.request (AX.defaultRequest { url = hnAddress <> "/askstories.json?print=pretty", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "Didn't workout 🤷🏻‍♂️" <> AX.printError err
    Right response -> do
      let news :: Array Int
          news = decodeNews response.body
      for_ news doShit
      -- traverse (\id -> fetchArticle id) news
      -- traverse (\id -> log $ (toStringAs decimal id)) news


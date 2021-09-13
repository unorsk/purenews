module NewsFetcher where

import Prelude

import Effect.Class (liftEffect)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Array (take)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method(..))
import Data.Int (decimal, toStringAs)
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

fetchArticleWithCallback :: (String -> Effect Unit) -> Int -> Aff Unit
fetchArticleWithCallback callback id = do
  result <- AX.request (AX.defaultRequest { url = hnAddress <> "/item/" <> (toStringAs decimal id) <> ".json?print=pretty", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> do
      case decodeNewsItem response.body of
        Left err -> log $ printJsonDecodeError err
        Right (NewsItem { title }) -> liftEffect $ callback $ title --log $ "hello"

fetchArticle :: Int -> Aff Unit
fetchArticle id = do
  result <- AX.request (AX.defaultRequest { url = hnAddress <> "/item/" <> (toStringAs decimal id) <> ".json?print=pretty", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> do
      case decodeNewsItem response.body of
        Left err -> log $ printJsonDecodeError err
        Right (NewsItem { title }) -> log $ withGraphics
          (foreground Yellow)
          title <> "\r\n"

loadAndAppend :: Int -> (String -> Effect Unit) -> Effect Unit
loadAndAppend maxElements callback = void $ launchAff $ do
  result <- AX.request (AX.defaultRequest { url = hnAddress <> "/topstories.json?print=pretty", responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "Didn't workout 🤷🏻‍♂️" <> AX.printError err
    Right response -> do
      let news :: Array Int
          news = take maxElements $ decodeNews response.body
      traverse_ (fetchArticleWithCallback callback) news


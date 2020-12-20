module Vocabulary.Slug exposing (Slug, toString, urlParser, decoder, buildSlug, encode)

import Url exposing (percentDecode)
import Url.Parser exposing (Parser, custom)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Slug 
  = Slug String


urlParser : Parser (Slug -> a) a
urlParser =
  custom "Slug" (\str -> Maybe.map Slug <| percentDecode str)

toString : Slug -> String
toString (Slug str) =
  str

-- for searchbox only
buildSlug : String -> Slug
buildSlug str =
  Slug str

decoder : Decoder String -> Decoder Slug
decoder modifiedString =
    Decode.map Slug modifiedString

encode : Slug -> Value
encode slug =
  Encode.string (toString slug)
module Vocabulary exposing (Slug, toString, urlParser)

import Url exposing (percentDecode)
import Url.Parser exposing (Parser, custom)

type Slug 
  = Slug String


urlParser : Parser (Slug -> a) a
urlParser =
  custom "Slug" (\str -> Maybe.map Slug <| percentDecode str)

toString : Slug -> String
toString (Slug str) =
  str
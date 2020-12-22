module Vocabulary.Id exposing (Id, toString, urlParser, decoder, isEq)

import Url.Parser.Query as Query exposing (Parser, map)
import Json.Decode as Decode exposing (Decoder, string)

type Id
    = Id String


toString : Id -> String
toString (Id s) =
    s


urlParser : Parser (Maybe Id)
urlParser =
    map (Maybe.map Id) (Query.string "id")


decoder : Decoder Id
decoder =
  Decode.map Id string

isEq : Id -> Id -> Bool
isEq id1 id2 =
    toString id1 == toString id2
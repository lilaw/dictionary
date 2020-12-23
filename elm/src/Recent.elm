module Recent exposing (Recent, LastWord, emptyList, add, remove, toList, encode, decoder)

-- import Vocabulary exposing (Word)

import Json.Decode as Decode exposing (Decoder, field, list, string)
import Json.Encode as Encode exposing (Value)
import Vocabulary.Id as Id exposing (Id)
import Vocabulary.Slug as Slug exposing (Slug)


type Recent
    = Recent (List LastWord)


type alias LastWord =
    { slug : Slug
    , id : Id
    , headword : String
    }



-- create
emptyList : Recent
emptyList =
    Recent []



-- transform


add : Recent -> LastWord -> Recent
add recent word =
    let
        refreshRecent =
            remove recent word.id 
    in
    case refreshRecent of
        Recent rec ->
            Recent 
              <| List.take 24 (word :: rec)


remove : Recent -> Id -> Recent
remove (Recent info) id =
    Recent <|
        List.filter (\l -> not <| Id.isEq id l.id) info


toList : Recent -> List LastWord
toList (Recent info) = 
  info


encode : Recent -> Value
encode (Recent info) =
    let
        config f =
            Encode.object
                [ ( "slug", Slug.encode f.slug )
                , ( "id", Encode.string <| Id.toString f.id )
                , ( "headword", Encode.string f.headword )
                ]
    in
    Encode.list config info


decoder : Decoder Recent
decoder =
    let
        configDecoder =
            Decode.map3 LastWord
                (field "slug" (Slug.decoder Decode.string))
                (field "id" Id.decoder)
                (field "headword" string)
    in
    Decode.list configDecoder
        |> Decode.map Recent

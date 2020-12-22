module Favorites exposing (Favorites, Favored, decoder, encode, add, remove, emptyList, isFavorited, length, toList)

import Json.Decode as Decode exposing (Decoder, field, string, list)
import Json.Encode as Encode exposing (Value)
import Vocabulary.Slug as Slug exposing (Slug)
import Vocabulary.Id as Id exposing (Id)

type Favorites 
  = Favorites (List Favored)

type alias Favored =
    { slug : Slug
    , id : Id
    , headword : String
    , shortDefine : List String
    } 

-- create 


emptyList : Favorites
emptyList = 
  Favorites []


-- TRANSFORM

length : Favorites -> Int
length (Favorites info) =
  List.length info

add : Favored -> Favorites -> Favorites
add favored (Favorites listFav) =
  Favorites ( favored :: listFav)

remove : Favored -> Favorites -> Favorites
remove favored (Favorites listFav) =
  Favorites (List.filter (\s -> s.headword /= favored.headword) listFav)

isFavorited : String -> Favorites -> Bool
isFavorited headword (Favorites listFav) =
  List.any (\f -> f.headword == headword) listFav

toList : Favorites -> List Favored
toList (Favorites info) =
  info

-- SERIALIZATION
encode : Favorites -> Value
encode (Favorites listFav) =
  let
      favoredObj f = 
        Encode.object 
          [ ("slug", Slug.encode f.slug)
          , ("id", Encode.string <| Id.toString f.id)
          , ("headword", Encode.string f.headword)
          , ("shortDefine", Encode.list Encode.string f.shortDefine)
          ]
  in
  Encode.list favoredObj listFav
    

decoder : Decoder Favorites
decoder =
  let
      configDecoder 
        = Decode.map4 Favored
          (field "slug" (Slug.decoder Decode.string))
          (field "id" Id.decoder)
          (field "headword" string)
          (field "shortDefine" (list string))
  in
  Decode.list configDecoder
    |> Decode.map Favorites
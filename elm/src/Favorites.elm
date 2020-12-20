module Favorites exposing (Favorites, decoder, encode, add, remove, emptyList, isFavorited)

import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode exposing (Value)
import Vocabulary.Slug as Slug exposing (Slug)

type Favorites 
  = Favorites (List Favored)

type alias Favored =
    { slug : Slug
    , headword : String
    } 

-- create 


emptyList : Favorites
emptyList = 
  Favorites []

-- TRANSFORM

add : Slug -> String -> Favorites -> Favorites
add slug headword (Favorites listFav) =
  Favorites ( Favored slug headword :: listFav)

remove : String -> Favorites -> Favorites
remove headword (Favorites listFav) =
  Favorites (List.filter (\s -> s.headword /= headword) listFav)

isFavorited : String -> Favorites -> Bool
isFavorited headword (Favorites listFav) =
  List.any (\f -> f.headword == headword) listFav

-- SERIALIZATION
encode : Favorites -> Value
encode (Favorites listFav) =
  let
      favoredObj f = 
        Encode.object [("slug", Slug.encode f.slug), ("headword", Encode.string f.headword)]
  in
  Encode.list favoredObj listFav
    

decoder : Decoder Favorites
decoder =
  let
      configDecoder 
        = Decode.map2 Favored
          (field "slug" (Slug.decoder Decode.string))
          (field "headword" string)
  in
  Decode.list configDecoder
    |> Decode.map Favorites
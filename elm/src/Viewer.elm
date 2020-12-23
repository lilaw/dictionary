module Viewer exposing (Viewer, decoder, defaultViewer, encode, favorites, updateFavor, store, favorite, unfavorite, updateRecent, recent)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Decode.Pipeline exposing ( required)
import Api
import Favorites exposing (Favorites, Favored)
import Recent exposing (Recent)

--model
type Viewer 
  = Viewer Internals
  
type alias Internals =
    { favorites : Favorites
    , recent : Recent
    }
  
-- info 
favorites : Viewer -> Favorites
favorites (Viewer info) =
  info.favorites

recent : Viewer -> Recent
recent (Viewer info) =
  info.recent


-- create
defaultViewer: Viewer
defaultViewer = Viewer { favorites = Favorites.emptyList, recent =  Recent.emptyList}

-- UPDATE
updateFavor : Viewer -> Favorites -> Viewer
updateFavor (Viewer info) newFavor =
  Viewer {info | favorites = newFavor}

updateRecent : Viewer -> Recent -> Viewer
updateRecent (Viewer info) newRecent =
  Viewer {info | recent = newRecent}


  -- serialization
decoder : Decoder Viewer
decoder =
  Decode.succeed Internals
    |> required "favorites" Favorites.decoder
    |> required "recent" Recent.decoder
    |> Decode.map Viewer

encode : Viewer -> Value
encode (Viewer info) =
    Encode.object 
        [ ( "favorites", Favorites.encode info.favorites)
        ]
  
store : Viewer -> Cmd msg
store (Viewer info) =
    Api.storeViewerWith
        info.favorites
        info.recent

-- favorite
favorite : Viewer -> Favored -> Cmd msg
favorite viewer fav =
  let
    oldFavor = favorites viewer
  in
    Favorites.add fav oldFavor
      |> updateFavor viewer
      |> store
  

unfavorite : Viewer -> Favored -> Cmd msg
unfavorite viewer fav =
  let
    oldFavor = favorites viewer
  in
    Favorites.remove fav oldFavor
      |> updateFavor viewer
      |> store

-- Recent 

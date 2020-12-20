module Viewer exposing (Viewer, decoder, defaultViewer, encode, favorites, updateFavor, store)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Decode.Pipeline exposing ( required)
import Api
import Favorites exposing (Favorites)

--model
type Viewer 
  = Viewer Internals
  
type alias Internals =
    { favorites : Favorites
    }
  
-- info 
favorites : Viewer -> Favorites
favorites (Viewer info) =
  info.favorites


-- create
defaultViewer: Viewer
defaultViewer = Viewer { favorites = Favorites.emptyList}

-- UPDATE
updateFavor : Viewer -> Favorites -> Viewer
updateFavor (Viewer info) newFavor =
  Viewer {info | favorites = newFavor}
  


  -- serialization
decoder : Decoder Viewer
decoder =
  Decode.succeed Internals
    |> required "favorites" Favorites.decoder
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
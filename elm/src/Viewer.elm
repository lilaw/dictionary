module Viewer exposing (Viewer, decoder, defaultViewer)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing ( required)

--model
type Viewer 
  = Viewer Internals
  
type alias Internals =
    { favorites : String
    
    }

defaultViewer: Viewer
defaultViewer = Viewer { favorites = ""}

  -- serialization
decoder : Decoder Viewer
decoder =
  Decode.succeed Internals
    |> required "favorites" Decode.string
    |> Decode.map Viewer
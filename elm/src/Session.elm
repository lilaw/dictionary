module Session exposing (Session, decode, navKey, changes, viewer)

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder, Value)
import Viewer exposing (Viewer, defaultViewer)
import Api

-- the viewer is the current visitor that has two cases; one has some favorites word or search history in local storage, other is not

-- TYPES
type Session 
    = Guest Nav.Key Viewer


-- INFO
navKey : Session -> Nav.Key
navKey (Guest key _) = key

viewer : Session -> Viewer
viewer (Guest _ view) =
    view

-- CHANGES

changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.onSessionChange (\val -> toMsg (decode key val))


decode : Nav.Key -> Value -> Session
decode key value =
    case Decode.decodeValue Decode.string value
        |> Result.andThen (Decode.decodeString (storageDecoder Viewer.decoder))
    of 
        Ok view ->
          Guest key view
        Err err ->
            let
                e = Debug.log "err" err
            in
            
          Guest key defaultViewer

storageDecoder : Decoder viewer -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" viewerDecoder
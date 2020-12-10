port module Session exposing (Session, decode, navKey)

import Browser.Navigation as Nav
import Viewer exposing (Viewer, defaultViewer)

import Json.Decode as Decode exposing (Decoder, Value)


-- TYPES
type Session 
    = Guest Nav.Key Viewer



-- INFO
navKey : Session -> Nav.Key
navKey (Guest key _) = key


-- LOGIN



-- LOGOUT
clearCache : Cmd msg
clearCache =
    storeSession Nothing

port storeSession : Maybe String -> Cmd msg


-- CHANGES
port onSessionChange : (Value -> msg) -> Sub msg

changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    onSessionChange (\val -> toMsg (decode key val))


decode : Nav.Key -> Value -> Session
decode key value =
    case Decode.decodeValue Decode.string value
        |> Result.andThen (Decode.decodeString Viewer.decoder)
    of 
        Ok viewer ->
          Guest key viewer
        Err _ ->
          Guest key defaultViewer


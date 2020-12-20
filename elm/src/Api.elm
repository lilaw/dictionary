port module Api exposing (url, decodeErrors, addServerError, userReplace, audioUrl, onSessionChange, soundCmdToJs, storeViewerWith)

import Http exposing (Error(..))
import Json.Encode as Encode exposing (Value)

import Json.Decode as Decode exposing (Decoder, Value)
-- import Json.Decode.Pipeline as Pipeline exposing (optional)
import Url.Builder exposing (string)
import Regex
import Favorites exposing (Favorites)



-- URL


{-| Get a URL to the dictionaryapi API-}
url : List String -> String
url paths =
    let
        base = "https://www.dictionaryapi.com/api/v3/references/learners/json/"
        token = "b42bc7db-c7e4-4a8a-bdca-f5a53d907e3f"
        param = [string "key" token]
    in
        Url.Builder.crossOrigin base paths param

-- to etch sond filed
audioUrl : String -> String
audioUrl filename =
    let
        base = " https://media.merriam-webster.com/audio/prons/en/us/mp3/"
        subffix = ".mp3"
        numberOrPunctuation = 
            Maybe.withDefault Regex.never <|
                Regex.fromString "[\\d!@#$%\\^&\\*\\(\\)_\\+\\{\\}:\"\\|<>\\?\\/\\.,;\'\\]\\[]"
        subdirectory = 
            if String.startsWith "bix" filename then
                "bix"
            else if String.startsWith "gg" filename then
                "gg"
            else if Regex.contains numberOrPunctuation (String.slice 0 1 filename) then
                "number"
            else
                String.slice 0 1 filename
    in
      base ++ subdirectory ++ "/" ++ filename ++ subffix      



-- -- ERRORS

addServerError : List String -> List String
addServerError list =
        "Server error" :: list

decodeErrors : Http.Error -> List String
decodeErrors error =
    let
        message = 
            case error of
                BadUrl err ->
                    "badurl" ++ err
                Timeout -> 
                    "timeout"
                NetworkError -> 
                    "network error"
                BadStatus code ->
                    "bad status" ++ String.fromInt code
                BadBody err ->
                    "bad body" ++ err
    in
    [message]
   
-- replace
userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
  case Regex.fromString userRegex of
    Nothing ->
      string

    Just regex ->
      Regex.replace regex replacer string


-- PERSISTENCE
storeViewerWith : Favorites -> Cmd msg
storeViewerWith favorites =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "favorites", Favorites.encode favorites )
                        ]
                  )
                ]
    in
    storeCache (Just json)



-- -- clear
clearCache : Cmd msg
clearCache =
    storeCache Nothing

port storeCache : Maybe Value -> Cmd msg
port onSessionChange : (Value -> msg) -> Sub msg

-- SoundManger
port soundCmdToJs : String -> Cmd msg


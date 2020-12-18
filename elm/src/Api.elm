module Api exposing (url, decodeErrors, addServerError, userReplace)

import Http exposing (Error(..))
-- import Json.Decode as Decode exposing (Decoder, decodeString, field, string, list)
-- import Json.Decode.Pipeline as Pipeline exposing (optional)
import Url.Builder exposing (string)
import Regex



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

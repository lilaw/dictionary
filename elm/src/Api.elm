module Api exposing (url, decodeErrors, addServerError)

import Http exposing (Error(..))
-- import Json.Decode as Decode exposing (Decoder, decodeString, field, string, list)
-- import Json.Decode.Pipeline as Pipeline exposing (optional)
import Url.Builder exposing (string)



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
   

-- errorDecoder : Decoder (List String)
-- errorDecoder =
-- 		Decode.string
-- 				|> Decode.map (\s -> [s])

-- errorsDecoder : Decoder (List String)
-- errorsDecoder =
-- 		Decode.keyValuePairs (list string)
-- 				|> Decode.map (List.concatMap fromPair)

-- fromPair : (String, List String) -> List String
-- fromPair (field, errors) =
-- 		List.map (\error -> field ++ " " ++ error) errors
-- fromPair : ( String, List String ) -> List String
-- fromPair ( field, errors ) =
--     List.map (\error -> field ++ " " ++ error) errors

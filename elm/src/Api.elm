-- module Api exposing (addServerError, decodeErrors, url)

-- import Http
-- import Json.Decode as Decode exposing (Decoder, decodeString, field, string, list)
-- import Json.Decode.Pipeline as Pipeline exposing (optional)
-- import Url.Builder



-- -- URL


-- {-| Get a URL to the Conduit API.
-- -}
-- url : List String -> String
-- url paths =
--     -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
--     -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
--     Url.Builder.crossOrigin "https://conduit.productionready.io" ("api" :: paths) []
    


-- -- ERRORS


-- addServerError : List String -> List String
-- addServerError list =
--     "Server error" :: list


-- {-| Many API endpoints include an "errors" field in their BadStatus responses.
-- -}
-- decodeErrors : Http.Error -> List String
-- decodeErrors error =
--     case error of 
--         Http.BadStatus response ->
--             let
--                 msgDecoder = Decode.oneOf 
--                     [ field "errors" errorsDecoder
--                     , field "error" errorDecoder
--                     ]
--                 statusDecoder = 
--                     field "status" Decode.string
--                         |> Decode.map (\code -> ["code: " ++ code ++ "\n"])
--                 statusAndError code msg =
--                     List.append code msg
--                 decoder = Decode.map2 statusAndError
--                     statusDecoder
--                     msgDecoder
--             in
--             response.body
--                 |> decodeString decoder
--                 |> Result.withDefault ["server error(badstatus): respone decode error"]
                
--         Http.BadPayload description _ ->
--             [description]
        
--         err ->
--             ["server error"]

-- errorDecoder : Decoder (List String)
-- errorDecoder =
--     Decode.string
--         |> Decode.map (\s -> [s])

-- errorsDecoder : Decoder (List String)
-- errorsDecoder =
--     Decode.keyValuePairs (list string)
--         |> Decode.map (List.concatMap fromPair)

-- fromPair : (String, List String) -> List String
-- fromPair (field, errors) =
--     List.map (\error -> field ++ " " ++ error) errors
-- -- fromPair : ( String, List String ) -> List String
-- -- fromPair ( field, errors ) =
-- --     List.map (\error -> field ++ " " ++ error) errors

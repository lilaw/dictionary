module Vocabulary exposing (Vocabulary, Word, Sense, Explan(..), decoder, relatedWord, id, entries, headword)

import Json.Decode as Decode exposing (Decoder, Value, list, string, at, field, index, oneOf, value, maybe, map2)
import Json.Decode.Pipeline exposing (custom, hardcoded, required, optional)
import Regex
import Http
import Api exposing (url)



type Vocabulary
  = Vocabulary Internals

type alias Internals =
    { slug : String
    , entries : List Word  -- for this word 
    , related : List Word   -- for other word
    }

type alias Word =
    { id : String
    , headword : String
    , define : Maybe (List (List Sense))
    -- , IPA : {}
    -- , functionalLabel : String  -- noun, verb
    -- , shortDefine : List String
    }

type alias Sense =
            { grammatical : String
            , explan : Maybe (List Explan)
            }
            
type Explan 
    = Meaning String --Text
    | ExampleSentence (List String)--vis
    | UsageNote --uns
        { text: String  -- explan
        , example: List String
        }
    | SupplementalNote String--snote
    | GrammaticalLabel String --wsgram
    | Other

  
-- Info
entries : Vocabulary -> List Word
entries (Vocabulary inter) =
  inter.entries

relatedWord : Vocabulary -> List Word
relatedWord (Vocabulary inter) =
  inter.related

id : Word -> String
id w = 
  w.id
  
headword : Word -> String
headword w = 
  w.headword

-- decoder


decoder : String -> Decoder Vocabulary
decoder slug = 
  Decode.succeed Internals
    |> hardcoded slug
    |> custom (list (wordDecode slug))
    |> custom (list (wordDecode slug))
    |> Decode.map filterOutRelate
    |> Decode.map Vocabulary


wordDecode : string -> Decoder Word
wordDecode slug = 
  Decode.succeed Word
    |> custom (at [ "meta", "id" ] string) -- id 
    |> custom (at [ "meta", "id" ] string) -- for headword, headword is an id without suffix
    |> custom (maybe (field "def" (index 0 (field "sseq" (list (list senseDecoder))))) )
    |> Decode.map removeSuffix


-- Internal
removeSuffix : Word -> Word
removeSuffix w = 
   {w | headword = userReplace ":\\d+$" (\_ -> "") w.id}


filterOutRelate : Internals -> Internals
filterOutRelate info  =
  {info 
    | entries = List.filter (\w -> w.headword == info.slug) info.entries
    , related =  List.filter (\w -> w.headword /= info.slug) info.related
  }



--  the definition of word is over-complex, we decode them to different elm type base on what the type they are with different decoder    
senseDecoder : Decoder Sense
senseDecoder =
    Decode.succeed Sense
        |> custom (
                maybe (index 1 (field "sgram" string)) |> Decode.map (Maybe.withDefault "")
            ) 
        |> custom ( 
                maybe (index 1 (field "dt" (list explanDecoder)))
            )

explanDecoder : Decoder Explan
explanDecoder = 
    index 0 string
        |> Decode.andThen explanHelper

explanHelper : String -> Decoder Explan
explanHelper idea =
    case idea of
        "text" ->
            Decode.map Meaning <|
                index 1 string
        "vis" ->
            Decode.map ExampleSentence <|
                index 1 (list (field "t" string)) 
        "uns" ->
            let
                unsDecoder = 
                    map2 (\a b -> {text= a, example= b})
                        (index 0 (index 0 (index 1 string)))
                        (index 0 (index 1 (index 1 (list (field "t" string)))))
            in
            Decode.map UsageNote <|
                index 1 unsDecoder 
        "wsgram" ->
            Decode.map GrammaticalLabel <|
                index 1 string 
                    
        _ -> 
            -- Decode.fail ("I can't decode" ++ idea)
            Decode.succeed Other

tokenParser : String -> String
tokenParser sentence =
    let
        remover = userReplace "\\{bc\\}" (\_ -> "")
        replacer = userReplace 
    in
    



module Vocabulary exposing (Vocabulary(..), Word, Sense, Explan(..), decoder, relatedWord, entries, headword, toFavored, toLastWord)

import Json.Decode as Decode exposing (Decoder, Value, list, string, at, field, index, oneOf, value, maybe, map2)
import Json.Decode.Pipeline exposing (custom, hardcoded, required, optional)
import Http
import Api exposing (url, userReplace)
import Vocabulary.Slug as Slug exposing (Slug)
import Vocabulary.Id as Id exposing (Id)
import Favorites exposing (Favored)
import Recent exposing (LastWord)
import Viewer exposing (Viewer)



type Vocabulary
  = Vocabulary Internals
  | Suggestion (List String)

type alias Internals =
    { slug : Slug
    , entries : List Word  -- for this word 
    , related : List Word   -- for other word
    }

type alias Word =
    { id : Id
    , headword : String
    , slug : Slug
    , define : Maybe (List (List Sense))
    , functionalLabel : Maybe String  -- noun, verb
    , ipa : Maybe String
    , audio : Maybe String
    , shortDefine : List String
    , crossReffences : Maybe String
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

    | Other -- ignore other

  
-- Info
entries : Vocabulary -> List Word
entries voc =
  case voc of
      Vocabulary inter->
        inter.entries
      _ ->
        []

relatedWord : Vocabulary -> List Word
relatedWord voc =
  case voc of
      Vocabulary inter->
        inter.related
      _ ->
        []

  
headword : Word -> String
headword w = 
  w.headword



-- decoder


decoder : Slug -> Decoder Vocabulary
decoder slug =
  let
      vocDecoder = 
        Decode.succeed Internals
          |> hardcoded slug
          |> custom (list wordDecode)
          |> custom (list wordDecode)
          |> Decode.map filterOutRelate
          |> Decode.map Vocabulary
      sugDecoder =
        Decode.map Suggestion <|
          list string
  in
    oneOf [ vocDecoder, sugDecoder ]

wordDecode : Decoder Word
wordDecode = 
  Decode.succeed Word
    |> custom (at [ "meta", "id" ] Id.decoder) -- id 
    |> custom (at [ "meta", "id" ] stringWithoutSuffix) -- for headword, headword is an id without suffix
    |> custom (at [ "meta", "id" ] (Slug.decoder stringWithoutSuffix))
    |> custom (maybe 
        (oneOf
            [ field "def" (index 0 (field "sseq" (list (list senseDecoder))))
            , field "dros" (index 0 (field "def" (index 0 (field "sseq" (list (list senseDecoder))))))
            ]
        ))
    |> custom (maybe (field "fl" string)) -- functional label
    |> custom (maybe (at ["hwi", "prs"] (index 0 (field "ipa" string)))) -- ipa
    |> custom (maybe (at ["hwi", "prs"] (index 0 (at ["sound", "audio"] string)))) --audio
    |> required "shortdef" (list string)
    |> custom (maybe (field "cxs" (index 0 crossReffencesDecoder)))
    


-- Internal
crossReffencesDecoder : Decoder String
crossReffencesDecoder = 
  Decode.succeed (\label hyperlinktext -> label ++ " {sx|" ++ hyperlinktext ++"||}")
    |> required "cxl" string
    |> custom (field "cxtis" (index 0 (field "cxt" string)))

stringWithoutSuffix : Decoder String
stringWithoutSuffix = 
  Decode.map 
    (userReplace ":\\d+$" (\_ -> "")) string

filterOutRelate : Internals -> Internals
filterOutRelate info  =
  {info 
    | entries = List.filter (\w -> w.headword == (Slug.toString info.slug)) info.entries
    , related = List.filter (\w -> w.headword /= (Slug.toString info.slug)) info.related
  }



--  the definition of word is over-complex, we decode them to different elm type base on what the type they are with different decoder    
senseDecoder : Decoder Sense
senseDecoder =
    Decode.succeed Sense
        |> custom (
                maybe (index 1 (field "sgram" string)) |> Decode.map (Maybe.withDefault "")
            ) 
        |> custom ( 
                maybe (index 1 (oneOf 
                    [ field "dt" (list explanDecoder)
                    , at ["sense", "dt"] (list explanDecoder)
                    ])
                )
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


-- FAVORITE
toFavored : Word -> Favored
toFavored wor =
  Favored wor.slug wor.id wor.headword wor.shortDefine


-- rcent 
toLastWord : Word -> LastWord
toLastWord wor =
  LastWord wor.slug wor.id wor.headword 
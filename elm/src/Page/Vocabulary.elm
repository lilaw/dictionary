module Page.Vocabulary exposing (Model, Msg, init, view, subscriptions, update)

import HttpBuilder exposing (RequestBuilder, withBody, withExpect)
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Api
import Session exposing (Session)
import Vocabulary exposing (Vocabulary, Word, Sense, Explan(..), decoder)
import Route exposing (Route)

type alias Model =
    { session : Session
    , errors : List String
    , vocabulary : Status Vocabulary
    }

type Status a
    = Loaded a
    | Loading 
    | LoadingSlowly
    | Failed

init : Session -> String -> (Model, Cmd Msg)
init session slug =
  ({ session = session
   , errors = []
   , vocabulary = Loading
   }
  , Cmd.batch
    [ fetch slug
    , Session.storeSession (Just "grppg")
    ]
  )

view : Model -> {title: String, content: Html msg}
view model = 
    let
        title = 
            case model.vocabulary of
                Loaded v ->
                    "loaded"
                Loading ->
                    "loading"
                LoadingSlowly ->
                    "loading"
                Failed ->
                    "failed"
        (enries, related) = sideNav model.vocabulary

    in
    { title = title
    , content = 
        div [class "content"]
            [ div [class "side-bar"]
                [ div [class "side-nav"] 
                        [ enries
                        , related
                        ]
                ]
            , div [class "vocabulary"] <|
                vocabularyView model.vocabulary
            ]
    }

relatedLink : Route -> Html msg -> Html msg
relatedLink route linkContent = li [class "entries__item"] [a [Route.href route, class "entries__link"] [linkContent]]

sideNav : Status Vocabulary -> (Html msg, Html msg)
sideNav vocabulary = 
    case vocabulary of
        Loaded voc ->
            let
                linkTo word = 
                    let
                        headword = Vocabulary.headword word
                    in
                    relatedLink (Route.Vocabulary headword) (text headword)
            in
                ( div [class "entries entries-thisword mb-md"]
                    [ h3 [class "entries__title heading-3"] [text "Entries"]
                    , ul [class "entries__list"] <|
                        List.map linkTo (Vocabulary.entries voc)
                        -- [ li [class "entries__item"] [a [href "", class "entries__link"] [text "go"] ]
                        -- , li [class "entries__item"] [a [href "", class "entries__link"] [text "go"] ]
                        -- ]
                    ]
                , div [class "entries entries-otherword"]
                    [ h3 [class "entries__title heading-3"] [text "Relate word"]
                    , ul [class "entries__list"] <|
                        List.map linkTo (Vocabulary.relatedWord voc)
                    ]
                )
                
        Loading ->
            ( text "loading", text "")
        LoadingSlowly ->
            ( text "loading", text "")
        Failed ->
            ( text "faild", text "")


vocabularyView : Status Vocabulary -> List (Html msg)
vocabularyView vocabulary =
        case vocabulary of
        Loaded voc ->
            let
                words = Vocabulary.entries voc
                a = Debug.log "head" words
                
                wordView word = 
                    div [class "vocabulary__container"] <|
                            h2 [class "vocabulary__headword heading-2 mb-sm"] [text (Vocabulary.headword word)] ::
                            defineView word.define
                            

                defineView define = 
                    case define of
                        Just listOfDefine ->
                            List.map (\listOfSenses -> 
                                    div [class "vocabulary__definition"] <|
                                        List.map senseView listOfSenses
                            ) listOfDefine
                    
                        Nothing ->
                            [text ""]
                    
            in
                List.map wordView words
                
                
        Loading ->
            [ text "loading" ]
        LoadingSlowly ->
            [ text "loadingSlowly" ]
        Failed ->
            [ text "faild" ]

senseView : Sense -> Html msg
senseView sense =
    let
        explanView explan = 
            case explan of
                Meaning mean ->
                    figcaption [class "vocabulary__senses-text"] 
                        [ if (String.isEmpty >> not) sense.grammatical then span [class "vocabulary__label"] [text sense.grammatical] else text ""
                        , text mean
                        ]

                ExampleSentence sentences ->
                    sentencesView sentences
                UsageNote note ->
                    div [class "vocabulary__usage"] 
                        [ p [class "vocabulary__usage-text"] [text note.text]
                        , sentencesView note.example
                        ]

                GrammaticalLabel lable ->
                    p [class "vocabulary__label"] [text lable]
                _ ->
                    text ""
    in
    -- div [class "vocabulary__definition"] [
        case sense.explan of
            Nothing ->
                span [class "vocabulary__grammatical"] [text sense.grammatical] 
        
            Just explans ->
                figure [class "vocabulary__senses"] <|
                    List.map explanView explans
    -- ]
    
sentencesView : List String -> Html msg
sentencesView sentences =
     ul [class "vocabulary__list-example"] <|
        List.map (\s -> li [class "vocabulary__example"] [text s]) sentences
    

-- embedHTML : String -> Html.Attribute msg









type Msg
    = CompetedVocabularyLoad (Result Http.Error Vocabulary)
    | GotSession Session

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
        CompetedVocabularyLoad (Ok vocabulary) ->
            -- let
            --     e = Debug.log "voc" (Vocabulary.entries vocabulary)
            -- in
            ( {model | vocabulary = Loaded vocabulary}, Cmd.none)
        CompetedVocabularyLoad (Err errors) ->
            let
                e = Debug.log "error" errors
            in
            
            ( {model | errors = Api.decodeErrors errors}, Cmd.none)
        GotSession session ->
            ( model, Cmd.none)



-- Http

fetch : String -> Cmd Msg
fetch slug =
  let
      expect = Http.expectJson CompetedVocabularyLoad (decoder slug)
  in
  Api.url [slug]
    |> HttpBuilder.get
    |> HttpBuilder.withExpect expect
    |> HttpBuilder.request





subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


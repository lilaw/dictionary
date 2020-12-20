module Header.SearchBox exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (svg, use)
import Svg.Attributes as SvgAttributes
import Route
import Session exposing (Session)
import Vocabulary.Slug exposing (buildSlug)

-- an searchbox in header


-- Model
type alias Model =
  { session : Session
  , problem : List Problem
  , form: Form
  }

type alias Form =
    { word : String }  

type Problem 
  = InvalidEntry Field String

init : Session -> Model
init session  = 
   { session = session
    , problem = []
    , form = { word = ""}
    }


-- View
view : Model -> Html Msg
view model =
  Html.form [ class "search", onSubmit SubmittedForm]
    [ input [ classList [("search__input", True), ("search__input-error", not (List.isEmpty model.problem)) ], type_ "text", required True, onInput EnteredWord, value model.form.word] []
    , button [ class "search__btn"] 
        [ svg [ SvgAttributes.class "search__icon"] [ use [ SvgAttributes.xlinkHref "/img/sprite.svg#icon-magnifying-glass" ] []]
        
        ]
    ]

type Msg
  = EnteredWord String
  | SubmittedForm 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        EnteredWord word ->
            updateForm (\form -> {form | word = word}) model

        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                  ( init model.session
                    , Route.pushUrl (Session.navKey model.session) (Route.Vocabulary <| buildSlug validForm.word)
                  )  
            
                Err problem ->
                  ({model | problem = problem}, Cmd.none)
                    
            

updateForm : (Form -> Form) -> Model -> (Model, Cmd msg)
updateForm transform model =
  ( {model | form = transform model.form}, Cmd.none)


-- validate
validate : Form -> Result (List Problem) Form
validate form =
  let
      trimmedForm = trimField form
  in
    case List.concatMap (validateField trimmedForm) fieldList of
      [] -> 
        Ok trimmedForm
      problem ->
        Err problem

validateField : Form -> Field -> List Problem
validateField form field = 
  List.map (InvalidEntry field) <|
    case field of
        Word ->
            if String.isEmpty form.word then
              ["please input a word to search"]
            else 
              []
  

  
trimField : Form -> Form
trimField form =
  { word = String.trim form.word }

type Field 
   = Word

fieldList : List Field
fieldList = [Word] 
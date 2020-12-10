module Page.Vocabulary exposing (Model, Msg, init, view, subscriptions)

import Session exposing (Session)
import Html exposing (..)

type alias Model =
    { session : Session
    ,  word : String
    }

init : Session -> String -> (Model, Cmd msg)
init session word =
  ({ session = session
   ,  word = word
   }
  , Cmd.none
  )

view : Model -> {title: String, content: Html msg}
view model = 
    { title = model.word
    , content = text "Vocabulary"
    }
  

type Msg
    = A

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
        A ->
             ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
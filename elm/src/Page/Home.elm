module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Svg exposing (svg, use)
import Svg.Attributes as SvgAttributes
import Session exposing (Session)
import Favorites exposing (Favorites)
import Recent exposing (Recent)
import Vocabulary.Id exposing (Id)
import Route 
import Viewer


-- MODEL
type alias Model =
    { session : Session
    , isEditingRecent : Bool
    }



init : Session -> (Model, Cmd Msg)
init session =
   ( { session = session
     , isEditingRecent = False
     }
   , Cmd.none
   )


-- VIEW
view : Model -> { title: String, content : Html Msg }
view model = 
    { title = "home"
    , content = 
        div [class "content"]
            [ section [class "showcase mb-md"]
                [ header [class "showcase__header"] 
                    [ h2 [class "showcase__title heading-3"] [text "recent"]
                    , div [class "btn-ranges"] <|
                        if model.isEditingRecent then
                            [ button [class "btn-ranges__btn showcase__btn-clear", onClick ClickedClearAll] [text "Clear all"]
                            , button [class "btn-ranges__btn showcase__btn-edit", onClick ClickedUnediting] [text "Done"]
                            ]
                        else
                            [button [class "btn-ranges__btn showcase__btn-clear", onClick ClickedEditing] [text "Edit"]]
                            
                    
                            
                    
                    ]
                , viewRecent (Viewer.recent <| Session.viewer model.session) model.isEditingRecent
                ]
            , section [class "showcase"]
                [ header [class "showcase__header"]
                    [ h3 [class "showcase__title heading-3"] [text "favorites"]
                    , div [class "btn-ranges"]
                        [
                        a [Route.href Route.Favorites, class "btn-ranges__btn showcase__link"] [text "more"] 
                        ]
                    ]
                , viewFavorites (Viewer.favorites <| Session.viewer model.session)
                ]
            ]
    }
viewRecent : Recent -> Bool -> Html Msg
viewRecent recent isEditingRecent =
    let
        histroylist = Recent.toList recent
        viewLink histroy =
            li [class "showcase__item"] 
                [ a [Route.href (Route.Vocabulary histroy.slug Nothing), class "showcase__link"] [text histroy.headword]
                , button [class "showcase__remove", onClick (ClickedRemoveHistroy histroy.id)]
                    [ svg [ SvgAttributes.class "showcase__remove-icon"] [ use [ SvgAttributes.xlinkHref "/img/sprite.svg#icon-close" ] [] ] ]
                ]
    in
    if List.isEmpty histroylist then
        p [] [text "No recent search history"]
    else
        ul [classList [("showcase__list", True), ("showcase__list-edit", isEditingRecent)]] <|
            List.map viewLink histroylist

viewFavorites : Favorites -> Html msg
viewFavorites favorites =
    let
        favoredList = Favorites.toList favorites
        viewLink favored =
            li [class "showcase__item"] 
                [ a [Route.href (Route.Vocabulary favored.slug (Just favored.id)), class "showcase__link"] [text favored.headword] ]

    in
    if List.isEmpty favoredList then 
        p [] [text "No favorited words"]
    else
        ul [class "showcase__list showcase__list-favorites"] <|
            List.map viewLink favoredList
            



-- UPDATE
type Msg
    = ClickedEditing
    | ClickedUnediting
    | ClickedClearAll
    | ClickedRemoveHistroy Id
    | GotSession Session



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedEditing ->
            ( {model | isEditingRecent = True }, Cmd.none)
        ClickedUnediting ->
            ( {model | isEditingRecent = False }, Cmd.none)
        ClickedClearAll ->
            ( model
            , Recent.emptyList
                |> Viewer.updateRecent (Session.viewer model.session)
                |> Viewer.store
            )
        ClickedRemoveHistroy id ->
            ( model
            , Recent.remove (Viewer.recent <| Session.viewer model.session) id
                |> Viewer.updateRecent (Session.viewer model.session)
                |> Viewer.store
            )
        GotSession session ->
            ({model | session = session}, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

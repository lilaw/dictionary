module Page.Favorites exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn, preventDefaultOn)
import Json.Decode as Decode
import Session exposing (Session)
import PaginatedList exposing (PaginatedList, fromList)
import Viewer exposing (Viewer)
import Favorites exposing (Favored, toList)
import Vocabulary.Slug as Slug exposing (Slug)
import Svg exposing (svg, use)
import Svg.Attributes as SvgAttributes
import Route
import Vocabulary


type alias Model =
    { session : Session
    , viewingPage : Int
    , itemPerPage : Int
    }

init : Session -> ( Model, Cmd Msg )
init session = 
  ( { session = session 
    , viewingPage = 1
    , itemPerPage = 9
    }
  , Cmd.none
  )

view : Model -> { title : String, content : Html Msg }
view model = 
  { title = "favorites"
  , content = 
    let
        viewer = Session.viewer model.session
        favorites = Viewer.favorites viewer
        totalPage =
            List.map toFloat [ Favorites.length favorites, model.itemPerPage]
            |> List.foldr (/) 1
            |> ceiling
        paginatedList = PaginatedList.fromList totalPage model.viewingPage model.itemPerPage (Favorites.toList favorites)
        
    in
      div [class "content"] 
        [ div [class "favorites"] <|
            List.concat
            [ [h3 [class "favorites__title heading-2"] [text "favorites"]]
            , List.map (viewFavored viewer) (PaginatedList.values paginatedList)
            , [viewPagination paginatedList]
            ]
        ]
  }

viewFavored : Viewer -> Favored -> Html Msg
viewFavored viewer favored =
    let
        defines meaning = li [class "favored__mean"] [text meaning]
    in
        figure [class "favored"]
            [ button [class "favored__remove", onCLickStopPropagation (ClickedUnfavorite viewer favored), type_ "button"]
                [ svg [ SvgAttributes.class "favored__remove-icon"] [ use [ SvgAttributes.xlinkHref "/img/sprite.svg#icon-close" ] [] ] ]
            , a [Route.href (Route.Vocabulary favored.slug (Just favored.id)), class "favored__link" ] 
                [ figcaption [class "favored__header"] 
                    [ span [class "favored__headword heading-3"] [text favored.headword] ]
                , ul [class "favored__defines"] <|
                    List.map defines (List.take 2 favored.shortDefine)
                ]
            ]
    
viewPagination : PaginatedList a -> Html Msg
viewPagination paginatedList =
    PaginatedList.view ClickedPage paginatedList

                
type Msg 
    = GotSession Session
    | ClickedPage Int
    | ClickedUnfavorite Viewer Favored

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
        GotSession session ->
            ( {model | session = session} , Cmd.none)
        ClickedPage pageNum -> 
            ( { model | viewingPage = pageNum }, Cmd.none)
        ClickedUnfavorite viewer favored ->
            ( model, Viewer.unfavorite viewer favored)
  
subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)          
  
-- export
toSession : Model -> Session
toSession model =
  model.session

-- internal
onCLickStopPropagation : msg -> Attribute msg
onCLickStopPropagation msg =
    stopPropagationOn "click" (Decode.succeed (msg, True))
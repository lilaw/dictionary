module Main exposing (main)

import Html exposing (div)
import Html.Attributes exposing (class)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Value)
import Url exposing (Url)
import Html exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import Page as ViewPage
import Page.Vocabulary as Vocabulary
import Page.Favorites as Favorites
import Header.SearchBox as SearchBox



-- model 
type alias Model =
    { searchBox : SearchBox.Model
    , page : Page
    }

type Page
    =  Vocabulary Vocabulary.Model
    | Favorites Favorites.Model
    | Redirect Session

init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg)
init flags url key=
    let
        a = Debug.log "url" url
    in
    
    changeRouteTo (Route.fromUrl url) 
        (Redirect (Session.decode key flags))
    

-- viewPage : ViewPage.Page ->  { title : String, content : Html msg } -> (msg -> Msg) -> Html msg -> Document Msg 

-- view
view : Model -> Document Msg
view model =
    let
        -- mapBody bo = List.drop 1 bo >> List.map (Html.map toMsg) 
        margePage page pageConfig toPageMsg = 
            let
                {header, footer} = ViewPage.viewHeaderFooter page searchBoxConfig
                {title, content} = ViewPage.viewMainContent pageConfig
            in
            { title = title
            , body = [ div [ class"container"]
                        [ Html.map GotSearchBoxMsg header
                        , Html.map toPageMsg content
                        , Html.map GotSearchBoxMsg footer
                        ] 
                     ]
            }
        searchBoxConfig = SearchBox.view model.searchBox
    in
    
    case model.page of
        Vocabulary vocabulary ->
            margePage ViewPage.Other (Vocabulary.view vocabulary) GotVocabularyMsg
        Favorites favorites -> 
            margePage ViewPage.Favorites (Favorites.view favorites) GotFavoritesMsg
        Redirect _ ->
            { title = "vocabulary"
            , body = [text "red"]
            }

            

-- update
type Msg 
    = GotVocabularyMsg Vocabulary.Msg
    | GotSearchBoxMsg SearchBox.Msg
    | GotFavoritesMsg Favorites.Msg
    | Ignored
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page) of
        ( Ignored, _) ->
            ( model, Cmd.none)
        ( GotSearchBoxMsg subMsg, anyPage) ->
            let
                (newModel, newMsg) = 
                    SearchBox.update subMsg model.searchBox
                a = Debug.log "input" newModel
            in
                ( { searchBox = newModel
                 , page = anyPage
                 }    
                , Cmd.map GotSearchBoxMsg newMsg
                )
        ( GotVocabularyMsg subMsg, Vocabulary vocabulary ) ->
            Vocabulary.update subMsg vocabulary
                |> updateWithPage Vocabulary GotVocabularyMsg model
        ( GotFavoritesMsg subMsg, Favorites favorites ) ->
            Favorites.update subMsg favorites
                |> updateWithPage Favorites GotFavoritesMsg model
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model.page
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        a = Debug.log "url" url
                    in
                    
                    case url.fragment of
                        Nothing ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model.page)) (Url.toString url)
                            )

                        Just _ ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            ( model, Cmd.none )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )
        ( _, _) ->
            ( model, Cmd.none)



changeRouteTo : Maybe Route -> Page -> ( Model, Cmd Msg )
changeRouteTo maybeRoute page =
    let
        session = toSession page
        a = Debug.log "route" maybeRoute
        model = 
            { searchBox = SearchBox.init session
            , page = page
            }    
    in
    
    case maybeRoute of
        Nothing ->
            (model, Cmd.none)

        Just (Route.Vocabulary slug) ->
            Vocabulary.init session slug
                |> updateWithPage Vocabulary GotVocabularyMsg model
        Just (Route.Favorites) ->
            Favorites.init session
                |> updateWithPage Favorites GotFavoritesMsg model
        Just Route.Home -> 
            (model, Cmd.none)

updateWithPage : (subModel -> Page) -> (subMsg -> Msg) -> Model -> (subModel, Cmd subMsg) -> (Model, Cmd Msg)
updateWithPage toPage toMsg model (subModel, subMsg) =
    let
        toModel subM = 
            { searchBox = model.searchBox
            , page = toPage subM
            }    
    in
    ( toModel subModel
    , Cmd.map toMsg subMsg
    )
    


toSession : Page -> Session
toSession page =
    case page of
        Redirect session ->
            session
        Vocabulary vocabulary ->
            vocabulary.session
        Favorites favorites ->
            favorites.session





-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = 
    case model.page of
        Redirect _ ->
            Sub.none
        Vocabulary voc ->
            Sub.map GotVocabularyMsg (Vocabulary.subscriptions voc)
        Favorites favorites ->
            Sub.map GotFavoritesMsg (Favorites.subscriptions favorites)

       


-- MAIN
main : Program Value Model Msg
main = Browser.application    
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ClickedLink
    , onUrlChange = ChangedUrl
    }
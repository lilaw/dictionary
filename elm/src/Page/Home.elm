module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Article.FeedSources as FeedSources exposing (FeedSources, Source(..))
import Article.Tag as Tag exposing (Tag)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import HttpBuilder
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Username exposing (Username)
import Viewer.Cred as Cred exposing (Cred)



-- MODEL
type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , feedTab : FeedTab
    , feedPage : Int
    -- load from server
    , tags : Status (List Tag)
    , feed : Status Feed.Model
    }

type FeedTab 
    = YourFeed Cred
    | GlobalFeed
    | TagFeed Tag
type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed

init : Session -> (Model, Cmd Msg)
init session =
    let
        feedTab = 
            case Session.cred session of
                Just cred ->
                    YourFeed cred
                Nothing ->
                    GlobalFeed
    in
    ( { session = session
      , timeZone = Time.utc
      , feedTab = feedTab
      , feedPage = 1
      , tags = Loading
      , feed = Loading
      }
    , Cmd.batch 
        [ fetchFeed session feedTab 1
            |> Task.attempt CompletedFeedLoad
        , Tag.list 
            |> Http.send CompletedTagLoad
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


-- VIEW
view : Model -> { title: String, content : Html Msg }
view model =
    { title = "Conduit"
    , content = div [ class "home-page" ]
        [ viewBanner
        , div [ class "container page" ]
            [ div [class "row" ]
                [ div [class "col-md-9"] <|
                    case model.feed of
                        Loaded feed ->
                            List.concat
                                [ [ viewTabs (Session.cred model.session) model.feedTab]
                                , Feed.viewArticles model.timeZone feed
                                    |> List.map (Html.map GotFeedMsg)
                                , [Feed.viewPagination ClickedFeedPage feed]
                                ]
                    
                        Loading -> 
                            []
                        LoadingSlowly ->
                            [ Loading.icon]
                        Failed ->
                            [Loading.error "feed"]
                    
                , div [class "col-md-3"] <|
                    case model.tags of
                        Loaded tags ->
                            [ div [ class "sidebar"] <|
                                [ p [] [text "popular tags"]
                                , viewTags tags
                                ]

                            ]
                    
                        Loading ->
                            []
                        LoadingSlowly ->
                            [Loading.icon]
                        Failed ->
                            [ Loading.error "tags"]
                ]
            ]
        ]
    }

viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]




-- TABS
viewTabs : Maybe Cred -> FeedTab -> Html Msg
viewTabs maybeCred tab =
    case tab of
        YourFeed cred ->
            Feed.viewTabs [] (yourFeed cred) [globalFeed]
        GlobalFeed ->
            let
                otherTabs = 
                    case maybeCred of
                        Just cred ->
                            [yourFeed cred]
                        Nothing ->
                            []
            in
            Feed.viewTabs otherTabs globalFeed []
        TagFeed tag ->
            let
                otherTabs = 
                    case maybeCred of
                        Just cred ->
                            [yourFeed cred, globalFeed]
                    
                        Nothing ->
                            [globalFeed]
            in
            Feed.viewTabs otherTabs (tagFeed tag) []

yourFeed : Cred -> (String, Msg)
yourFeed cred =
    ("your feed", ClickedTab (YourFeed cred))

globalFeed : (String, Msg)
globalFeed =
    ("global feed", ClickedTab GlobalFeed)
tagFeed : Tag -> (String, Msg)
tagFeed tag =
    ("#" ++ Tag.toString tag, ClickedTab (TagFeed tag))




-- TAGS
viewTags : List Tag -> Html Msg
viewTags tags = 
    div [ class "tag-list" ] (List.map viewTag tags)

viewTag : Tag -> Html Msg
viewTag tagName =
    a [ class "tag-pill tag-default", onClick (ClickedTag tagName), href ""]
        [text (Tag.toString tagName)]
        



-- UPDATE
type Msg
    = ClickedTab FeedTab
    | ClickedTag Tag
    | ClickedFeedPage Int
    | CompletedFeedLoad (Result Http.Error Feed.Model)
    | CompletedTagLoad (Result Http.Error (List Tag))
    | GotTimeZone Time.Zone
    | GotSession Session
    | GotFeedMsg Feed.Msg
    | PassedSlowLoadThreshold


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedTab tab ->
            ( {model | feedTab = tab}
            , fetchFeed model.session tab 1
                |> Task.attempt CompletedFeedLoad
            )

        ClickedTag tag ->
            ( {model | feedTab = TagFeed tag}
            , fetchFeed model.session model.feedTab 1
                |> Task.attempt CompletedFeedLoad
            )
        ClickedFeedPage pageToLoad ->
            ( model
            , fetchFeed model.session model.feedTab pageToLoad
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Task.attempt CompletedFeedLoad
            )
        CompletedFeedLoad (Ok feed) ->
            ( {model | feed = Loaded feed }, Cmd.none)
        CompletedFeedLoad (Err err) ->
            ( {model | feed = Failed }, Cmd.none)
        
        CompletedTagLoad (Ok tags) ->
            ( {model | tags = Loaded tags}, Cmd.none)
        CompletedTagLoad (Err err) ->
            ( {model | tags = Failed}, Cmd.none)

        GotFeedMsg subMsg -> 
            case model.feed of
                Loaded feed ->
                    let
                        (subModel, subCmd) = Feed.update (Session.cred model.session) subMsg feed
                    in
                    ( {model | feed = Loaded subModel}
                    , Cmd.map GotFeedMsg subCmd
                    )                    
                Loading ->
                    ( model, Log.error )
                LoadingSlowly ->
                    ( model, Log.error )
                Failed ->
                    ( model, Log.error )
                    
            
        GotTimeZone tz -> 
            ({model | timeZone = tz}, Cmd.none)
            
        PassedSlowLoadThreshold ->
            let
                feed = 
                    case model.feed of
                        Loading ->
                            LoadingSlowly
                        other ->
                            other
                tags =
                    case model.tags of
                        Loading -> 
                            LoadingSlowly
                        other ->
                            other                    
            in
            ({model | feed = feed, tags = tags}, Cmd.none)
        
        GotSession session ->
            ({model | session = session}, Cmd.none)
-- HTTP
fetchFeed : Session -> FeedTab -> Int -> Task Http.Error Feed.Model
fetchFeed session feedTab curPage =
    let
        maybeCred = Session.cred session
        builder = 
            case feedTab of
                YourFeed cred ->
                    Api.url ["articles", "feed"]
                        |> HttpBuilder.get
                        |> Cred.addHeader cred
            
                GlobalFeed ->
                    Api.url ["articles"]
                        |> HttpBuilder.get
                        |> Cred.addHeaderIfAvailable maybeCred

                TagFeed tag ->
                    Api.url ["articles"]
                        |> HttpBuilder.get
                        |> Cred.addHeaderIfAvailable maybeCred
                        |> HttpBuilder.withQueryParam "tag" (Tag.toString tag)
        expect = Feed.decoder maybeCred resultsPerPage
        resultsPerPage = 10
    in
    builder
        |> HttpBuilder.withExpectJson expect
        |> PaginatedList.fromRequestBuilder resultsPerPage curPage
        |> Task.map (Feed.init session)

scrollToTop : Task x ()
scrollToTop = 
    Dom.setViewport 0 0
        |> Task.onError (\_ -> Task.succeed ())



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

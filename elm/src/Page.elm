module Page exposing (Page(..), viewHeaderFooter, viewMainContent)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (svg, use)
import Svg.Attributes as SvgAttributes
import Route exposing (Route)
import Html.Events exposing (onClick)
import Asset exposing (logo)
import Header.SearchBox as SearchBox

{-| Determines which navbar link (if any) will be rendered as active.
-}

type Page
    = Other
    | Home


{-| Take a page's Html and frames it with a header and footer.


-}
-- view : Html msg -> Page -> { title : String, content : Html msg }  -> {title: String, header : Html msg, content: Html msg, footer: Html msg}
-- view searchBoxContent page { title, content }  =
--     { title = title ++ " - Dictionary"
--     , header = 
--     , content = content
--     , footer = viewFooter
--     }
viewHeaderFooter : Page -> Html msg -> {header: Html msg, footer : Html msg}
viewHeaderFooter page searchBoxConfig =
    { header = viewHeader searchBoxConfig page 
    , footer = viewFooter
    }
viewMainContent :  { title : String, content : Html msg } ->  { title : String, content : Html msg }
viewMainContent  { title, content } =
    { title = title ++ " - Dictionary"
    , content = content
    }
    

-- view :  Page -> { title : String, content : Html msg } -> Html msg  -> {title: String, header : Html msg, content: Html msg, footer: Html msg}
-- view page { title, content } searchBoxContent =
--     { title = title ++ " - Dictionary"
--     , header = viewHeader searchBoxContent page 
--     , content = content
--     , footer = viewFooter
--     }

viewHeader : Html msg -> Page -> Html msg
viewHeader searchBoxContent page  =
    header [ class "header"]
        [ img [ class "header__logo", Asset.src Asset.logo] []
        , searchBoxContent
        , nav [ class "nav__container" ]
            [ ul [class "nav__list"] 
                [ li [class "nav__item nav__item--active"]
                    [ a [href "", class "nav__link"] 
                        [ svg [ SvgAttributes.class "nav__icon"] [ use [ SvgAttributes.xlinkHref "/img/sprite.svg#icon-home" ] [] ]
                        , span [] [text "home"]
                        ]
                    ]
                
                , li [class "nav__item"]
                    [ a [href "", class "nav__link"]
                        [ svg [ SvgAttributes.class "nav__icon"] [ use [ SvgAttributes.xlinkHref "/img/sprite.svg#icon-key" ] [] ]
                        , span [] [text "favorites"]
                        ]
                    ]
                ]
            ]
        ]

-- viewMenu : Page -> Maybe Viewer -> List (Html msg)
-- viewMenu page maybeViewer =
--     let
--         linkTo = navbarLink page
--     in
    
--     case maybeViewer of
--         Just viewer ->
--             let
--                 cred = Viewer.cred viewer
--                 username = Cred.username cred
--                 avatar = Profile.avatar (Viewer.profile viewer)
--             in
--                 [ linkTo Route.NewArticle [ i [ class "ion-compose" ] [], text " New Post" ]
--                 , linkTo Route.Settings [ i [ class "ion-gear-a" ] [], text " Settings" ]
--                 , linkTo
--                     (Route.Profile username)
--                     [ img [ class "user-pic", Avatar.src avatar ] []
--                     , Username.toHtml username
--                     ]
--                 , linkTo Route.Logout [ text "sign out"]
--                 ]

    
--         Nothing ->
--                 [ linkTo Route.Login [ text "sign in"]
--                 , linkTo Route.Register [ text "sign up"]
--                 ]

viewFooter : Html msg
viewFooter =
    footer []
        [ 
          
             text "footer"
          
        ]

-- navbarLink : Page -> Route -> List (Html msg) -> Html msg
-- navbarLink page route linkContent =
--     li [ classList [("nav-item", True), ("active", isActive page route)] ]
--         [ a [ class "nav-link", Route.href route] linkContent ] 

-- isActive : Page -> Route -> Bool
-- isActive page route =
--     case ( page, route ) of
--         (Home, Route.Home) ->
--             True
--         (Login, Route.Login) ->
--             True
--         (Register, Route.Register) ->
--             True
--         ( Settings, Route.Settings) ->
--             True
--         ( Profile pageUsername, Route.Profile routeUsername) ->
--             pageUsername == routeUsername
--         (NewArticle, Route.NewArticle) ->
--             True
--         _ ->
--             False

-- -- Render dismissable errors. We use this all over the place!
-- viewError : msg -> List String -> Html msg
-- viewError dismissErrors errors =
--     if List.isEmpty errors then 
--         text ""
--     else 
--         div
--         [ class "error-messages"
--         , style "position" "fixed"
--         , style "top" "0"
--         , style "background" "rgb(250, 250, 250)"
--         , style "padding" "20px"
--         , style "border" "1px solid"
--         ]
--         <|
--         List.map (\err -> p [] [ text err]) errors
--             ++ [ button [ onClick dismissErrors ] [text "ok"] ]
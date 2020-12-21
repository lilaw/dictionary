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
    | Favorites


{-| Take a page's Html and frames it with a header and footer.


-}

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
    

viewHeader : Html msg -> Page -> Html msg
viewHeader searchBoxContent page  =
    header [ class "header"]
        [ img [ class "header__logo", Asset.src Asset.logo] []
        , searchBoxContent
        , nav [ class "nav__container" ]
            [ ul [class "nav__list"] 
                (viewMenu page)
            ]
        ]

viewMenu : Page  -> List (Html msg)
viewMenu page =
    let
        linkTo = navbarLink page
    in
        [ linkTo Route.Home 
            [ svg [ SvgAttributes.class "nav__icon"] [ use [ SvgAttributes.xlinkHref "/img/sprite.svg#icon-home" ] [] ]
            , span [] [text "home"]
            ]
        , linkTo Route.Favorites
            [ svg [ SvgAttributes.class "nav__icon"] [ use [ SvgAttributes.xlinkHref "/img/sprite.svg#icon-star-full" ] [] ]
            , span [] [text "favorites"]
            ]
        ]

viewFooter : Html msg
viewFooter =
    footer []
        [ 
            text ""
        ]

navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [classList [("nav__item", True), ("nav__item-active", isActive page route)]]
        [ a [Route.href route, class "nav__link"] linkContent
        ]

isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        (Home, Route.Home) ->
            True
        (Favorites, Route.Favorites) ->
            True
        _ ->
            False

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
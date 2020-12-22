module Route exposing (Route(..), fromUrl, href, pushUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)
import Vocabulary.Id as Id exposing (Id)
import Vocabulary.Slug as Slug exposing (Slug)



-- ROUTING


type Route
    = Home
    | Vocabulary Slug (Maybe Id)
    | Favorites


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Vocabulary (s "vocabulary" </> Slug.urlParser <?> Id.urlParser)
        , map Favorites (s "favorites")
        ]



-- PUBLIC HELPERS


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)



-- -- INTERNAL


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    []

                Vocabulary slug id ->
                    let
                        query = 
                            case id of
                                Just i ->
                                    "?id=" ++ Id.toString i
                                Nothing ->
                                    ""
                    in
                    [ "vocabulary"
                    , Slug.toString slug ++ query
                    ]

                Favorites ->
                    [ "favorites" ]
    in
    "/" ++ String.join "/" pieces

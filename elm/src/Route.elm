module Route exposing (Route(..), fromUrl, pushUrl, href)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)
import Browser.Navigation as Nav
import Vocabulary.Slug as Slug exposing (Slug)



-- ROUTING


type Route
    = Home
    | Vocabulary Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Vocabulary (s "vocabulary" </> Slug.urlParser)
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
            
                Vocabulary slug ->
                    ["vocabulary", Slug.toString slug]
            
    in
        "/" ++ String.join "/" pieces

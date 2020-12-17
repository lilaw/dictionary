module Route exposing (Route(..), fromUrl, pushUrl, href)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)
import Browser.Navigation as Nav



-- ROUTING


type Route
    = Home
    | Vocabulary String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Vocabulary (s "vocabulary" </> string)
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
            
                Vocabulary string ->
                    ["vocabulary", string]
            
    in
        "/" ++ String.join "/" pieces

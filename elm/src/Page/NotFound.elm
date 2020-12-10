module Page.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Asset

-- VIEW

view : { title : String, content : Html msg}
view = 
    { title = "Not found"
    , content = 
        main_ [ class "container", id "cotent", tabindex -1 ]
            [ h1 [] [ text "page not found"]
            , div [ class "row" ]
                [ img [Asset.src Asset.error] [] ]
            ]
    }
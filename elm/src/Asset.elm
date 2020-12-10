module Asset exposing (Image, logo, error, loading, src)

{-| Assets, such as images, videos, and audio. (We only have images for now.)

We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attr

type Image 
    = Image String

-- IMAGES

error : Image
error =
    image "error.jpg"


image : String -> Image
image file =
    Image ("/img/" ++ file)


loading : Image
loading =
    image "loading.svg"

logo : Image
logo = image "logo.png"


-- USING IMAGES

src : Image -> Attribute msg
src (Image url) =
    Attr.src url

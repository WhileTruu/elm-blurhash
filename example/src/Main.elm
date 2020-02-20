module Main exposing (main)

import Blurhash
import Browser
import Html exposing (Html)
import Html.Attributes


uri : String
uri =
    Blurhash.toUri { width = 30, height = 30 } 1.0 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"


view : Html msg
view =
    Html.img [ Html.Attributes.style "width" "400px", Html.Attributes.src uri ] []


main =
    Browser.sandbox
        { init = ()
        , view = \_ -> view
        , update = \_ _ -> ()
        }

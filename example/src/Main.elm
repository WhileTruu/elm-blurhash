module Main exposing (main)

import Blurhash
import Html exposing (Html)
import Html.Attributes


main : Html msg
main =
    let
        uri =
            Blurhash.toUri { width = 30, height = 30 }
                1.0
                "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"
    in
    Html.img
        [ Html.Attributes.style "width" "400px"
        , Html.Attributes.src uri
        ]
        []

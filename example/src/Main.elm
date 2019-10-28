module Main exposing (main)

import Base64
import Bitwise
import Blurhash
import Browser exposing (Document)
import Html as H exposing (Html)
import Html.Attributes as HA
import Image exposing (Image)


main : Platform.Program () () Never
main =
    Browser.sandbox
        { init = ()
        , update = \_ _ -> ()
        , view = \_ -> view
        }



-- VIEW


view : Html Never
view =
    let
        pngEncodeBase64 =
            Blurhash.decode 300 300 1.0 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"
                |> List.map rgbToInt
                |> Image.fromList 300
                |> Image.encodePng
                |> Base64.fromBytes
                |> Maybe.withDefault ""
    in
    H.img [ HA.style "width" "400px", HA.src ("data:image/png;base64," ++ pngEncodeBase64) ] []


rgbToInt : ( Int, Int, Int ) -> Int
rgbToInt ( r, g, b ) =
    Bitwise.shiftLeftBy 24 r + Bitwise.shiftLeftBy 16 g + Bitwise.shiftLeftBy 8 b + 255

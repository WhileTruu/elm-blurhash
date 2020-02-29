module BlurHash exposing
    ( toUri
    , fromImage, fromPixels
    , encodeBase83, decodeBase83
    )

{-| Display [blurhash](https://blurha.sh/) in elm

@docs toUri


## Encoding

Create a blurhash from an image. Unlikely this is useful, but here it is.

@docs fromImage, fromPixels


## Base83

@docs encodeBase83, decodeBase83

-}

import Array
import CellGrid exposing (CellGrid)
import Color exposing (Color)
import Image
import Internal


{-| Convert a blurhash into an image URI. The float parameter is the `punch`, used to increase/decrease contrast of the resulting image

    punch : Float
    punch =
        0.9

    hash : String
    hash =
        "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"

    BlurHash.toUri { width = 4, height = 4 } punch hash
    -->  "data:image/bmp;base64,Qk26AAAAAAAAAHoAAABsAAAABAAAAAQAAAABACAAAwAAAEAAAAATCwAAEwsAAAAAAAAAAAAAAAAA/wAA/wAA/wAA/wAAAFdpbiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/WovA/0F+wv87fs3/UIXI/12Lxf85eMH/Q4DT/1iK0f9djrr/TYGy/0Z/uf9RhMD/UYWu/1OHr/9Bfqn/T4Gv"

-}
toUri : { width : Int, height : Int } -> Float -> String -> String
toUri =
    Internal.toUri


{-| Decode a base83 string

    decodeBase83 "X"
        --> 33

    decodeBase83 "foo"
        --> 286649

-}
decodeBase83 : String -> Int
decodeBase83 =
    Internal.decodeBase83


{-| encode an integer in base 83, the second parameter is the width: the number will be padded with 0's on the left untill that length is reached.

    encodeBase83 { padTo = 2 } 4
        --> "04"

    encodeBase83 { padTo = 4 } 4
        --> "0004"

    encodeBase83 { padTo = 4 } 420
        --> "0055"

-}
encodeBase83 : { padTo : Int } -> Int -> String
encodeBase83 { padTo } value =
    Internal.encodeBase83 value padTo


{-| Encode an image as a blurhash. The `Image` type is from [`justgook/elm-image`](https://package.elm-lang.org/packages/justgook/elm-image/latest/)
-}
fromImage : { width : Int, height : Int } -> Image.Image -> String
fromImage =
    Internal.encode


{-| Encode an array of pixel colors as a blurhash

    import Color exposing (Color)
    import Array exposing (Array)

    pixels : Array Color
    pixels =
        Array.initialize 25 (\i -> Color.rgb255 i i i)

    mask : { width : Int, height : Int }
    mask = { width = 4, height = 4}

    fromPixels mask { rows = 5, columns = 5 } pixels
        --> "U01fC^t7WB%MIUWBayWBIUWBfQWB%Mj[ayof"

-}
fromPixels : { width : Int, height : Int } -> { rows : Int, columns : Int } -> Array.Array Color -> String
fromPixels mask dimensions pixels =
    Internal.encodeCellGrid mask (CellGrid.CellGrid dimensions pixels)

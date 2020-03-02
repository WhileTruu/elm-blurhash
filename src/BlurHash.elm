module BlurHash exposing
    ( toUri
    , fromPixels
    )

{-| Display [blurhash](https://blurha.sh/)

@docs toUri


## Encoding

@docs fromPixels

-}

import Array
import CellGrid exposing (CellGrid)
import Color exposing (Color)
import Internal


{-| Convert a blurhash into an image URI. The float parameter is the `punch`,
used to increase/decrease contrast of the resulting image

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


{-| Encode an array of pixel colors as a blurhash

    import Color exposing (Color)
    import Array exposing (Array)

    pixels : Array Color
    pixels =
        Array.initialize 25 (\i -> Color.rgb255 i i i)

    mask : { width : Int, height : Int }
    mask =
        { width = 4, height = 4}

    fromPixels mask { rows = 5, columns = 5 } pixels
        --> "U01fC^t7WB%MIUWBayWBIUWBfQWB%Mj[ayof"

-}
fromPixels :
    { width : Int, height : Int }
    -> { rows : Int, columns : Int }
    -> Array.Array Color
    -> String
fromPixels mask dimensions pixels =
    Internal.encodeCellGrid mask (CellGrid.CellGrid dimensions pixels)

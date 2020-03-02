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
        "LGFFaXYk^6#M@-5c,1J5@[or[Q6."

    BlurHash.toUri { width = 3, height = 3 } punch hash
    -->  "data:image/bmp;base64,Qk1aAAAAAAAAADYAAAAoAAAAAwAAAAMAAAABABgAAAAAACQAAAATCwAAEwsAAAAAAAAAAAAAmp6Rr5BTZXaAAAAAsoyUvoRlY22PAAAAo3etuXWZiliIAAAA"

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
        { width = 4, height = 4 }

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

module Internal exposing
    ( toUri
    , toCellGrid
    , encode, encodeCellGrid, encodeHelp
    , encodeBase83, decodeBase83
    )

{-| Display blurhash

    import BlurHash
    import Html exposing (Html)
    import Html.Attributes

    main : Html msg
    main =
        let
            uri =
                BlurHash.toUri { width = 30, height = 30 }
                    1.0
                    "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"
        in
        Html.img
            [ Html.Attributes.style "width" "400px"
            , Html.Attributes.src uri
            ]
            []

@docs toUri
@docs toCellGrid
@docs encode, encodeCellGrid, encodeHelp
@docs encodeBase83, decodeBase83

-}

import Array
import Bitwise
import CellGrid exposing (CellGrid, Position)
import Color exposing (Color)
import Dict exposing (Dict)
import Image
import Image.Color


type alias Dimensions =
    { rows : Int
    , columns : Int
    }


type Triplet a
    = Triplet a a a


{-| Convert a blurhash into an image URI.

The float parameter is the `punch`, used to increase/decrease contrast of the
resulting image

    punch : Float
    punch =
        0.9

    hash : String
    hash =
        "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"

    BlurHash.toUri { width = 4, height = 4 } punch hash
    --> "data:image/bmp;base64,Qk2KAAAAAAAAAHoAAABsAAAAAgAAAAIAAAABACAAAwAAABAAAAATCwAAEwsAAAAAAAAAAAAAAAAA/wAA/wAA/wAA/wAAAFdpbiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/XY3A/z9+w/9djcD/P37D"

-}
toUri : { width : Int, height : Int } -> Float -> String -> String
toUri { width, height } punch blurhash =
    foldGrid width height punch blurhash folderList2d { row = [], rows = [] }
        |> .rows
        |> Image.Color.fromList2d
        |> Image.toBmpUrl


{-| Helper for testing
-}
toCellGrid : { width : Int, height : Int } -> Float -> String -> CellGrid Color
toCellGrid { width, height } punch blurhash =
    CellGrid.CellGrid (Dimensions height width)
        (Array.fromList (foldGrid width height punch blurhash folderList1d []))



-- Phase 1: decode metadata


{-| Create a list from the image represented by a blurhash

This type is generic so we can generate both a flat list and a 2D list by picking
a different folder and default.

-}
foldGrid :
    Int
    -> Int
    -> Float
    -> String
    -> ((Int -> Int -> Color) -> Int -> Int -> b -> b)
    -> b
    -> b
foldGrid width height punch blurhash folder default =
    let
        sizeInfo : Int
        sizeInfo =
            decodeBase83 (String.slice 0 1 blurhash)

        maximumValue : Float
        maximumValue =
            let
                a =
                    decodeBase83 (String.slice 1 2 blurhash)
            in
            (toFloat (a + 1) / 166) * punch

        dimensions : Dimensions
        dimensions =
            Dimensions height width

        filter : Dimensions
        filter =
            { rows = (sizeInfo // 9) + 1
            , columns = (sizeInfo |> modBy 9) + 1
            }

        lookup : Int -> Triplet Float
        lookup =
            buildDict filter maximumValue blurhash

        toValue : Int -> Int -> Color
        toValue row column =
            calcPixel column row width height filter lookup
    in
    foldDimensionsReversed dimensions (folder toValue) default



-- Phase 2: decoding the blurhash into a function `Int -> Triplet Float`


alphabet : String
alphabet =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"


base83chars : Dict Char Int
base83chars =
    let
        folder char { index, dict } =
            { index = index + 1
            , dict = Dict.insert char index dict
            }
    in
    alphabet
        |> String.foldl folder { index = 0, dict = Dict.empty }
        |> .dict


{-|

    decodeBase83 "X"
        --> 33

    decodeBase83 "foo"
        --> 286649

-}
decodeBase83 : String -> Int
decodeBase83 str =
    let
        folder a acc =
            case Dict.get a base83chars of
                Just v ->
                    v + acc * 83

                Nothing ->
                    acc
    in
    String.foldl folder 0 str


{-| Sign-preserving exponentiation.
-}
signPow : number -> number -> number
signPow value exp =
    if value < 0 then
        -(abs value ^ exp)

    else
        value ^ exp


decodeAC : Float -> Int -> Triplet Float
decodeAC maximumValue value =
    Triplet
        (signPow ((toFloat (floor (toFloat value / (19 * 19))) - 9.0) / 9.0) 2.0 * maximumValue)
        (signPow ((toFloat (floor (toFloat value / 19) |> modBy 19) - 9.0) / 9.0) 2.0 * maximumValue)
        (signPow ((toFloat (value |> modBy 19) - 9.0) / 9.0) 2.0 * maximumValue)


buildDict : Dimensions -> Float -> String -> (Int -> Triplet Float)
buildDict dimensions maximumValue blurhash =
    let
        cache : Dict Int (Triplet Float)
        cache =
            foldDimensions dimensions
                (\row column dict ->
                    case row * dimensions.columns + column of
                        0 ->
                            let
                                bits =
                                    decodeBase83 (String.slice 2 6 blurhash)

                                value =
                                    Triplet
                                        (srgbToLinear (bits |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF))
                                        (srgbToLinear (bits |> Bitwise.shiftRightBy 8 |> Bitwise.and 255))
                                        (srgbToLinear (bits |> Bitwise.and 255))
                            in
                            Dict.insert 0 value dict

                        i ->
                            let
                                key =
                                    String.slice (4 + i * 2) (4 + (i + 1) * 2) blurhash

                                value =
                                    decodeAC maximumValue (decodeBase83 key)
                            in
                            Dict.insert i value dict
                )
                Dict.empty
    in
    -- creating a new function with one argument here make sure that the
    -- cache is only created once, then re-used for all subsequent calls.
    -- this works because we partially apply all arguments needed above.
    \index ->
        case Dict.get index cache of
            Just v ->
                v

            Nothing ->
                Triplet 0 0 0



-- Phase 3: determine the color at a position


calcPixel : Int -> Int -> Int -> Int -> Dimensions -> (Int -> Triplet Float) -> Color
calcPixel x y width height filter lookup =
    let
        folder row column (Triplet pixel0 pixel1 pixel2) =
            let
                basis : Float
                basis =
                    cos (pi * toFloat x * toFloat column / toFloat width)
                        * cos (pi * toFloat y * toFloat row / toFloat height)

                (Triplet colour0 colour1 colour2) =
                    lookup (row * filter.columns + column)
            in
            Triplet
                (pixel0 + colour0 * basis)
                (pixel1 + colour1 * basis)
                (pixel2 + colour2 * basis)

        (Triplet r g b) =
            foldDimensions filter folder (Triplet 0 0 0)
    in
    Color.rgb255 (linearToSrgb r) (linearToSrgb g) (linearToSrgb b)



-- Color conversion


{-| srgb 0-255 integer to linear 0.0-1.0 floating point conversion.
-}
srgbToLinear : Int -> Float
srgbToLinear srgbInt =
    let
        a =
            toFloat srgbInt / 255
    in
    if a <= 0.04045 then
        a / 12.92

    else
        ((a + 0.055) / 1.055) ^ 2.4


unlinear : Color -> Color
unlinear color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    Color.rgb
        (if red <= 0.04045 then
            red / 12.92

         else
            ((red + 0.055) / 1.055) ^ 2.4
        )
        (if green <= 0.04045 then
            green / 12.92

         else
            ((green + 0.055) / 1.055) ^ 2.4
        )
        (if blue <= 0.04045 then
            blue / 12.92

         else
            ((blue + 0.055) / 1.055) ^ 2.4
        )


{-| linear 0.0-1.0 floating point to srgb 0-255 integer conversion.
-}
linearToSrgb : Float -> Int
linearToSrgb linearFloat =
    let
        a =
            clamp 0 1 linearFloat
    in
    if a <= 0.0031308 then
        floor (a * 12.92 * 255 + 0.5)

    else
        floor ((1.055 * (a ^ (1 / 2.4)) - 0.055) * 255 + 0.5)



-- Efficient folding over all positions in a grid


{-| Fold from the top-left to the bottom-right. Useful for building up arrays
-}
foldDimensions : Dimensions -> (Int -> Int -> a -> a) -> a -> a
foldDimensions { rows, columns } folder default =
    let
        go row column accum =
            if column < columns - 1 then
                go row (column + 1) (folder row column accum)

            else if row < rows - 1 then
                go (row + 1) 0 (folder row column accum)

            else
                folder row column accum
    in
    go 0 0 default


{-| Fold from the bottom-right to the top-left Useful for building up lists
-}
foldDimensionsReversed : Dimensions -> (Int -> Int -> a -> a) -> a -> a
foldDimensionsReversed { rows, columns } folder default =
    let
        go row column accum =
            if column > 0 then
                go row (column - 1) (folder row column accum)

            else if row > 0 then
                go (row - 1) (columns - 1) (folder row column accum)

            else
                folder row column accum
    in
    go (rows - 1) (columns - 1) default


{-| Build up a flat list
-}
folderList1d : (Int -> Int -> a) -> Int -> Int -> List a -> List a
folderList1d toValue row column accum =
    toValue row column :: accum


{-| Build up a 2D list
-}
folderList2d :
    (Int -> Int -> a)
    -> Int
    -> Int
    -> { row : List a, rows : List (List a) }
    -> { row : List a, rows : List (List a) }
folderList2d toValue row column r =
    let
        value =
            toValue row column
    in
    case column of
        0 ->
            { row = [], rows = (value :: r.row) :: r.rows }

        _ ->
            { row = value :: r.row, rows = r.rows }


{-| encode an integer in base 83, the second parameter is the width: the number
will be padded with 0's on the left until that length is reached.

    encodeBase83 { padTo = 2 } 4
        --> "04"

    encodeBase83 { padTo = 4 } 4
        --> "0004"

    encodeBase83 { padTo = 4 } 420
        --> "0055"

-}
encodeBase83 : Int -> Int -> String
encodeBase83 value length =
    let
        go i accum =
            if i <= length then
                let
                    digit =
                        (value // (83 ^ (length - i))) |> modBy 83
                in
                go (i + 1) (accum ++ String.slice digit (digit + 1) alphabet)

            else
                accum
    in
    go 1 ""


{-| Encode an image
-}
encode : { width : Int, height : Int } -> Image.Image -> String
encode maskSize image =
    case Image.toList2d image of
        first :: rest ->
            let
                dimensions =
                    { rows = 1 + List.length rest, columns = List.length first }
            in
            CellGrid.CellGrid dimensions (Image.Color.toArray image)
                |> encodeHelp maskSize False

        _ ->
            ""


encodeCellGrid : { width : Int, height : Int } -> CellGrid Color -> String
encodeCellGrid mask grid =
    encodeHelp mask False grid


encodeHelp : { width : Int, height : Int } -> Bool -> CellGrid Color -> String
encodeHelp maskSize_ isLinear image =
    let
        componentsSize =
            { rows = maskSize_.height
            , columns = maskSize_.width
            }

        { components, max_ac_component } =
            if isLinear then
                findComponents componentsSize image

            else
                findComponents componentsSize (CellGrid.map unlinear <| image)
    in
    case components of
        [] ->
            ""

        first :: rest ->
            let
                dc_value : Int
                dc_value =
                    let
                        r =
                            linearToSrgb first.r
                                |> Bitwise.shiftLeftBy 16

                        g =
                            linearToSrgb first.g
                                |> Bitwise.shiftLeftBy 8

                        b =
                            linearToSrgb first.b
                    in
                    r + g + b

                quant_max_ac_component : Int
                quant_max_ac_component =
                    max 0 (min 82 (floor (max_ac_component * 166 - 0.5)))

                ac_component_norm_factor : Float
                ac_component_norm_factor =
                    toFloat (quant_max_ac_component + 1) / 166.0

                ac_values : List Int
                ac_values =
                    List.map
                        (\{ r, g, b } ->
                            let
                                x =
                                    max 0 (min 18 (floor (signPow (r / ac_component_norm_factor) 0.5 * 9.0 + 9.5))) * 19 * 19

                                y =
                                    max 0 (min 18 (floor (signPow (g / ac_component_norm_factor) 0.5 * 9.0 + 9.5))) * 19

                                z =
                                    max 0 (min 18 (floor (signPow (b / ac_component_norm_factor) 0.5 * 9.0 + 9.5)))
                            in
                            x + y + z
                        )
                        rest
            in
            encodeBase83 (componentsSize.columns - 1 + (componentsSize.rows - 1) * 9) 1
                ++ encodeBase83 quant_max_ac_component 1
                ++ encodeBase83 dc_value 4
                ++ List.foldl (\new accum -> accum ++ encodeBase83 new 2) "" ac_values


findComponents :
    Dimensions
    -> CellGrid Color
    -> { components : List { r : Float, g : Float, b : Float }, max_ac_component : Float }
findComponents dimensions ((CellGrid.CellGrid grid _) as cellgrid) =
    foldDimensionsReversed dimensions
        (\row column { components, max_ac_component } ->
            let
                { r, g, b } =
                    encodePixel (Position row column) cellgrid

                size =
                    toFloat (grid.rows * grid.columns)

                normalized =
                    { r = r / size, g = g / size, b = b / size }
            in
            { components = normalized :: components
            , max_ac_component =
                if not (row == 0 && column == 0) then
                    max max_ac_component
                        (max (abs normalized.r)
                            (max (abs normalized.g) (abs normalized.b))
                        )

                else
                    max_ac_component
            }
        )
        { components = [], max_ac_component = 0 }


encodePixel : Position -> CellGrid Color -> { r : Float, g : Float, b : Float }
encodePixel position ((CellGrid.CellGrid dimensions _) as cellgrid) =
    let
        norm_factor =
            if i == 0 && j == 0 then
                1.0

            else
                2.0

        i =
            position.column

        j =
            position.row
    in
    CellGrid.foldl
        (\value { r, g, b, index } ->
            let
                pos =
                    CellGrid.matrixIndex dimensions index

                x =
                    pos.column

                y =
                    pos.row

                basis =
                    norm_factor
                        * cos (pi * toFloat i * toFloat x / toFloat dimensions.columns)
                        * cos (pi * toFloat j * toFloat y / toFloat dimensions.rows)

                { red, green, blue } =
                    Color.toRgba value

                result =
                    { r = r + basis * red
                    , g = g + basis * green
                    , b = b + basis * blue
                    , index = index + 1
                    }
            in
            result
        )
        { r = 0, g = 0, b = 0, index = 0 }
        cellgrid
        |> (\{ r, g, b } -> { r = r, g = g, b = b })

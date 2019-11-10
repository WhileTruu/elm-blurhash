module Blurhash exposing (toUri)

{-| Display blur hashes in elm

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

@docs toUri

-}

import Base64
import Bitwise
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

The float parameter is the `punch`, used to increase/decrease contrast of the resulting image

    punch : Float
    punch =
        0.9

    hash : String
    hash =
        "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"

    Blurhash.toUri { width = 4, height = 4 } punch hash
    --> "data:image/bmp;base64,Qk2KAAAAAAAAAHoAAABsAAAAAgAAAAIAAAABACAAAwAAABAAAAATCwAAEwsAAAAAAAAAAAAAAAAA/wAA/wAA/wAA/wAAAFdpbiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/XY3A/z9+w/9djcD/P37D"

-}
toUri : { width : Int, height : Int } -> Float -> String -> String
toUri { width, height } punch blurhash =
    foldGrid width height punch blurhash folderList2d { row = [], rows = [] }
        |> .rows
        |> Image.Color.fromList2d
        |> Image.encodeBmp
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> (\uri -> "data:image/bmp;base64," ++ uri)



-- Phase 1: decode metadata


{-| Create a list from the image represented by a blurhash

This type is generic so we can generate both a flat list and a 2D list by picking a different folder and default.

-}
foldGrid : Int -> Int -> Float -> String -> ((Int -> Int -> Color) -> Int -> Int -> b -> b) -> b -> b
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
            Dimensions ((sizeInfo // 9) + 1) ((sizeInfo |> modBy 9) + 1)

        lookup : Int -> Triplet Float
        lookup =
            buildDict { rows = height, columns = width } maximumValue blurhash

        toValue : Int -> Int -> Color
        toValue row column =
            calcPixel column row width height filter lookup
    in
    foldDimensionsReversed dimensions (folder toValue) default



-- Phase 2: decoding the blur hash into a function `Int -> Triplet Float`


base83chars : Dict Char Int
base83chars =
    let
        folder char { index, dict } =
            { index = index + 1
            , dict = Dict.insert char index dict
            }
    in
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"
        |> String.foldl folder { index = 0, dict = Dict.empty }
        |> .dict


{-| Decode a base83 string, as used in blurhash, to an integer.
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
        -(value ^ exp)

    else
        value ^ exp


decodeAC : Float -> Int -> Triplet Float
decodeAC maximumValue value =
    let
        a1 =
            toFloat value / (19 * 19)

        a2 =
            value // 19 |> modBy 19 |> toFloat

        a3 =
            value |> modBy 19 |> toFloat
    in
    Triplet
        (signPow ((a1 - 9) / 9) 2 * maximumValue)
        (signPow ((a2 - 9) / 9) 2 * maximumValue)
        (signPow ((a3 - 9) / 9) 2 * maximumValue)


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
                                        (srgbToLinear (bits |> Bitwise.shiftRightBy 16))
                                        (srgbToLinear (bits |> Bitwise.shiftRightBy 8 |> Bitwise.and 255))
                                        (srgbToLinear (bits |> Bitwise.and 255))
                            in
                            Dict.insert 0 value dict

                        i ->
                            let
                                key =
                                    String.slice (4 + i * 2) (4 + (i + 1) * 2) blurhash
                            in
                            Dict.insert i (decodeAC maximumValue (decodeBase83 key)) dict
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


decode : Int -> Int -> Float -> String -> List Color
decode width height punch blurhash =
    foldGrid width height punch blurhash folderList1d []


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
folderList2d : (Int -> Int -> a) -> Int -> Int -> { row : List a, rows : List (List a) } -> { row : List a, rows : List (List a) }
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

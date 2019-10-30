module Blurhash exposing (decode)

import Bitwise
import Dict exposing (Dict)
import List.Extra


{-| Blurhash decoder.

@docs decode

-}
base83chars : Dict Char Int
base83chars =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"
        |> String.toList
        |> List.indexedMap (\i char -> ( char, i ))
        |> Dict.fromList


{-| Decodes a base83 string, as used in blurhash, to an integer.
-}
decodeBase83 : String -> Int
decodeBase83 =
    List.foldl
        (\a acc ->
            Dict.get a base83chars
                |> Maybe.map ((+) (acc * 83))
                |> Maybe.withDefault acc
        )
        0
        << String.toList


{-| srgb 0-255 integer to linear 0.0-1.0 floating point conversion.
-}
srgbToLinear : Int -> Float
srgbToLinear srgbInt =
    (toFloat srgbInt / 255)
        |> (\a ->
                if a <= 0.04045 then
                    a / 12.92

                else
                    ((a + 0.055) / 1.055) ^ 2.4
           )


{-| linear 0.0-1.0 floating point to srgb 0-255 integer conversion.
-}
linearToSrgb : Float -> Int
linearToSrgb linearFloat =
    clamp 0 1 linearFloat
        |> (\a ->
                if a <= 0.0031308 then
                    floor (a * 12.92 * 255 + 0.5)

                else
                    floor ((1.055 * (a ^ (1 / 2.4)) - 0.055) * 255 + 0.5)
           )


{-| Sign-preserving exponentiation.
-}
signPow : number -> number -> number
signPow value exp =
    (value ^ exp)
        |> (\a ->
                if value < 0 then
                    a * -1

                else
                    a
           )


type alias Metadata =
    { sizeY : Int
    , sizeX : Int
    , maximumValue : Float
    }


decodeMetadata : Float -> String -> Metadata
decodeMetadata punch s =
    let
        sizeInfo : Int
        sizeInfo =
            decodeBase83 (s |> String.slice 0 1)
    in
    { sizeX = (sizeInfo |> modBy 9) + 1
    , sizeY = floor (toFloat sizeInfo / 9) + 1
    , maximumValue =
        decodeBase83 (s |> String.slice 1 2)
            |> (\a -> (toFloat (a + 1) / 166) * punch)
    }


decodeAC : Float -> Int -> ( Float, Float, Float )
decodeAC maximumValue value =
    ( toFloat value / (19 * 19)
    , toFloat value / 19 |> floor |> modBy 19 |> toFloat
    , value |> modBy 19 |> toFloat
    )
        |> (\( a1, a2, a3 ) ->
                ( signPow ((a1 - 9) / 9) 2 * maximumValue
                , signPow ((a2 - 9) / 9) 2 * maximumValue
                , signPow ((a3 - 9) / 9) 2 * maximumValue
                )
           )


{-| Decodes given blurhash to an RGB image with specified dimensions

Punch parameter can be used to increase/decrease contrast of the resulting image

-}
decode : Int -> Int -> Float -> String -> List ( Int, Int, Int )
decode width height punch blurhash =
    let
        metadata : Metadata
        metadata =
            decodeMetadata punch blurhash
    in
    List.range 0 (height * width - 1)
        |> List.map
            ((\( x, y ) ->
                calcPixel
                    { x = x
                    , y = y
                    , width = width
                    , height = height
                    , blurhash = blurhash
                    , metadata = metadata
                    }
             )
                << (\a -> ( a |> modBy width, floor (toFloat a / toFloat width) ))
            )


calcPixel :
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , blurhash : String
    , metadata : Metadata
    }
    -> ( Int, Int, Int )
calcPixel { x, y, width, height, blurhash, metadata } =
    List.foldr
        (\index ( pixel0, pixel1, pixel2 ) ->
            let
                ( i, j ) =
                    ( index |> modBy metadata.sizeX
                    , floor (toFloat index / toFloat metadata.sizeX)
                    )

                basis : Float
                basis =
                    cos (pi * toFloat x * toFloat i / toFloat width)
                        * cos (pi * toFloat y * toFloat j / toFloat height)

                ( colour0, colour1, colour2 ) =
                    getColour blurhash metadata (i + j * metadata.sizeX)
            in
            ( pixel0 + colour0 * basis
            , pixel1 + colour1 * basis
            , pixel2 + colour2 * basis
            )
        )
        ( 0, 0, 0 )
        (List.range 0 (metadata.sizeX * metadata.sizeY - 1))
        |> (\( a, b, c ) -> ( linearToSrgb a, linearToSrgb b, linearToSrgb c ))


getColour : String -> { a | maximumValue : Float } -> Int -> ( Float, Float, Float )
getColour blurHash { maximumValue } i =
    if i == 0 then
        decodeBase83 (blurHash |> String.slice 2 6)
            |> (\a ->
                    ( srgbToLinear (a |> Bitwise.shiftRightBy 16)
                    , srgbToLinear (a |> Bitwise.shiftRightBy 8 |> Bitwise.and 255)
                    , srgbToLinear (a |> Bitwise.and 255)
                    )
               )

    else
        decodeBase83 (blurHash |> String.slice (4 + i * 2) (4 + (i + 1) * 2))
            |> decodeAC maximumValue

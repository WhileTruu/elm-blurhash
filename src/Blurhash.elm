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


{-| Decodes given blurhash to an RGB image with specified dimensions

Punch parameter can be used to increase/decrease contrast of the resulting image

-}
decode : Int -> Int -> Float -> String -> List ( Int, Int, Int )
decode width height punch s =
    let
        -- Decode metadata
        sizeInfo : Int
        sizeInfo =
            decodeBase83 (s |> String.slice 0 1)

        sizeY : Int
        sizeY =
            floor (toFloat sizeInfo / 9) + 1

        sizeX : Int
        sizeX =
            (sizeInfo |> modBy 9) + 1

        quantMaxValue : Int
        quantMaxValue =
            decodeBase83 (s |> String.slice 1 2)

        realMaxValue : Float
        realMaxValue =
            (toFloat (quantMaxValue + 1) / 166) * punch

        -- Make sure we at least have the right number of characters
        -- TODO: ignore? I mean, what's the worst that could happen?
        --
        -- Decode DC component
        dcValue : Int
        dcValue =
            decodeBase83 (s |> String.slice 2 6)

        colour : ( Float, Float, Float )
        colour =
            ( srgbToLinear (dcValue |> Bitwise.shiftRightBy 16)
            , srgbToLinear (dcValue |> Bitwise.shiftRightBy 8 |> Bitwise.and 255)
            , srgbToLinear (dcValue |> Bitwise.and 255)
            )

        -- Decode AC components
        colours : List ( Float, Float, Float )
        colours =
            List.range 1 (sizeX * sizeY - 1)
                |> List.map
                    (\component ->
                        let
                            acValue : Int
                            acValue =
                                decodeBase83 (s |> String.slice (4 + component * 2) (4 + (component + 1) * 2))
                        in
                        ( signPow
                            ((toFloat (floor (toFloat acValue / (19 * 19))) - 9) / 9)
                            2
                            * realMaxValue
                        , signPow
                            ((toFloat (floor (toFloat acValue / 19) |> modBy 19) - 9) / 9)
                            2
                            * realMaxValue
                        , signPow
                            ((toFloat (acValue |> modBy 19) - 9) / 9)
                            2
                            * realMaxValue
                        )
                    )
                |> (::) colour
    in
    List.range 0 (height * width - 1)
        |> List.map
            (\index ->
                let
                    x : Int
                    x =
                        index |> modBy width

                    y : Int
                    y =
                        floor (toFloat index / toFloat width)
                in
                calcPixel
                    { x = x
                    , y = y
                    , width = width
                    , height = height
                    , sizeX = sizeX
                    , sizeY = sizeY
                    , colours = colours
                    }
            )


calcPixel :
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , sizeX : Int
    , sizeY : Int
    , colours : List ( Float, Float, Float )
    }
    -> ( Int, Int, Int )
calcPixel { x, y, width, height, sizeX, sizeY, colours } =
    List.range 0 (sizeX * sizeY - 1)
        |> List.foldr
            (\index ( pixel0, pixel1, pixel2 ) ->
                let
                    i : Int
                    i =
                        index |> modBy sizeX

                    j : Int
                    j =
                        floor (toFloat index / toFloat sizeX)

                    basis : Float
                    basis =
                        cos (pi * toFloat x * toFloat i / toFloat width)
                            * cos (pi * toFloat y * toFloat j / toFloat height)

                    -- TODO: figure out if I need the colours list
                    ( colour0, colour1, colour2 ) =
                        List.Extra.getAt (i + j * sizeX) colours
                            |> Maybe.withDefault ( 0, 0, 0 )
                in
                ( pixel0 + colour0 * basis
                , pixel1 + colour1 * basis
                , pixel2 + colour2 * basis
                )
            )
            ( 0, 0, 0 )
        |> (\( a, b, c ) ->
                ( linearToSrgb a
                , linearToSrgb b
                , linearToSrgb c
                )
           )

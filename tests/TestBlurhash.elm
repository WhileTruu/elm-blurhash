module TestBlurhash exposing (suite)

import CellGrid
import Color exposing (rgb255)
import Expect exposing (Expectation)
import Internal
import Test exposing (..)


suite : Test
suite =
    describe "Internal"
        [ describe "decoding" <|
            let
                decodeTest ({ width, height } as dim) punch input colors =
                    test (String.fromInt width ++ "x" ++ String.fromInt height ++ " @" ++ String.fromFloat punch ++ " " ++ input) <|
                        \_ ->
                            Internal.toCellGrid dim punch input
                                |> CellGrid.toLists
                                |> Expect.equal colors
            in
            [ decodeTest { width = 4, height = 4 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ rgb255 172 134 81, rgb255 173 135 83, rgb255 166 126 63, rgb255 173 129 79 ]
                , [ rgb255 185 143 95, rgb255 176 129 77, rgb255 185 126 69, rgb255 192 132 81 ]
                , [ rgb255 198 140 94, rgb255 193 119 55, rgb255 214 127 66, rgb255 211 138 89 ]
                , [ rgb255 192 140 91, rgb255 194 126 64, rgb255 207 126 57, rgb255 201 133 80 ]
                ]
            , decodeTest { width = 5, height = 5 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ rgb255 172 134 81, rgb255 173 135 84, rgb255 170 131 74, rgb255 166 124 60, rgb255 177 133 88 ]
                , [ rgb255 181 141 92, rgb255 176 134 84, rgb255 175 125 69, rgb255 183 129 71, rgb255 186 131 83 ]
                , [ rgb255 195 144 98, rgb255 187 128 74, rgb255 193 117 54, rgb255 212 136 84, rgb255 200 133 82 ]
                , [ rgb255 197 137 90, rgb255 197 124 63, rgb255 208 118 39, rgb255 220 136 80, rgb255 204 135 86 ]
                , [ rgb255 190 142 94, rgb255 190 132 78, rgb255 197 123 54, rgb255 203 129 66, rgb255 193 129 77 ]
                ]
            , decodeTest { width = 5, height = 4 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ rgb255 172 134 81, rgb255 173 135 84, rgb255 170 131 74, rgb255 166 124 60, rgb255 177 133 88 ]
                , [ rgb255 185 143 95, rgb255 178 133 82, rgb255 178 123 67, rgb255 191 131 75, rgb255 189 131 82 ]
                , [ rgb255 198 140 94, rgb255 193 124 67, rgb255 203 117 44, rgb255 220 138 84, rgb255 204 135 85 ]
                , [ rgb255 192 140 91, rgb255 193 130 72, rgb255 202 122 49, rgb255 208 131 70, rgb255 196 132 81 ]
                ]
            , decodeTest { width = 4, height = 5 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ rgb255 172 134 81, rgb255 173 135 83, rgb255 166 126 63, rgb255 173 129 79 ]
                , [ rgb255 181 141 92, rgb255 175 131 80, rgb255 179 126 68, rgb255 186 131 80 ]
                , [ rgb255 195 144 98, rgb255 186 122 64, rgb255 204 127 69, rgb255 206 137 86 ]
                , [ rgb255 197 137 90, rgb255 198 119 51, rgb255 216 127 61, rgb255 210 138 89 ]
                , [ rgb255 190 142 94, rgb255 191 129 70, rgb255 201 125 56, rgb255 197 131 76 ]
                ]
            , decodeTest { width = 5, height = 5 } 1 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" <|
                [ [ rgb255 170 134 82, rgb255 175 134 84, rgb255 173 129 73, rgb255 167 124 57, rgb255 180 134 85 ]
                , [ rgb255 181 141 92, rgb255 179 134 84, rgb255 179 125 70, rgb255 186 129 71, rgb255 190 133 83 ]
                , [ rgb255 196 143 96, rgb255 191 128 73, rgb255 198 119 54, rgb255 214 137 86, rgb255 204 134 85 ]
                , [ rgb255 197 137 86, rgb255 198 123 58, rgb255 211 118 34, rgb255 221 138 80, rgb255 206 137 88 ]
                , [ rgb255 190 142 94, rgb255 192 133 77, rgb255 200 124 53, rgb255 205 132 65, rgb255 196 132 77 ]
                ]
            , decodeTest { width = 5, height = 5 } 0.5 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" <|
                [ [ rgb255 183 133 79, rgb255 185 132 80, rgb255 184 130 75, rgb255 181 128 67, rgb255 187 132 81 ]
                , [ rgb255 188 136 85, rgb255 187 132 80, rgb255 187 128 73, rgb255 190 130 74, rgb255 192 132 80 ]
                , [ rgb255 195 137 87, rgb255 192 130 74, rgb255 196 125 66, rgb255 205 134 81, rgb255 199 133 81 ]
                , [ rgb255 195 134 81, rgb255 196 127 68, rgb255 203 125 59, rgb255 208 135 78, rgb255 200 134 82 ]
                , [ rgb255 192 137 85, rgb255 193 132 77, rgb255 197 128 66, rgb255 199 131 71, rgb255 195 131 76 ]
                ]
            , decodeTest { width = 4, height = 3 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ rgb255 172 134 81, rgb255 173 135 83, rgb255 166 126 63, rgb255 173 129 79 ]
                , [ rgb255 191 145 98, rgb255 181 125 71, rgb255 196 127 70, rgb255 201 135 84 ]
                , [ rgb255 195 137 89, rgb255 198 122 55, rgb255 214 127 59, rgb255 207 136 86 ]
                ]
            , decodeTest { width = 3, height = 4 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ rgb255 172 134 81, rgb255 172 133 80, rgb255 168 125 65 ]
                , [ rgb255 185 143 95, rgb255 176 124 70, rgb255 193 133 79 ]
                , [ rgb255 198 140 94, rgb255 197 114 40, rgb255 218 140 90 ]
                , [ rgb255 192 140 91, rgb255 198 122 52, rgb255 207 133 76 ]
                ]
            , decodeTest { width = 3, height = 3 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ rgb255 172 134 81, rgb255 172 133 80, rgb255 168 125 65 ]
                , [ rgb255 191 145 98, rgb255 181 119 61, rgb255 205 137 84 ]
                , [ rgb255 195 137 89, rgb255 202 118 41, rgb255 214 137 83 ]
                ]
            ]
        , describe "encoding" <|
            let
                encodeTest ({ width, height } as dim) punch input size output =
                    test (String.fromInt width ++ "x" ++ String.fromInt height ++ " @" ++ String.fromFloat punch ++ " " ++ input) <|
                        \_ ->
                            Internal.toCellGrid dim punch input
                                |> Internal.encodeHelp size False
                                |> Expect.equal output
            in
            [ encodeTest { width = 30, height = 30 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" { width = 4, height = 4 } "UBL_:rx@GG^jP9R*w_WB2skV$jM{=wnOWYR+"
            , encodeTest { width = 30, height = 30 } 1 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" { width = 4, height = 4 } "UCMOZftPGG^jO?R*w_WB2sbv$jNG-SjEWYS2"
            , encodeTest { width = 30, height = 30 } 0.5 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" { width = 4, height = 4 } "U7MOZf-nKi~Ax[WUw_ae70bbxGNG^NniWqWV"
            , encodeTest { width = 20, height = 30 } 0.75 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" { width = 4, height = 5 } "dAMOZf?EKj~Ab^WUw_WB70bvxGNG=vniWXWVWpayjtay"
            ]
        , describe "base83 encoding" <|
            let
                base83Test value length expected =
                    test (String.fromInt value ++ " -> " ++ expected) <|
                        \_ ->
                            Internal.encodeBase83 value length
                                |> Expect.equal expected
            in
            [ base83Test 0 2 "00"
            , base83Test 20 2 "0K"
            , base83Test 83 4 "0010"
            ]
        ]

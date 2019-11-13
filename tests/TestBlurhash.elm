module TestBlurhash exposing (suite)

import CellGrid
import Color
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal
import Test exposing (..)


suite : Test
suite =
    describe "Internal"
        [ describe "decoding" <|
            let
                decodeTest dim punch input colors =
                    test (Debug.toString dim ++ " @" ++ String.fromFloat punch ++ " " ++ input) <|
                        \_ ->
                            Internal.toCellGrid dim punch input
                                |> CellGrid.toLists
                                |> Expect.equal colors
            in
            [ decodeTest { width = 4, height = 4 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ Color.rgb255 172 134 81, Color.rgb255 173 135 83, Color.rgb255 166 126 63, Color.rgb255 173 129 79 ]
                , [ Color.rgb255 185 143 95, Color.rgb255 176 129 77, Color.rgb255 185 126 69, Color.rgb255 192 132 81 ]
                , [ Color.rgb255 198 140 94, Color.rgb255 193 119 55, Color.rgb255 214 127 66, Color.rgb255 211 138 89 ]
                , [ Color.rgb255 192 140 91, Color.rgb255 194 126 64, Color.rgb255 207 126 57, Color.rgb255 201 133 80 ]
                ]
            , decodeTest { width = 5, height = 5 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ Color.rgb255 172 134 81, Color.rgb255 173 135 84, Color.rgb255 170 131 74, Color.rgb255 166 124 60, Color.rgb255 177 133 88 ]
                , [ Color.rgb255 181 141 92, Color.rgb255 176 134 84, Color.rgb255 175 125 69, Color.rgb255 183 129 71, Color.rgb255 186 131 83 ]
                , [ Color.rgb255 195 144 98, Color.rgb255 187 128 74, Color.rgb255 193 117 54, Color.rgb255 212 136 84, Color.rgb255 200 133 82 ]
                , [ Color.rgb255 197 137 90, Color.rgb255 197 124 63, Color.rgb255 208 118 39, Color.rgb255 220 136 80, Color.rgb255 204 135 86 ]
                , [ Color.rgb255 190 142 94, Color.rgb255 190 132 78, Color.rgb255 197 123 54, Color.rgb255 203 129 66, Color.rgb255 193 129 77 ]
                ]
            , decodeTest { width = 5, height = 4 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ Color.rgb255 172 134 81, Color.rgb255 173 135 84, Color.rgb255 170 131 74, Color.rgb255 166 124 60, Color.rgb255 177 133 88 ]
                , [ Color.rgb255 185 143 95, Color.rgb255 178 133 82, Color.rgb255 178 123 67, Color.rgb255 191 131 75, Color.rgb255 189 131 82 ]
                , [ Color.rgb255 198 140 94, Color.rgb255 193 124 67, Color.rgb255 203 117 44, Color.rgb255 220 138 84, Color.rgb255 204 135 85 ]
                , [ Color.rgb255 192 140 91, Color.rgb255 193 130 72, Color.rgb255 202 122 49, Color.rgb255 208 131 70, Color.rgb255 196 132 81 ]
                ]
            , decodeTest { width = 4, height = 5 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ Color.rgb255 172 134 81, Color.rgb255 173 135 83, Color.rgb255 166 126 63, Color.rgb255 173 129 79 ]
                , [ Color.rgb255 181 141 92, Color.rgb255 175 131 80, Color.rgb255 179 126 68, Color.rgb255 186 131 80 ]
                , [ Color.rgb255 195 144 98, Color.rgb255 186 122 64, Color.rgb255 204 127 69, Color.rgb255 206 137 86 ]
                , [ Color.rgb255 197 137 90, Color.rgb255 198 119 51, Color.rgb255 216 127 61, Color.rgb255 210 138 89 ]
                , [ Color.rgb255 190 142 94, Color.rgb255 191 129 70, Color.rgb255 201 125 56, Color.rgb255 197 131 76 ]
                ]
            , decodeTest { width = 5, height = 5 } 1 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" <|
                [ [ Color.rgb255 170 134 82, Color.rgb255 175 134 84, Color.rgb255 173 129 73, Color.rgb255 167 124 57, Color.rgb255 180 134 85 ]
                , [ Color.rgb255 181 141 92, Color.rgb255 179 134 84, Color.rgb255 179 125 70, Color.rgb255 186 129 71, Color.rgb255 190 133 83 ]
                , [ Color.rgb255 196 143 96, Color.rgb255 191 128 73, Color.rgb255 198 119 54, Color.rgb255 214 137 86, Color.rgb255 204 134 85 ]
                , [ Color.rgb255 197 137 86, Color.rgb255 198 123 58, Color.rgb255 211 118 34, Color.rgb255 221 138 80, Color.rgb255 206 137 88 ]
                , [ Color.rgb255 190 142 94, Color.rgb255 192 133 77, Color.rgb255 200 124 53, Color.rgb255 205 132 65, Color.rgb255 196 132 77 ]
                ]
            , decodeTest { width = 5, height = 5 } 0.5 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" <|
                [ [ Color.rgb255 183 133 79, Color.rgb255 185 132 80, Color.rgb255 184 130 75, Color.rgb255 181 128 67, Color.rgb255 187 132 81 ]
                , [ Color.rgb255 188 136 85, Color.rgb255 187 132 80, Color.rgb255 187 128 73, Color.rgb255 190 130 74, Color.rgb255 192 132 80 ]
                , [ Color.rgb255 195 137 87, Color.rgb255 192 130 74, Color.rgb255 196 125 66, Color.rgb255 205 134 81, Color.rgb255 199 133 81 ]
                , [ Color.rgb255 195 134 81, Color.rgb255 196 127 68, Color.rgb255 203 125 59, Color.rgb255 208 135 78, Color.rgb255 200 134 82 ]
                , [ Color.rgb255 192 137 85, Color.rgb255 193 132 77, Color.rgb255 197 128 66, Color.rgb255 199 131 71, Color.rgb255 195 131 76 ]
                ]
            , decodeTest { width = 4, height = 3 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ Color.rgb255 172 134 81, Color.rgb255 173 135 83, Color.rgb255 166 126 63, Color.rgb255 173 129 79 ]
                , [ Color.rgb255 191 145 98, Color.rgb255 181 125 71, Color.rgb255 196 127 70, Color.rgb255 201 135 84 ]
                , [ Color.rgb255 195 137 89, Color.rgb255 198 122 55, Color.rgb255 214 127 59, Color.rgb255 207 136 86 ]
                ]
            , decodeTest { width = 3, height = 4 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ Color.rgb255 172 134 81, Color.rgb255 172 133 80, Color.rgb255 168 125 65 ]
                , [ Color.rgb255 185 143 95, Color.rgb255 176 124 70, Color.rgb255 193 133 79 ]
                , [ Color.rgb255 198 140 94, Color.rgb255 197 114 40, Color.rgb255 218 140 90 ]
                , [ Color.rgb255 192 140 91, Color.rgb255 198 122 52, Color.rgb255 207 133 76 ]
                ]
            , decodeTest { width = 3, height = 3 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH" <|
                [ [ Color.rgb255 172 134 81, Color.rgb255 172 133 80, Color.rgb255 168 125 65 ]
                , [ Color.rgb255 191 145 98, Color.rgb255 181 119 61, Color.rgb255 205 137 84 ]
                , [ Color.rgb255 195 137 89, Color.rgb255 202 118 41, Color.rgb255 214 137 83 ]
                ]
            ]
        , describe "encoding" <|
            let
                encodeTest dim punch input size output =
                    test (Debug.toString dim ++ " @" ++ String.fromFloat punch ++ " " ++ input) <|
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

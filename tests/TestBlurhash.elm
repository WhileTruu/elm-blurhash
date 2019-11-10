module TestBlurhash exposing (suite)

import Blurhash
import Color
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Blurhash"
        [ test "example (punch=0.5)" <|
            \_ ->
                Blurhash.toUri { width = 5, height = 5 } 0.5 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"
                    |> Expect.equal "data:image/bmp;base64,Qk3eAAAAAAAAAHoAAABsAAAABQAAAAUAAAABACAAAwAAAGQAAAATCwAAEwsAAAAAAAAAAAAAAAAA/wAA/wAA/wAA/wAAAFdpbiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/VYjA/02Dv/9Cf8H/R4LF/02CwP9ThsL/Rn/B/z18x/9Ohc7/UYXF/1iJwf9Lgbz/Qny//1CFyf9Pg8P/VIi9/1CEuf9JgLb/SYG7/1CDvf9PhLz/UIW5/0uCtP9Ef7T/UoO6"
        , test "example (punch=1.0)" <|
            \_ ->
                Blurhash.toUri { width = 5, height = 5 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"
                    |> Expect.equal "data:image/bmp;base64,Qk3eAAAAAAAAAHoAAABsAAAABQAAAAUAAAABACAAAwAAAGQAAAATCwAAEwsAAAAAAAAAAAAAAAAA/wAA/wAA/wAA/wAAAFdpbiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/Xo7B/06Evv82e8P/QoHL/02Bwf9aicb/P3zD/yd2zv9QiNz/VofL/2KQw/9KgLr/NnW//1SI0/9Shcf/XI28/1SGs/9Ffa3/R4G4/1ODu/9Rhrn/VIez/0qDqP88fKj/WIW2"
        , test "example { width = 4, height = 6 }" <|
            \_ ->
                Blurhash.toUri { width = 4, height = 6 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"
                    |> Expect.equal "data:image/bmp;base64,Qk3aAAAAAAAAAHoAAABsAAAABAAAAAYAAAABACAAAwAAAGAAAAATCwAAEwsAAAAAAAAAAAAAAAAA/wAA/wAA/wAA/wAAAFdpbiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/YJDA/0qCvP84fcT/SYHC/1mJxf83esT/O3/V/1aIz/9ejMX/N3e//0J/1P9ZitL/YpHB/0d9s/9Gf8L/VIfI/1mLu/9RhLD/Q36u/1CDuf9Rhrn/U4ex/z9+pv9PgbI="
        , test "example { width = 2, height = 2 }" <|
            \_ ->
                Blurhash.toUri { width = 2, height = 2 } 1 "UBL_:rOpGG-oBUNG,qRj2so|=eE1w^n4S5NH"
                    |> Expect.equal "data:image/bmp;base64,Qk2KAAAAAAAAAHoAAABsAAAAAgAAAAIAAAABACAAAwAAABAAAAATCwAAEwsAAAAAAAAAAAAAAAAA/wAA/wAA/wAA/wAAAFdpbiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/XY3A/z9+w/9djcD/P37D"
        ]


toRgb255 color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    ( round (red * 255)
    , round (green * 255)
    , round (blue * 255)
    )


toColor ( a, b, c ) =
    Color.rgb255 a b c

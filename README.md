# BlurHash

Display blurhash in elm. Blurhash is ["A very compact representation of a placeholder for an image"](https://blurha.sh/).
The hash is sent as a small string (for example in a json payload) and can be loaded more quickly than a large image file.

![the blurhash process](https://github.com/woltapp/blurhash/raw/master/Media/HowItWorks1.jpg)


This package turns the blurhash string into an image, which can be loaded into a document as a base64-encoded uri.

```elm
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
```

An encoder is also included!

## Demo

[Webpage](https://whiletruu.github.io/elm-blurhash/)

[Code](https://github.com/WhileTruu/elm-blurhash/tree/publish-package/example)
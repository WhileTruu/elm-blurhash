module Main exposing (main)

import Browser exposing (Document)
import Html as H exposing (Html)


main : Platform.Program () () Never
main =
    Browser.sandbox
        { init = ()
        , update = \_ _ -> ()
        , view = \_ -> view
        }



-- VIEW


view : Html Never
view =
    H.div [] [ H.text "Hello world" ]

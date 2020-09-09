module UI exposing (buttonAttributes)

import Element exposing (Attribute, mouseOver, paddingXY, rgb255)
import Element.Background as Background
import Element.Border as Border


buttonAttributes : List (Attribute msg)
buttonAttributes =
    [ Border.width 1
    , paddingXY 20 10
    , Border.rounded 3
    , mouseOver [ Background.color <| rgb255 240 240 240 ]
    ]

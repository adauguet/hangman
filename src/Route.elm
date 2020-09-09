module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Browser.Navigation as Nav exposing (Key)
import Html exposing (Attribute)
import Html.Attributes
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = Home
    | Solo
    | Multi


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Solo (s "solo")
        , map Multi (s "multi")
        ]


href : Route -> Attribute msg
href route =
    Html.Attributes.href (toString route)


pushUrl : Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toString route)


replaceUrl : Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toString route)


fromUrl : Url -> Route
fromUrl url =
    parse parser url |> Maybe.withDefault Home


toString : Route -> String
toString route =
    "/" ++ String.join "/" (toPieces route)


toPieces : Route -> List String
toPieces route =
    case route of
        Home ->
            []

        Solo ->
            [ "solo" ]

        Multi ->
            [ "multi" ]

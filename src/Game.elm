module Game exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Browser.Navigation exposing (Key)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , px
        , rgb
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as D exposing (Decoder)
import Route exposing (Route)
import Set exposing (Set)
import UI
import Word exposing (Word)


type alias Model =
    { key : Key
    , word : Word
    , triedChars : Set Char
    , replayRoute : Route
    , backRoute : Route
    }


init : Key -> Word -> Route -> Route -> Model
init key word replayRoute backRoute =
    { key = key
    , word = word
    , triedChars = Set.empty
    , replayRoute = replayRoute
    , backRoute = backRoute
    }


{-| Computes the score
-}
score : Word -> Set Char -> Int
score word triedChars =
    10 - Set.size (Set.diff triedChars <| Word.toSet word)


isGuessed : Set Char -> Char -> Bool
isGuessed triedChars char =
    Set.member char triedChars


type State
    = Playing
    | Lost
    | Won


{-| Computes the state
-}
state : Word -> Set Char -> State
state word triedChars =
    if score word triedChars == 0 then
        Lost

    else if Word.all (isGuessed triedChars) word then
        Won

    else
        Playing



-- update


type Msg
    = OnKeyPressed Char
    | OnClickKey Char
    | ClickedReplay
    | ClickedHome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnKeyPressed char ->
            case state model.word model.triedChars of
                Playing ->
                    ( { model | triedChars = Set.insert char model.triedChars }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnClickKey char ->
            ( { model | triedChars = Set.insert char model.triedChars }, Cmd.none )

        ClickedReplay ->
            ( model, Route.pushUrl model.key model.replayRoute )

        ClickedHome ->
            ( model, Route.pushUrl model.key Route.Home )



-- view


view : Model -> Element Msg
view model =
    case state model.word model.triedChars of
        Playing ->
            column
                [ centerX, centerY, spacing 50 ]
                [ wordView letterView model.word model.triedChars
                , imageView <| score model.word model.triedChars
                , keyboard OnClickKey model.triedChars
                ]

        Won ->
            column [ centerX, centerY, spacing 50 ]
                [ wordView letterView model.word model.triedChars
                , imageView <| score model.word model.triedChars
                , column
                    [ centerX, spacing 30, height <| px 150 ]
                    [ el [ centerX ] <| text "Vous avez gagné !"
                    , column [ spacing 15, centerX ]
                        [ buttonReplay ClickedReplay
                        , buttonHome ClickedHome
                        ]
                    ]
                ]

        Lost ->
            column [ centerX, centerY, spacing 50 ]
                [ wordView letterLostView model.word model.triedChars
                , imageView <| score model.word model.triedChars
                , column
                    [ centerX, spacing 30, height <| px 150 ]
                    [ el [ centerX ] <| text "Vous avez perdu !"
                    , column [ spacing 15, centerX ]
                        [ buttonReplay ClickedReplay
                        , buttonHome ClickedHome
                        ]
                    ]
                ]


wordView : (Set Char -> Char -> Element msg) -> Word -> Set Char -> Element msg
wordView letterToView word triedChars =
    row
        [ centerX
        , spacing 10
        , Font.size 40
        , Font.family
            [ Font.typeface "DM Mono"
            , Font.monospace
            ]
        ]
        (Word.map (letterToView triedChars) word)


letterView : Set Char -> Char -> Element msg
letterView triedChars char =
    let
        letter =
            if isGuessed triedChars char then
                String.fromChar char

            else
                "_"
    in
    el [ width <| px 30 ] <| el [ centerX ] <| text letter


letterLostView : Set Char -> Char -> Element msg
letterLostView triedChars letter =
    if isGuessed triedChars letter then
        el [ width <| px 30 ] <| el [ centerX ] <| text (String.fromChar letter)

    else
        el [ width <| px 30, Font.color <| rgb 220 0 0 ] <| el [ centerX ] <| text (String.fromChar letter)


imageView : Int -> Element msg
imageView c =
    image [ centerX ]
        { src = "images/step" ++ String.fromInt c ++ ".svg"
        , description = ""
        }


buttonHome : msg -> Element msg
buttonHome msg =
    Input.button (width fill :: UI.buttonAttributes)
        { onPress = Just msg
        , label = el [ centerX ] <| text "Home"
        }


buttonReplay : msg -> Element msg
buttonReplay msg =
    Input.button (width fill :: UI.buttonAttributes)
        { onPress = Just msg
        , label = el [ centerX ] <| text "Rejouer"
        }


keyboard : (Char -> msg) -> Set Char -> Element msg
keyboard clickedKey triedChars =
    column [ spacing 5, height <| px 150 ]
        [ row [ spacing 5 ]
            (List.range 65 77
                |> List.map Char.fromCode
                |> List.map (\c -> keyView clickedKey c (Set.member c triedChars))
            )
        , row [ spacing 5 ]
            (List.range 78 90
                |> List.map Char.fromCode
                |> List.map (\c -> keyView clickedKey c (Set.member c triedChars))
            )
        ]


keyView : (Char -> msg) -> Char -> Bool -> Element msg
keyView clickedKey char isDisabled =
    let
        ( backgroundColor, msg ) =
            if isDisabled then
                ( Element.rgb255 200 200 200, Nothing )

            else
                ( Element.rgb255 255 255 255, Just <| clickedKey char )
    in
    Input.button
        [ width <| px 50
        , height <| px 50
        , Border.width 1
        , Border.rounded 3
        , Background.color backgroundColor
        , Font.family
            [ Font.typeface "DM Mono"
            , Font.monospace
            ]
        ]
        { onPress = msg
        , label = el [ centerX ] <| text <| String.fromChar char
        }



-- subscriptions


subscriptions : Sub Msg
subscriptions =
    Browser.Events.onKeyPress keyDecoder


keyDecoder : Decoder Msg
keyDecoder =
    D.field "key" D.string
        |> D.andThen
            (\string ->
                case String.uncons string of
                    Just ( char, "" ) ->
                        if Char.isAlpha char then
                            D.succeed (OnKeyPressed (Char.toUpper char))

                        else
                            D.fail "failed to decode letter char"

                    _ ->
                        D.fail "failed to decode char"
            )

module Multi exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , column
        , spacing
        , text
        )
import Element.Input as Input
import Game
import Route
import UI
import Word


type alias Model =
    { key : Key
    , step : Step
    }


type Step
    = Input String
    | Game Game.Model


init : Key -> Model
init key =
    { key = key
    , step = Input ""
    }



-- update


type Msg
    = OnInput String
    | ClickedValid
    | OnGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.step, msg ) of
        ( Input _, OnInput string ) ->
            case parseInput string of
                Just validString ->
                    ( { model | step = Input validString }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( Input string, ClickedValid ) ->
            case Word.new string of
                Just word ->
                    ( { model | step = Game <| Game.init model.key word Route.Multi Route.Multi }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( Game subModel, OnGameMsg subMsg ) ->
            let
                ( m, cmd ) =
                    Game.update subMsg subModel
            in
            ( { model | step = Game m }, Cmd.map OnGameMsg cmd )

        _ ->
            ( model, Cmd.none )


parseInput : String -> Maybe String
parseInput string =
    if List.all Char.isAlpha (String.toList string) && String.length string <= 15 then
        Just string

    else
        Nothing



-- view


view : Model -> Element Msg
view model =
    case model.step of
        Input word ->
            column
                [ centerX, centerY, spacing 30 ]
                [ text "Quel est le mot à faire deviner ?"
                , Input.text []
                    { onChange = OnInput
                    , text = word
                    , placeholder = Nothing
                    , label = Input.labelHidden ""
                    }
                , Input.button (alignRight :: UI.buttonAttributes)
                    { onPress =
                        if String.length word < 3 then
                            Nothing

                        else
                            Just ClickedValid
                    , label = text "Valider"
                    }
                ]

        Game subModel ->
            Game.view subModel
                |> Element.map OnGameMsg



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.step of
        Input _ ->
            Sub.none

        Game _ ->
            Game.subscriptions |> Sub.map OnGameMsg

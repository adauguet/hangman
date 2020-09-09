module Home exposing (Model, Msg, init, update, view)

import Browser.Navigation exposing (Key)
import Element exposing (Element, centerX, centerY, column, el, fill, spacing, text, width)
import Element.Input as Input
import Route
import UI



-- model


type alias Model =
    { key : Key
    , isSoloEnabled : Bool
    }


init : Key -> Bool -> ( Model, Cmd Msg )
init key isSoloEnabled =
    ( { key = key
      , isSoloEnabled = isSoloEnabled
      }
    , Cmd.none
    )



-- update


type Msg
    = ClickedSolo
    | ClickedMulti


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSolo ->
            ( model, Route.pushUrl model.key Route.Solo )

        ClickedMulti ->
            ( model, Route.pushUrl model.key Route.Multi )



-- view


view : Model -> Element Msg
view model =
    column [ centerX, centerY, spacing 30 ]
        [ el [ centerX ] <| text "Menu"
        , column [ spacing 20 ]
            [ Input.button (width fill :: UI.buttonAttributes)
                { onPress =
                    if model.isSoloEnabled then
                        Just ClickedSolo

                    else
                        Nothing
                , label = el [ centerX ] <| text "Solo"
                }
            , Input.button (width fill :: UI.buttonAttributes)
                { onPress = Just ClickedMulti
                , label = el [ centerX ] <| text "2 joueurs"
                }
            ]
        ]

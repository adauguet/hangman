module Solo exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Element exposing (Element, none)
import Game
import Random
import Route
import Word exposing (Word)


type alias Model =
    { key : Key
    , state : State
    }


type State
    = Loading
    | Loaded ( String, List String ) Game.Model


init : Key -> ( String, List String ) -> ( Model, Cmd Msg )
init key ( head, tail ) =
    ( { key = key
      , state = Loading
      }
    , generateRandomWord ( head, tail )
    )



-- update


type Msg
    = GotGameMsg Game.Msg
    | GotRandomWord ( String, List String ) (Maybe Word)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGameMsg subMsg ->
            case model.state of
                Loading ->
                    ( model, Cmd.none )

                Loaded words subModel ->
                    let
                        ( m, cmd ) =
                            Game.update subMsg subModel
                    in
                    ( { model | state = Loaded words m }, Cmd.map GotGameMsg cmd )

        GotRandomWord words (Just word) ->
            ( { model | state = Loaded words <| Game.init model.key word Route.Solo Route.Home }, Cmd.none )

        GotRandomWord _ Nothing ->
            ( model, Cmd.none )


generateRandomWord : ( String, List String ) -> Cmd Msg
generateRandomWord ( head, tail ) =
    Random.uniform head tail
        |> Random.map Word.new
        |> Random.generate (GotRandomWord ( head, tail ))



-- view


view : Model -> Element Msg
view model =
    case model.state of
        Loading ->
            none

        Loaded _ subModel ->
            Game.view subModel |> Element.map GotGameMsg



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Loading ->
            Sub.none

        Loaded _ _ ->
            Game.subscriptions |> Sub.map GotGameMsg

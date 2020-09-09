module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Element exposing (layout, none)
import Home
import Json.Decode as D exposing (Decoder, Value)
import Multi
import Route exposing (Route)
import Solo
import Url exposing (Url)



-- model


type alias Model =
    { key : Key
    , page : Page
    , words : Words
    }


type Words
    = Loaded ( String, List String )
    | Error


type Page
    = Home Home.Model
    | Solo Solo.Model
    | Multi Multi.Model
    | Redirect


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    { key = key
    , page = Redirect
    , words =
        case D.decodeValue decoder value of
            Ok words ->
                Loaded words

            Err _ ->
                Error
    }
        |> changeRouteTo (Route.fromUrl url)


decoder : Decoder ( String, List String )
decoder =
    D.list D.string
        |> D.andThen
            (\list ->
                case list of
                    [] ->
                        D.fail "could not decode empty list"

                    x :: xs ->
                        D.succeed ( x, xs )
            )



-- update


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
    | OnHomeMsg Home.Msg
    | OnSoloMsg Solo.Msg
    | OnMultiMsg Multi.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, ClickedLink urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Route.pushUrl model.key (Route.fromUrl url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        ( _, ChangedUrl url ) ->
            changeRouteTo (Route.fromUrl url) model

        ( Home subModel, OnHomeMsg subMsg ) ->
            Home.update subMsg subModel
                |> updateWith model Home OnHomeMsg

        ( Solo subModel, OnSoloMsg subMsg ) ->
            Solo.update subMsg subModel
                |> updateWith model Solo OnSoloMsg

        ( Multi subModel, OnMultiMsg subMsg ) ->
            Multi.update subMsg subModel
                |> updateWith model Multi OnMultiMsg

        _ ->
            ( model, Cmd.none )


updateWith : Model -> (page -> Page) -> (msg -> Msg) -> ( page, Cmd msg ) -> ( Model, Cmd Msg )
updateWith model toPage toMsg ( subModel, subCmd ) =
    ( { model | page = toPage subModel }, Cmd.map toMsg subCmd )


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Route.Home ->
            let
                isSoloEnabled =
                    case model.words of
                        Loaded _ ->
                            True

                        _ ->
                            False
            in
            Home.init model.key isSoloEnabled
                |> updateWith model Home OnHomeMsg

        Route.Solo ->
            case model.words of
                Loaded words ->
                    Solo.init model.key words
                        |> updateWith model Solo OnSoloMsg

                _ ->
                    ( { model | page = Redirect }, Route.replaceUrl model.key Route.Home )

        Route.Multi ->
            ( { model | page = Multi <| Multi.init model.key }, Cmd.none )



-- view


view : Model -> Document Msg
view model =
    let
        body =
            case model.page of
                Home subModel ->
                    Home.view subModel |> Element.map OnHomeMsg

                Multi subModel ->
                    Multi.view subModel |> Element.map OnMultiMsg

                Solo subModel ->
                    Solo.view subModel |> Element.map OnSoloMsg

                Redirect ->
                    none
    in
    { title = "Pendu", body = [ Element.layout [] body ] }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Solo subModel ->
            Solo.subscriptions subModel |> Sub.map OnSoloMsg

        Multi subModel ->
            Multi.subscriptions subModel |> Sub.map OnMultiMsg

        _ ->
            Sub.none



-- main


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }

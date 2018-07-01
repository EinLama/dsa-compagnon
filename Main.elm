module Main exposing (..)

import Html exposing (Html, input, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { kl : Int
    , ko : Int
    , ff : Int
    , kk : Int
    , mu : Int
    , ge : Int
    , in_ : Int
    , ch : Int
    }


model : Model
model =
    { mu = 13
    , kl = 14
    , in_ = 14
    , ch = 15
    , ff = 13
    , ge = 12
    , ko = 11
    , kk = 9
    }


type Msg
    = NoOp
    | ChangeKl String
    | ChangeKo String
    | ChangeIn String
    | ChangeMu String
    | ChangeFf String
    | ChangeGe String
    | ChangeCh String
    | ChangeKk String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeKl val ->
            ( { model | kl = parseIntWithDefault val }, Cmd.none )

        ChangeIn val ->
            ( { model | in_ = parseIntWithDefault val }, Cmd.none )

        ChangeKo val ->
            ( { model | ko = parseIntWithDefault val }, Cmd.none )

        ChangeMu val ->
            ( { model | mu = parseIntWithDefault val }, Cmd.none )

        ChangeFf val ->
            ( { model | ff = parseIntWithDefault val }, Cmd.none )

        ChangeGe val ->
            ( { model | ge = parseIntWithDefault val }, Cmd.none )

        ChangeCh val ->
            ( { model | ch = parseIntWithDefault val }, Cmd.none )

        ChangeKk val ->
            ( { model | kk = parseIntWithDefault val }, Cmd.none )


parseIntWithDefault : String -> Int
parseIntWithDefault str =
    String.toInt str |> Result.withDefault 0


view : Model -> Html Msg
view model =
    div []
        [ renderAttributes model
        , div [] [ text (toString model) ]
        ]


renderAttributes : Model -> Html Msg
renderAttributes model =
    div []
        [ renderFieldForTrait "MU" model.mu ChangeMu NoOp
        , renderFieldForTrait "KL" model.kl ChangeKl NoOp
        , renderFieldForTrait "IN" model.in_ ChangeIn NoOp
        , renderFieldForTrait "CH" model.ch ChangeCh NoOp
        , renderFieldForTrait "FF" model.ff ChangeFf NoOp
        , renderFieldForTrait "GE" model.ge ChangeGe NoOp
        , renderFieldForTrait "KO" model.ko ChangeKo NoOp
        , renderFieldForTrait "KK" model.kk ChangeKk NoOp
        ]


renderFieldForTrait : String -> Int -> (String -> msg) -> msg -> Html msg
renderFieldForTrait traitLabel trait changeEvent rollEvent =
    div []
        [ text traitLabel
        , input [ placeholder traitLabel, type_ "number", onInput changeEvent, value (toString trait) ] []
        , button [ onClick rollEvent ] [ text ("Roll " ++ traitLabel) ]
        ]

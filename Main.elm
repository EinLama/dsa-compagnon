module Main exposing (..)

import Html exposing (Html, input, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random


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


type Trait
    = Kl
    | Ko
    | Ff
    | Kk
    | Mu
    | Ge
    | In
    | Ch


type alias CharacterTrait =
    { trait : Trait
    , value : Int
    , rolls : List Int
    }


updateTraitValue : Int -> CharacterTrait -> CharacterTrait
updateTraitValue value trait =
    { trait | value = value }


getTraitValue : CharacterTrait -> Int
getTraitValue { value } =
    value


addTraitRoll : Int -> CharacterTrait -> CharacterTrait
addTraitRoll value ({ rolls } as trait) =
    { trait | rolls = value :: rolls }


type alias Model =
    { kl : CharacterTrait
    , ko : CharacterTrait
    , ff : CharacterTrait
    , kk : CharacterTrait
    , mu : CharacterTrait
    , ge : CharacterTrait
    , in_ : CharacterTrait
    , ch : CharacterTrait
    }


model : Model
model =
    { mu = CharacterTrait Mu 13 []
    , kl = CharacterTrait Kl 14 []
    , in_ = CharacterTrait In 14 []
    , ch = CharacterTrait Ch 15 []
    , ff = CharacterTrait Ff 13 []
    , ge = CharacterTrait Ge 12 []
    , ko = CharacterTrait Ko 11 []
    , kk = CharacterTrait Kk 9 []
    }


type Msg
    = NoOp
    | Roll Trait
    | Rolled Trait Int
    | Change Trait String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Roll trait ->
            ( model, Random.generate (Rolled trait) (Random.int 1 20) )

        Rolled trait result ->
            case trait of
                Kl ->
                    ( { model | kl = addTraitRoll result model.kl }, Cmd.none )

                Mu ->
                    ( { model | mu = addTraitRoll result model.mu }, Cmd.none )

                In ->
                    ( { model | in_ = addTraitRoll result model.in_ }, Cmd.none )

                Ch ->
                    ( { model | ch = addTraitRoll result model.ch }, Cmd.none )

                Ff ->
                    ( { model | ff = addTraitRoll result model.ff }, Cmd.none )

                Ge ->
                    ( { model | ge = addTraitRoll result model.ge }, Cmd.none )

                Ko ->
                    ( { model | ko = addTraitRoll result model.ko }, Cmd.none )

                Kk ->
                    ( { model | kk = addTraitRoll result model.kk }, Cmd.none )

        Change trait value ->
            case trait of
                Kl ->
                    ( { model | kl = updateTraitValue (parseIntWithDefault value) model.kl }, Cmd.none )

                Mu ->
                    ( { model | mu = updateTraitValue (parseIntWithDefault value) model.mu }, Cmd.none )

                In ->
                    ( { model | in_ = updateTraitValue (parseIntWithDefault value) model.in_ }, Cmd.none )

                Ch ->
                    ( { model | ch = updateTraitValue (parseIntWithDefault value) model.ch }, Cmd.none )

                Ff ->
                    ( { model | ff = updateTraitValue (parseIntWithDefault value) model.ff }, Cmd.none )

                Ge ->
                    ( { model | ge = updateTraitValue (parseIntWithDefault value) model.ge }, Cmd.none )

                Ko ->
                    ( { model | ko = updateTraitValue (parseIntWithDefault value) model.ko }, Cmd.none )

                Kk ->
                    ( { model | kk = updateTraitValue (parseIntWithDefault value) model.kk }, Cmd.none )


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
        [ renderFieldForTrait model "MU" (.mu >> getTraitValue) (Change Mu) (Roll Mu)
        , renderFieldForTrait model "KL" (getTraitValue << .kl) (Change Kl) (Roll Kl)
        , renderFieldForTrait model "IN" (.in_ >> getTraitValue) (Change In) NoOp
        , renderFieldForTrait model "CH" (.ch >> getTraitValue) (Change Ch) NoOp
        , renderFieldForTrait model "FF" (.ff >> getTraitValue) (Change Ff) NoOp
        , renderFieldForTrait model "GE" (.ge >> getTraitValue) (Change Ge) NoOp
        , renderFieldForTrait model "KO" (.ko >> getTraitValue) (Change Ko) NoOp
        , renderFieldForTrait model "KK" (.kk >> getTraitValue) (Change Kk) NoOp
        ]

renderFieldForTrait model traitLabel traitAccessor changeEvent rollEvent =
    div []
        [ text traitLabel
        , input [ placeholder traitLabel, type_ "number", onInput changeEvent, value (toString (traitAccessor model)) ] []
        , button [ onClick rollEvent ] [ text ("Roll " ++ traitLabel) ]
        ]

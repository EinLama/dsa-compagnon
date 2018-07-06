module Main exposing (..)

import Html exposing (Html, button, div, h1, h2, input, label, li, ol, text)
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



-- Types


type Trait
    = Kl
    | Ko
    | Ff
    | Kk
    | Mu
    | Ge
    | In
    | Ch



-- TODO: nachlesen über "union type"


type alias CharacterTrait =
    { trait : Trait
    , value : Int
    }


type alias TraitRoll =
    ( Trait, Int )


type alias Model =
    { traits : List CharacterTrait
    , rolls : List TraitRoll
    }


model : Model
model =
    { traits =
        [ CharacterTrait Mu 13
        , CharacterTrait Kl 14
        , CharacterTrait In 14
        , CharacterTrait Ch 15
        , CharacterTrait Ff 13
        , CharacterTrait Ge 12
        , CharacterTrait Ko 11
        , CharacterTrait Kk 9
        ]
    , rolls = []
    }


type Msg
    = NoOp
    | Roll Trait
    | Rolled Trait Int
    | Change Trait String
    | ResetRolls


traitLabel : Trait -> String
traitLabel trait =
    case trait of
        Kl ->
            "KL"

        Ko ->
            "KO"

        Ff ->
            "FF"

        Kk ->
            "KK"

        Mu ->
            "MU"

        Ge ->
            "GE"

        In ->
            "IN"

        Ch ->
            "CH"


updateTraitValue : Model -> Trait -> Int -> Model
updateTraitValue model trait value =
    { model
        | traits =
            List.map
                (\t ->
                    if t.trait == trait then
                        CharacterTrait trait value
                    else
                        t
                )
                model.traits
    }


getTrait : Trait -> Model -> Maybe CharacterTrait
getTrait trait model =
    model.traits |> List.filter (\t -> t.trait == trait) |> List.head


getTraitValue : Trait -> Model -> Int
getTraitValue trait model =
    model |> (getTrait trait) |> Maybe.map .value |> Maybe.withDefault -1


addTraitRoll : Int -> Trait -> Model -> Model
addTraitRoll value trait model =
    { model | rolls = ( trait, value ) :: model.rolls }


analyseRolls : Model -> Int
analyseRolls model =
    model.rolls
        |> List.foldl (\( rollTrait, rollValue ) total -> total + compareRollForTrait (model |> getTraitValue rollTrait) rollValue) 0


compareRollForTrait : Int -> Int -> Int
compareRollForTrait traitValue rollValue =
    if rollValue <= traitValue then
        0
    else
        rollValue - traitValue


extractTraitRolls : Model -> List String
extractTraitRolls model =
    model.rolls |> List.map (\( trait, value ) -> (traitLabel trait) ++ ": " ++ (toString value))


parseIntWithDefault : String -> Int
parseIntWithDefault str =
    String.toInt str |> Result.withDefault 0



-- Update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Roll trait ->
            ( model, Random.generate (Rolled trait) (Random.int 1 20) )

        Rolled trait result ->
            ( addTraitRoll result trait model, Cmd.none )

        Change trait value ->
            ( updateTraitValue model trait (parseIntWithDefault value), Cmd.none )

        ResetRolls ->
            ( { model | rolls = [] }, Cmd.none )



-- View Functions


view : Model -> Html Msg
view model =
    let
        rollsSum =
            model |> analyseRolls
    in
        div [ class "section" ]
            [ div [ class "section" ] [ h1 [ class "title" ] [ text "DSA Compagnon" ] ]
            , div [ class "section" ]
                [ div [ class "container" ]
                    [ renderAttributes model
                    , renderResultAndReset rollsSum
                    ]
                , renderRolls model

                --, div [] [ text (toString model) ]
                ]
            ]


renderRolls : Model -> Html Msg
renderRolls model =
    let
        rolls =
            model |> extractTraitRolls |> List.reverse
    in
        div [ class "container" ]
            [ h2 [ class "title" ] [ text "Ergebnisse" ]
            , div [ class "container" ]
                [ ol [ class "rolls" ] (List.map (\s -> li [ class "rolls-roll" ] [ text s ]) rolls)
                ]
            ]


renderResultAndReset : Int -> Html Msg
renderResultAndReset rollsSum =
    div [ class "section result-and-reset" ]
        [ div [ class "field has-addons has-addons" ]
            [ div [ class "control" ]
                [ input [ classList [ ( "is-danger", rollsSum > 0 ), ( "input is-large", True ) ], value (rollsSum |> toString), readonly True ] []
                ]
            , div [ class "control" ]
                [ button [ class "button is-danger is-large", onClick ResetRolls ] [ text "Reset" ]
                ]
            ]
        ]


renderAttributes : Model -> Html Msg
renderAttributes model =
    div [ class "columns traits" ]
        ([ div [ class "column is-2" ]
            [ renderFieldForTrait model Mu (model |> getTraitValue Mu) Change
            , renderFieldForTrait model Kl (model |> getTraitValue Kl) Change
            , renderFieldForTrait model In (model |> getTraitValue In) Change
            , renderFieldForTrait model Ch (model |> getTraitValue Ch) Change
            ]
         , div [ class "column is-2" ]
            [ renderFieldForTrait model Ff (model |> getTraitValue Ff) Change
            , renderFieldForTrait model Ge (model |> getTraitValue Ge) Change
            , renderFieldForTrait model Ko (model |> getTraitValue Ko) Change
            , renderFieldForTrait model Kk (model |> getTraitValue Kk) Change
            ]
         ]
            ++ renderRollButtons model
        )


renderRollButton : Trait -> (Trait -> Msg) -> Html Msg
renderRollButton trait rollEvent =
    let
        labelForTrait =
            traitLabel trait
    in
        div
            [ class "control roll-button" ]
            [ button [ class "button is-medium is-info", onClick <| rollEvent trait ] [ text ("Roll " ++ labelForTrait) ]
            ]


renderRollButtons : Model -> List (Html Msg)
renderRollButtons model =
    [ div [ class "column is-1" ]
        (model.traits
            |> List.take 4
            |> List.map (\t -> renderRollButton t.trait Roll)
        )
    , div [ class "column is-1" ]
        (model.traits
            |> List.drop 4
            |> List.map (\t -> renderRollButton t.trait Roll)
        )
    ]


renderFieldForTrait : Model -> Trait -> Int -> (Trait -> String -> msg) -> Html msg
renderFieldForTrait model trait traitValue changeEvent =
    let
        labelForTrait =
            traitLabel trait
    in
        div [ class "trait" ]
            [ div [ class "field is-horizontal" ]
                [ div [ class "field-label" ]
                    [ label [ class "label is-medium" ] [ text labelForTrait ]
                    ]
                , div
                    [ class "field-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ input [ class "input is-medium", placeholder labelForTrait, type_ "number", onInput (changeEvent trait), value (toString traitValue) ] []
                            ]
                        ]
                    ]
                ]
            ]

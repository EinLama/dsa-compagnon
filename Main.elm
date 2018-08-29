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


type alias Talent =
    { title : String
    , traits : List Trait
    , value : Int -- skilled value for this talent
    }


type alias TraitRoll =
    ( Trait, Int )


type alias Model =
    { traits : List CharacterTrait
    , talents : List Talent
    , rolls : List TraitRoll
    , singleRoll : Int
    , rolledTalent : Maybe Talent
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
    , talents =
        [ Talent "Klettern" [ Mu, Mu, Ff ] 5
        , Talent "Götter und Kulte" [ Kl, Kl, In ] 3
        ]
    , rolls = []
    , singleRoll = 0
    , rolledTalent = Maybe.Nothing
    }


type Msg
    = NoOp
    | RollSingle Int
    | RolledSingle Int
    | Roll Trait
    | RollTalent Talent
    | Rolled Trait Int
    | RolledTalent Talent (List Int)
    | Change Trait String
    | ResetRolls


traitLabel : Trait -> String
traitLabel trait =
    -- this will no longer be valid in elm 0.19
    trait
        |> toString
        |> String.toUpper


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


addTraitRolls : List TraitRoll -> Model -> Model
addTraitRolls results model =
    { model | rolls = (results :: [ model.rolls ]) |> List.concat }


addTalentRoll : List TraitRoll -> Talent -> Model -> Model
addTalentRoll rollResults talent model =
    { model
        | rolls = (addTraitRolls rollResults model).rolls
        , rolledTalent = Maybe.Just talent
    }


analyseRolls : List TraitRoll -> Int
analyseRolls traitRolls =
    traitRolls
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

        RollSingle eyes ->
            ( model, Random.generate RolledSingle (Random.int 1 eyes) )

        RolledSingle result ->
            ( { model | singleRoll = result }, Cmd.none )

        Roll trait ->
            ( model, Random.generate (Rolled trait) twentySidedDieGen )

        Rolled trait result ->
            ( addTraitRoll result trait model, Cmd.none )

        RollTalent talent ->
            let
                threeDice =
                    Random.list 3 twentySidedDieGen
            in
                ( model, Random.generate (RolledTalent talent) threeDice )

        RolledTalent talent results ->
            let
                zipped =
                    List.map2 (,) talent.traits results
            in
                ( addTalentRoll zipped talent model, Cmd.none )

        Change trait value ->
            ( updateTraitValue model trait (parseIntWithDefault value), Cmd.none )

        ResetRolls ->
            ( { model | rolls = [], rolledTalent = Maybe.Nothing }, Cmd.none )


twentySidedDieGen : Random.Generator Int
twentySidedDieGen =
    -- possibly return a Random.pair here:
    Random.int 1 20



-- View Functions


view : Model -> Html Msg
view model =
    let
        rollsSum =
            model.rolls |> analyseRolls
    in
        div [ class "section" ]
            [ div [ class "section" ] [ h1 [ class "title" ] [ text "DSA Compagnon" ] ]
            , div [ class "section" ]
                [ div [ class "container" ]
                    [ renderAttributes model
                    ]
                , div [ class "columns" ]
                    [ renderResultAndReset rollsSum (model.rolledTalent |> Maybe.map .value |> Maybe.withDefault 0)
                    , renderSingleRolls model
                    ]
                , renderRolls model

                --, div [] [ text (toString model) ]
                ]
            ]


renderSingleRolls : Model -> Html Msg
renderSingleRolls model =
    div [ class "column is-3 is-offset-2 single-rolls" ]
        [ text "Einen Einzelwurf durchführen:"
        , div [ class "field has-addons" ]
            [ div [ class "control" ]
                [ input [ class "input is-large", value (model.singleRoll |> toString), readonly True ] []
                ]
            , div [ class "control" ]
                [ button [ class "button is-large roll-6", onClick (RollSingle 6) ] [ text "Roll 6" ]
                ]
            , div [ class "control" ]
                [ button [ class "button is-large roll-20", onClick (RollSingle 20) ] [ text "Roll 20" ]
                ]
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


renderResultAndReset : Int -> Int -> Html Msg
renderResultAndReset rollsSum talentValue =
    let
        talentPointsLeft =
            talentValue - rollsSum
    in
        div [ class "column is-2 is-offset-3 result-and-reset" ]
            [ text "Wurf Attribut, Talent:"
            , div [ class "field has-addons" ]
                [ div [ class "control" ]
                    [ input [ classList [ ( "is-danger", rollsSum > 0 ), ( "input is-large", True ) ], value (rollsSum |> toString), readonly True ] []
                    ]
                , div [ class "control" ]
                    [ input [ classList [ ( "is-danger", talentPointsLeft < 0 ), ( "is-success", talentPointsLeft > 0 ), ( "input is-large", True ) ], value (talentPointsLeft |> toString), readonly True ] []
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
            -- append roll buttons to the side
            ++ renderRollButtons model
            ++ [ renderTalents model ]
        )


renderTalents : Model -> Html Msg
renderTalents model =
    div [ class "column is-2 talents" ]
        (List.map renderTalentButton model.talents)


renderTalentButton : Talent -> Html Msg
renderTalentButton talent =
    let
        buttonLabel =
            String.join " " [ talent.title, traitsForTalents talent.traits, toString talent.value ]
    in
        div
            [ class "control roll-button" ]
            [ button [ class "button is-medium is-info", onClick <| RollTalent talent ] [ text buttonLabel ]
            ]


traitsForTalents : List Trait -> String
traitsForTalents traits =
    let
        traitsStr =
            String.join "/" (List.map traitLabel traits)
    in
        String.join "" [ "(", traitsStr, ")" ]


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

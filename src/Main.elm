port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ingredients exposing (Ingredient, catalog)
import Json.Decode as Decode
import Json.Encode as Encode


-- ---------------------------------------------------------------------------
--  PORTS
-- ---------------------------------------------------------------------------


port saveState : String -> Cmd msg


-- ---------------------------------------------------------------------------
--  RECIPE ENCODING / DECODING  (for URL hash)
-- ---------------------------------------------------------------------------


type alias Recipe =
    { targetAmount : String
    , selections : List ( String, String, Bool )
    }


encodeRecipe : Model -> String
encodeRecipe model =
    Encode.object
        [ ( "t", Encode.string model.targetAmount )
        , ( "s", Encode.list encodeSelection model.selected )
        ]
        |> Encode.encode 0


encodeSelection : SelectedIngredient -> Encode.Value
encodeSelection sel =
    Encode.object
        [ ( "n", Encode.string sel.ingredient.name )
        , ( "a", Encode.string sel.amount )
        , ( "l", Encode.bool sel.locked )
        ]


decodeRecipe : Decode.Decoder Recipe
decodeRecipe =
    Decode.map2 Recipe
        (Decode.field "t" Decode.string)
        (Decode.field "s" (Decode.list decodeSelectionEntry))


decodeSelectionEntry : Decode.Decoder ( String, String, Bool )
decodeSelectionEntry =
    Decode.map3 (\n a l -> ( n, a, l ))
        (Decode.field "n" Decode.string)
        (Decode.field "a" Decode.string)
        (Decode.field "l" Decode.bool)


findIngredient : String -> List Ingredient -> Maybe Ingredient
findIngredient name catalog_ =
    List.filter (\i -> i.name == name) catalog_ |> List.head


applyRecipe : Recipe -> Model -> Model
applyRecipe recipe base =
    let
        selections =
            List.indexedMap
                (\idx ( name, amount, locked ) ->
                    case findIngredient name base.catalog of
                        Just ing ->
                            Just { id = idx, ingredient = ing, amount = amount, locked = locked }

                        Nothing ->
                            Nothing
                )
                recipe.selections
    in
    { base
        | targetAmount = recipe.targetAmount
        , selected = List.filterMap identity selections
        , nextId = List.length recipe.selections
    }


-- ---------------------------------------------------------------------------
--  TARGET RANGES  (percentages)
-- ---------------------------------------------------------------------------

waterLow : Float
waterLow =
    55.0


waterHigh : Float
waterHigh =
    64.0


sugarLow : Float
sugarLow =
    15.0


sugarHigh : Float
sugarHigh =
    20.0


fatLow : Float
fatLow =
    10.0


fatHigh : Float
fatHigh =
    16.0



-- ---------------------------------------------------------------------------
--  MODEL
-- ---------------------------------------------------------------------------


type alias SelectedIngredient =
    { id : Int
    , ingredient : Ingredient
    , amount : String
    , locked : Bool
    }


type alias Model =
    { catalog : List Ingredient
    , selected : List SelectedIngredient
    , targetAmount : String
    , nextId : Int
    , showInfo : Bool
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        defaultModel =
            { catalog = catalog
            , selected = []
            , targetAmount = "1000"
            , nextId = 0
            , showInfo = False
            }
    in
    case Decode.decodeValue decodeRecipe flags of
        Ok recipe ->
            ( applyRecipe recipe defaultModel, Cmd.none )

        Err _ ->
            ( defaultModel, Cmd.none )



-- ---------------------------------------------------------------------------
--  MESSAGES
-- ---------------------------------------------------------------------------


type Msg
    = SetTarget String
    | AddIngredient Ingredient
    | RemoveIngredient Int
    | SetAmount Int String
    | ToggleLock Int
    | ToggleInfo



-- ---------------------------------------------------------------------------
--  SOLVER
-- ---------------------------------------------------------------------------


type alias IngredientResult =
    { id : Int
    , name : String
    , amount : Float
    , locked : Bool
    }


type alias SolverOutput =
    { results : List IngredientResult
    , waterPct : Float
    , sugarPct : Float
    , fatPct : Float
    , waterOk : Bool
    , sugarOk : Bool
    , fatOk : Bool
    , feasible : Bool
    }


{-| Solve for unlocked ingredient amounts given a target total and a list of
selected ingredients (some locked, some free).

Returns the computed amounts together with the resulting nutrient percentages
and flags indicating whether each is inside its target range.

-}
solve : Float -> List SelectedIngredient -> SolverOutput
solve targetAmount selected =
    if targetAmount <= 0 || List.isEmpty selected then
        emptyOutput selected

    else
        let
            -- partition locked / unlocked
            lockedEntries : List ( Float, Ingredient )
            lockedEntries =
                List.filterMap
                    (\s ->
                        if s.locked then
                            String.toFloat s.amount |> Maybe.map (\a -> ( a, s.ingredient ))
                        else
                            Nothing
                    )
                    selected

            unlockedIngredients : List Ingredient
            unlockedIngredients =
                List.filterMap
                    (\s ->
                        if s.locked then
                            Nothing
                        else
                            Just s.ingredient
                    )
                    selected

            -- fixed contributions
            fixedAmount : Float
            fixedAmount =
                List.sum (List.map Tuple.first lockedEntries)

            fixedWater : Float
            fixedWater =
                List.sum (List.map (\( a, i ) -> a * i.water / 100) lockedEntries)

            fixedSugar : Float
            fixedSugar =
                List.sum (List.map (\( a, i ) -> a * i.sugar / 100) lockedEntries)

            fixedFat : Float
            fixedFat =
                List.sum (List.map (\( a, i ) -> a * i.fat / 100) lockedEntries)

            remaining : Float
            remaining =
                targetAmount - fixedAmount

            k : Int
            k =
                List.length unlockedIngredients

            -- evaluate: given a list of free-ingredient amounts (in same order
            -- as unlockedIngredients), compute full SolverOutput.
            evaluate : List Float -> SolverOutput
            evaluate freeAmounts =
                let
                    paired : List ( Float, Ingredient )
                    paired =
                        List.map2 (\a i -> ( a, i )) freeAmounts unlockedIngredients

                    freeWater : Float
                    freeWater =
                        List.sum (List.map (\( a, i ) -> a * i.water / 100) paired)

                    freeSugar : Float
                    freeSugar =
                        List.sum (List.map (\( a, i ) -> a * i.sugar / 100) paired)

                    freeFat : Float
                    freeFat =
                        List.sum (List.map (\( a, i ) -> a * i.fat / 100) paired)

                    totalWater : Float
                    totalWater =
                        (fixedWater + freeWater) / targetAmount * 100

                    totalSugar : Float
                    totalSugar =
                        (fixedSugar + freeSugar) / targetAmount * 100

                    totalFat : Float
                    totalFat =
                        (fixedFat + freeFat) / targetAmount * 100

                    results : List IngredientResult
                    results =
                        buildResults selected lockedEntries unlockedIngredients freeAmounts
                in
                { results = results
                , waterPct = totalWater
                , sugarPct = totalSugar
                , fatPct = totalFat
                , waterOk = totalWater >= waterLow - 0.001 && totalWater <= waterHigh + 0.001
                , sugarOk = totalSugar >= sugarLow - 0.001 && totalSugar <= sugarHigh + 0.001
                , fatOk = totalFat >= fatLow - 0.001 && totalFat <= fatHigh + 0.001
                , feasible = False
                }
                    |> (\out ->
                            { out
                                | feasible = out.waterOk && out.sugarOk && out.fatOk
                            }
                       )
        in
        if k == 0 then
            -- everything is locked — just evaluate
            evaluate []

        else if remaining <= 0 then
            -- locked ingredients already consume (or exceed) the target;
            -- set free ingredients to zero and report
            evaluate (List.repeat k 0)

        else if k == 1 then
            -- exactly one free ingredient — the whole remainder goes to it
            evaluate [ remaining ]

        else
            -- two or more free ingredients — iterative proportional balancing
            let
                maxIter : Int
                maxIter =
                    400

                -- learning rate (decays each iteration)
                startLr : Float
                startLr =
                    0.08

                decay : Float
                decay =
                    0.992

                -- midpoints we aim for
                waterMid : Float
                waterMid =
                    (waterLow + waterHigh) / 2

                sugarMid : Float
                sugarMid =
                    (sugarLow + sugarHigh) / 2

                fatMid : Float
                fatMid =
                    (fatLow + fatHigh) / 2

                -- initial equal split
                initial : List Float
                initial =
                    List.repeat k (remaining / toFloat k)

                -- single iteration: adjust proportions, clip, renormalise
                step : Float -> List Float -> List Float
                step lr amounts =
                    let
                        -- current weighted nutrient contributions from free
                        -- ingredients (0-100 scale)
                        curWater : Float
                        curWater =
                            (fixedWater
                                + List.sum (List.map2 (\a i -> a * i.water / 100) amounts unlockedIngredients)
                            )
                                / targetAmount
                                * 100

                        curSugar : Float
                        curSugar =
                            (fixedSugar
                                + List.sum (List.map2 (\a i -> a * i.sugar / 100) amounts unlockedIngredients)
                            )
                                / targetAmount
                                * 100

                        curFat : Float
                        curFat =
                            (fixedFat
                                + List.sum (List.map2 (\a i -> a * i.fat / 100) amounts unlockedIngredients)
                            )
                                / targetAmount
                                * 100

                        errWater : Float
                        errWater =
                            waterMid - curWater

                        errSugar : Float
                        errSugar =
                            sugarMid - curSugar

                        errFat : Float
                        errFat =
                            fatMid - curFat

                        -- mean nutrient values among free ingredients
                        avgWater : Float
                        avgWater =
                            List.sum (List.map .water unlockedIngredients) / toFloat k

                        avgSugar : Float
                        avgSugar =
                            List.sum (List.map .sugar unlockedIngredients) / toFloat k

                        avgFat : Float
                        avgFat =
                            List.sum (List.map .fat unlockedIngredients) / toFloat k

                        -- adjust each free amount
                        adjusted : List Float
                        adjusted =
                            List.map2
                                (\a ing ->
                                    let
                                        dw : Float
                                        dw =
                                            ing.water - avgWater

                                        ds : Float
                                        ds =
                                            ing.sugar - avgSugar

                                        df : Float
                                        df =
                                            ing.fat - avgFat

                                        delta : Float
                                        delta =
                                            lr
                                                * (errWater * dw + errSugar * ds + errFat * df)
                                                / 10000
                                                * remaining
                                    in
                                    a + delta
                                )
                                amounts
                                unlockedIngredients

                        -- clip negative → 0
                        clipNeg : Float -> Float
                        clipNeg x =
                            if x < 0 then 0 else x

                        clipped : List Float
                        clipped =
                            List.map clipNeg adjusted

                        -- renormalise so sum = remaining
                        s : Float
                        s =
                            List.sum clipped
                    in
                    if s > 0 then
                        List.map (\x -> x * remaining / s) clipped

                    else
                        initial
            in
            -- iterate
            let
                go : Float -> List Float -> Int -> List Float
                go lr amounts iter =
                    if iter >= maxIter then
                        amounts

                    else
                        let
                            next =
                                step lr amounts
                        in
                        if amountsClose next amounts then
                            next

                        else
                            go (lr * decay) next (iter + 1)
            in
            evaluate (go startLr initial 0)


{-| Check whether two amount lists are close enough to stop iterating.
-}
amountsClose : List Float -> List Float -> Bool
amountsClose a b =
    let
        diffs =
            List.map2 (\x y -> abs (x - y)) a b
    in
    List.sum diffs < 0.01


{-| Build an ordered result list that preserves the original selection order,
using parsed locked amounts and computed free amounts.
-}
buildResults :
    List SelectedIngredient
    -> List ( Float, Ingredient )
    -> List Ingredient
    -> List Float
    -> List IngredientResult
buildResults selected_ lockedEntries unlockedIngredients freeAmounts =
    let
        go :
            List SelectedIngredient
            -> List ( Float, Ingredient )
            -> List Float
            -> List IngredientResult
            -> List IngredientResult
        go sels locks frees acc =
            case sels of
                [] ->
                    List.reverse acc

                s :: rest ->
                    if s.locked then
                        case locks of
                            ( amt, _ ) :: restLocks ->
                                go rest restLocks frees
                                    ({ id = s.id, name = s.ingredient.name, amount = amt, locked = True }
                                        :: acc
                                    )

                            [] ->
                                go rest [] frees
                                    ({ id = s.id, name = s.ingredient.name, amount = 0, locked = True }
                                        :: acc
                                    )

                    else
                        case frees of
                            amt :: restFrees ->
                                go rest locks restFrees
                                    ({ id = s.id, name = s.ingredient.name, amount = amt, locked = False }
                                        :: acc
                                    )

                            [] ->
                                go rest locks []
                                    ({ id = s.id, name = s.ingredient.name, amount = 0, locked = False }
                                        :: acc
                                    )
    in
    go selected_ lockedEntries freeAmounts []


{-| Output used when there is nothing meaningful to solve.
-}
emptyOutput : List SelectedIngredient -> SolverOutput
emptyOutput selected_ =
    { results =
        List.map
            (\s ->
                { id = s.id
                , name = s.ingredient.name
                , amount = 0
                , locked = s.locked
                }
            )
            selected_
    , waterPct = 0
    , sugarPct = 0
    , fatPct = 0
    , waterOk = False
    , sugarOk = False
    , fatOk = False
    , feasible = False
    }



-- ---------------------------------------------------------------------------
--  UPDATE
-- ---------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                SetTarget val ->
                    { model | targetAmount = val }

                AddIngredient ing ->
                    let
                        entry =
                            { id = model.nextId
                            , ingredient = ing
                            , amount = "0"
                            , locked = False
                            }
                    in
                    { model
                        | selected = model.selected ++ [ entry ]
                        , nextId = model.nextId + 1
                    }

                RemoveIngredient id_ ->
                    { model
                        | selected = List.filter (\s -> s.id /= id_) model.selected
                    }

                SetAmount id_ val ->
                    { model
                        | selected =
                            List.map
                                (\s ->
                                    if s.id == id_ then
                                        { s | amount = val }

                                    else
                                        s
                                )
                                model.selected
                    }

                ToggleLock id_ ->
                    { model
                        | selected =
                            List.map
                                (\s ->
                                    if s.id == id_ then
                                        { s | locked = not s.locked }

                                    else
                                        s
                                )
                                model.selected
                    }

                ToggleInfo ->
                    { model | showInfo = not model.showInfo }
    in
    ( newModel, saveState (encodeRecipe newModel) )



-- ---------------------------------------------------------------------------
--  VIEW
-- ---------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        parsedTarget : Float
        parsedTarget =
            String.toFloat model.targetAmount |> Maybe.withDefault 0

        solverOutput : SolverOutput
        solverOutput =
            solve parsedTarget model.selected
    in
    div [ class "app" ]
        [ header []
            [ h1 [] [ text "🍦 Ice Cream Calculator" ]
            , div [ class "subtitle-row" ]
                [ p [ class "subtitle" ]
                    [ text "Select ingredients, lock some amounts, and let the app balance the rest." ]
                , button
                    [ class "btn btn-info"
                    , classList [ ( "active", model.showInfo ) ]
                    , onClick ToggleInfo
                    , attribute "aria-label" "How it works"
                    ]
                    [ text (if model.showInfo then "✕" else "ℹ") ]
                ]
            , if model.showInfo then
                div [ class "info-panel" ]
                    [ p []
                        [ text "🍨  "
                        , strong [] [ text "How it works" ]
                        ]
                    , p []
                        [ text "Ice cream is all about balance! The sweet spot for creamy, scoopable results is "
                        , strong [] [ text "55–64% water" ]
                        , text ", "
                        , strong [] [ text "15–20% sugar" ]
                        , text ", and "
                        , strong [] [ text "10–16% fat" ]
                        , text "."
                        ]
                    , p []
                        [ text "Add ingredients from the catalog on the left, then either lock a precise amount or leave it set to auto. When you lock some ingredients, the app balances the rest to hit all three target ranges simultaneously — so you get perfect proportions without doing any math."
                        ]
                    , p []
                        [ text "Start by setting your target total weight, then pick your ingredients. Lock the ones you want to fix (e.g. the cream you have left in the fridge), and watch the sliders turn green! ✓"
                        ]
                    ]
              else
                text ""
            ]
        , main_ []
            [ -- target amount
              section [ class "card target-section" ]
                [ div [ class "target-row" ]
                    [ label [ for "target-amount" ] [ text "Target amount" ]
                    , input
                        [ id "target-amount"
                        , type_ "number"
                        , class "input target-input"
                        , value model.targetAmount
                        , onInput SetTarget
                        , Html.Attributes.min "1"
                        , step "1"
                        ]
                        []
                    , span [ class "unit" ] [ text "g" ]
                    ]
                ]

            -- two-column layout: catalog | mix
            , div [ class "columns" ]
                [ -- left: ingredient catalog
                  section [ class "card catalog-panel" ]
                    [ h2 [] [ text "Available ingredients" ]
                    , div [ class "catalog-list" ]
                        (List.map viewCatalogItem model.catalog)
                    ]

                -- right: your mix + results
                , div [ class "right-panel" ]
                    [ -- selected ingredients
                      section [ class "card mix-panel" ]
                        [ h2 [] [ text "Your mix" ]
                        , if List.isEmpty model.selected then
                            p [ class "hint" ]
                                [ text "Add ingredients from the catalog on the left." ]

                          else
                            div [ class "selected-list" ]
                                (List.map (viewSelectedItem solverOutput) model.selected)
                        ]

                    -- results dashboard (only when there is a target and ingredients)
                    , if parsedTarget > 0 && not (List.isEmpty model.selected) then
                        viewDashboard solverOutput

                      else
                        text ""
                    ]
                ]
            ]
        , footer []
            [ text "Target ranges:  Water "
            , strong [] [ text "55–64%" ]
            , text "  ·  Sugar "
            , strong [] [ text "15–20%" ]
            , text "  ·  Fat "
            , strong [] [ text "10–16%" ]
            ]
        ]


-- -----  catalog item  -------------------------------------------------------


viewCatalogItem : Ingredient -> Html Msg
viewCatalogItem ing =
    div [ class "catalog-item" ]
        [ div [ class "catalog-item-info" ]
            [ span [ class "catalog-item-name" ] [ text ing.name ]
            , span [ class "catalog-item-stats" ]
                [ text
                    ("W " ++ formatPct ing.water ++ "  ·  S " ++ formatPct ing.sugar ++ "  ·  F " ++ formatPct ing.fat)
                ]
            ]
        , button
            [ class "btn btn-add"
            , onClick (AddIngredient ing)
            ]
            [ text "+ Add" ]
        ]


-- -----  selected / mix item  ------------------------------------------------


viewSelectedItem : SolverOutput -> SelectedIngredient -> Html Msg
viewSelectedItem out sel =
    let
        result : Maybe IngredientResult
        result =
            List.filter (\r -> r.id == sel.id) out.results |> List.head

        displayAmount : String
        displayAmount =
            case result of
                Just r ->
                    String.fromFloat (roundTo 1 r.amount)

                Nothing ->
                    "0"

        isOverallocated : Bool
        isOverallocated =
            not out.feasible && out.waterPct == 0 && out.sugarPct == 0 && out.fatPct == 0
    in
    div [ class "selected-item", classList [ ( "locked", sel.locked ) ] ]
        [ div [ class "selected-item-header" ]
            [ span [ class "selected-item-name" ] [ text sel.ingredient.name ]
            , button
                [ class "btn btn-remove"
                , onClick (RemoveIngredient sel.id)
                ]
                [ text "✕" ]
            ]
        , div [ class "selected-item-body" ]
            [ if sel.locked then
                input
                    [ type_ "number"
                    , class "input amount-input"
                    , value sel.amount
                    , onInput (SetAmount sel.id)
                    , Html.Attributes.min "0"
                    , step "1"
                    , placeholder "grams"
                    ]
                    []

              else
                span [ class "computed-amount" ] [ text (displayAmount ++ " g") ]
            , span [ class "unit" ] [ text "grams" ]
            , label [ class "lock-toggle" ]
                [ input
                    [ type_ "checkbox"
                    , checked sel.locked
                    , onClick (ToggleLock sel.id)
                    ]
                    []
                , span [ class "lock-label" ]
                    [ text
                        (if sel.locked then
                            "🔒 locked"

                         else
                            "🔓 auto"
                        )
                    ]
                ]
            ]
        ]


-- -----  results dashboard  ---------------------------------------------------


viewDashboard : SolverOutput -> Html Msg
viewDashboard out =
    section [ class "card dashboard" ]
        [ h2 [] [ text "📊  Results" ]
        , div [ class "gauges" ]
            [ viewGauge "Water"
                waterLow
                waterHigh
                out.waterPct
                out.waterOk
                "%"
            , viewGauge "Sugar"
                sugarLow
                sugarHigh
                out.sugarPct
                out.sugarOk
                "%"
            , viewGauge "Fat"
                fatLow
                fatHigh
                out.fatPct
                out.fatOk
                "%"
            ]
        , if not out.feasible && out.waterPct > 0 then
            div [ class "infeasible-warning" ]
                [ text "⚠  No combination satisfies all targets. "
                , text "Try unlocking more ingredients or adjusting fixed amounts."
                , br [] []
                , text "Violated: "
                , if not out.waterOk then
                    span [ class "tag bad" ] [ text "water" ]

                  else
                    text ""
                , if not out.sugarOk then
                    span [ class "tag bad" ] [ text "sugar" ]

                  else
                    text ""
                , if not out.fatOk then
                    span [ class "tag bad" ] [ text "fat" ]

                  else
                    text ""
                ]

          else
            text ""
        , if out.feasible then
            div [ class "feasible-badge" ] [ text "✓ All proportions in range" ]

          else
            text ""
        ]


viewGauge : String -> Float -> Float -> Float -> Bool -> String -> Html Msg
viewGauge label_ low_ high_ value_ ok_ unit_ =
    let
        clamped : Float
        clamped =
            clamp 0 100 value_

        -- position of the value as percentage within the gauge width
        pos : Float
        pos =
            clamped

        -- range highlight positions
        rangeLeft : Float
        rangeLeft =
            low_

        rangeRight : Float
        rangeRight =
            high_

        statusClass : String
        statusClass =
            if ok_ then
                "ok"

            else
                "bad"
    in
    div [ class "gauge" ]
        [ div [ class "gauge-header" ]
            [ span [ class "gauge-label" ] [ text label_ ]
            , span [ class ("gauge-value " ++ statusClass) ]
                [ text (formatDecimal value_ ++ unit_) ]
            , span [ class ("gauge-icon " ++ statusClass) ]
                [ text
                    (if ok_ then
                        "✓"

                     else
                        "✗"
                    )
                ]
            ]
        , div [ class "gauge-track" ]
            [ -- range band
              div
                [ class "gauge-range"
                , style "left" (String.fromFloat rangeLeft ++ "%")
                , style "width" (String.fromFloat (rangeRight - rangeLeft) ++ "%")
                ]
                []
            , -- current value marker
              div
                [ class "gauge-marker"
                , style "left" (String.fromFloat pos ++ "%")
                ]
                []
            , -- low / high labels
              span [ class "gauge-bound" ] [ text (formatDecimal low_ ++ "%") ]
            , span [ class "gauge-bound right" ] [ text (formatDecimal high_ ++ "%") ]
            ]
        ]



-- ---------------------------------------------------------------------------
--  HELPERS
-- ---------------------------------------------------------------------------


formatPct : Float -> String
formatPct v =
    formatDecimal v ++ "%"


formatDecimal : Float -> String
formatDecimal v =
    let
        rounded =
            roundTo 1 v

        intPart =
            round rounded

        str =
            if rounded == toFloat intPart then
                String.fromInt intPart

            else
                String.fromFloat rounded
    in
    str


roundTo : Int -> Float -> Float
roundTo places value =
    let
        factor =
            10 ^ places |> toFloat
    in
    toFloat (round (value * factor)) / factor



-- ---------------------------------------------------------------------------
--  MAIN
-- ---------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

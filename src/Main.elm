module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, input, button, div, text, table, td, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Random
import Tuple exposing (first)

-- MAIN

main =
     Browser.element { init = init
                     , update = update
                     , subscriptions = subscriptions
                     , view = view
                     }

-- MODEL

type alias Model =
    { table : Table
    , settings : Settings
    , currentSelection : Index
    , numberOfSteps : Int
    }

type alias Settings =
    --These are the user input for generating a custom size table, thus they can be different from the actual size of the current table.
    { setWidth : Int
    , setHeight : Int
    , seed : Int
    , seedInput : String
    }
    
init : () -> (Model, Cmd Msg)
init _ =
    (Model emptyTable (Settings 10 10 0 "") (Index 1 1 ) 0
    ,Random.generate GenerateSeedAndTable randInt
    )
   

-- UPDATE

type Msg
    = GenerateRandomSeed
    | GenerateSeedAndTable Int
    | GenerateTableFromSeed
    | UpdateSettings SettingsChanges
    | NewPosition Index
    | ChangeColor Color

type SettingsChanges =
      IncrementWidth
    | DecrementWidth
    | IncrementHeight
    | DecrementHeight
    | UpdateSeed String

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
    case msg of
        GenerateRandomSeed ->
            (model , Random.generate GenerateSeedAndTable randInt)

        GenerateTableFromSeed ->
            case String.toInt model.settings.seedInput of
                Nothing -> (model, Cmd.none)
                Just seedNum ->
                    let settings = model.settings
                        newSettings = {settings | seed = seedNum}
                    in ( updateTable {model | settings = newSettings}, Cmd.none)
            

        GenerateSeedAndTable newInt ->
            let settings = model.settings
                newSettings = {settings | seed = newInt }
            in ( updateTable {model | settings = newSettings}
               , Cmd.none)
                

        UpdateSettings changes ->            
            updateSettings changes model
                
        NewPosition newIndex ->
             ({model | currentSelection = newIndex}, Cmd.none)

        ChangeColor color ->
            let newTable = recolorRegion color  model.table model.currentSelection
                currentNumberOfSteps = model.numberOfSteps
            in case newTable of
                   Nothing -> (model , Cmd.none)
                   Just something -> 
                       ( {model | table = something
                                , numberOfSteps = currentNumberOfSteps + 1
                         }
                       , Cmd.none)

--This function updates the table to match the current settings
updateTable : Model -> Model
updateTable model  =
    let settings = model.settings
        newTable =
            first ( Random.step (genTable settings.setWidth settings.setHeight)
                        (Random.initialSeed settings.seed)
                  ) --This maybe.withdefault should never get a Nothing value, because we patternmatch on the settings.seed value before calling updateTable.
    in { model | currentSelection = (Index 1 1)
               , table = newTable
               , numberOfSteps = 0
       }


updateSettings : SettingsChanges -> Model -> (Model,Cmd Msg)
updateSettings changes model =
    let width = model.settings.setWidth
        height = model.settings.setHeight
        settings = model.settings 
    in case changes of

           IncrementWidth ->
               let newSettings = {settings | setWidth = width + 1}
               in ({model | settings = newSettings}, Cmd.none)

           DecrementWidth ->
               let newSettings = {settings | setWidth = width - 1}
               in ({model | settings = newSettings}, Cmd.none)

           IncrementHeight ->
               let newSettings = {settings | setHeight = height + 1}
               in ({model | settings = newSettings}, Cmd.none)

           DecrementHeight ->
               let newSettings = {settings | setHeight = height - 1}
               in ({model | settings = newSettings}, Cmd.none)

           UpdateSeed inpText ->
               let newSettings = {settings | seedInput = inpText}
               in ({model | settings = newSettings}, Cmd.none)

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none  

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ viewCurrentPosition model.currentSelection
        , viewChangeColorBar
        , viewTable model.table
        , text ("Current number of steps: " ++ (String.fromInt model.numberOfSteps))
        , viewSettings model.settings
        ]

viewCell : Cell -> Html Msg
viewCell cell =
    td [] [ button [ style "background-color" (colorToString cell.color)
                   , style "height" "30px"
                   , style "width" "30px"
                   , onClick (NewPosition cell.index)
                   ]
                   []
          ]

colorToString : Color -> String
colorToString color =
    case color of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Yellow -> "yellow"
        Purple -> "purple"

viewRow : List Cell -> Html Msg
viewRow cells =
    tr [] (List.map viewCell cells)

viewTable : Table -> Html Msg
viewTable rows =
    table [] (List.map viewRow rows)


viewSettings : Settings -> Html Msg
viewSettings settings =
    div []
        [ viewWidthCounter settings.setWidth
        , viewHeightCounter settings.setHeight
        , div []
            [ button [ onClick GenerateRandomSeed ] [text "Generate random puzzle"]
            , text ("Current seed is: " ++ (String.fromInt settings.seed))
            ]
        , input [onInput (UpdateSettings << UpdateSeed) ] []
        , button [ onClick
                      (GenerateTableFromSeed)
                 ]
                 [ text "Generate puzzle from seed" ]
        ]

viewWidthCounter : Int -> Html Msg
viewWidthCounter val =
    div []
        [ button [onClick (UpdateSettings DecrementWidth) ] [text "-"]
        , text (String.fromInt val)
        , button [onClick (UpdateSettings IncrementWidth) ] [text "+"]
        ]

viewHeightCounter : Int -> Html Msg
viewHeightCounter val =
    div []
        [ button [onClick (UpdateSettings DecrementHeight) ] [text "-"]
        , text (String.fromInt val)
        , button [onClick (UpdateSettings IncrementHeight) ] [text "+"]
        ]

viewCurrentPosition : Index -> Html Msg
viewCurrentPosition ind =
    div []
        [ text ("The currently selected field is: "
               ++ (String.fromInt ind.row)
               ++ ", "
               ++ (String.fromInt ind.col))
        ]

viewChangeColorButton : Color -> Html Msg
viewChangeColorButton color =
    button [ onClick (ChangeColor color)]
           [ text (colorToString color)]

viewChangeColorBar : Html Msg
viewChangeColorBar =
    div []
        [ viewChangeColorButton Red
        , viewChangeColorButton Blue
        , viewChangeColorButton Green
        , viewChangeColorButton Yellow
        , viewChangeColorButton Purple
        ]

--Table data structure and helper funcitons

type alias Table = List (List Cell)
    

type alias Index =
    { row : Int
    , col : Int
    }

emptyTable : Table
emptyTable =
    [[]]


type Color =
    Red
  | Blue
  | Green
  | Yellow
  | Purple

type alias Cell =
    { color : Color
    , index : Index
    }

--Random generation
randInt : Random.Generator Int
randInt = Random.int 0 Random.maxInt

genColor : Random.Generator Color
genColor =
    Random.uniform Red [ Blue, Green, Yellow, Purple]

genCell : Int -> Int -> Random.Generator Cell
genCell rowNum colNum =
    let mkCell colorArg =
               { color = colorArg
               , index = { row = rowNum, col = colNum}
               }
    in Random.map mkCell genColor


genRow : Int -> Int -> Random.Generator (List Cell)
genRow numOfCols rowNum =
    --sequence is there to swap structures ie: from List (Random.Generator Cell) to Random.Generator (List Cell)
    let sequence lst =
           case lst of
               x :: xs ->
                   x
                   |> Random.andThen (\cell -> Random.map (\ls -> cell :: ls)
                                     (sequence xs))
               [] -> Random.list 0 (genCell 0 0)
     in sequence (List.map (genCell rowNum) (List.range 1 numOfCols))
  

genTable : Int -> Int -> Random.Generator Table
genTable numOfCols numOfRows =
    --sequence is there to swap structures ie: from List (Random.Generator (List Cell)) to Random.Generator (List (List Cell))
    let randomTable = List.map (genRow numOfCols) (List.range 1 numOfRows)
        sequence lst =
            case lst of
                x :: xs ->
                    x
                    |> Random.andThen (\row -> Random.map (\ls -> row :: ls)
                                      (sequence xs))
                [] -> Random.list 0 (genRow 0 0)
     in (sequence randomTable)

--------------------
--Recoloring parts of the table
--------------------

reColor : Color -> Cell -> Table -> Table
reColor newColor inpCell table =
     changeCell inpCell.index table (\cell -> {cell | color = newColor})

changeCell : Index -> Table -> (Cell -> Cell) -> Table
changeCell {col, row} table f =
     modifyElement row table (\tableRow -> modifyElement col tableRow f)

modifyElement : Int -> List a -> (a -> a) -> List a
modifyElement n list g =
    let result = Maybe.map3 (\a b c -> a ++ (b :: c))
                 (Just (List.take (n-1) list))
                 (Maybe.map g (List.head (List.drop (n-1) list)))
                 (Just (List.drop n list))
    in Maybe.withDefault list result
            --If the index is out of bounds, we give back the original list

findCell : Index -> Table -> Maybe Cell
findCell {row, col} table =
    let tableHeight = List.length table
        tableWidth = Maybe.withDefault 0 (Maybe.map List.length (List.head table))
     in case (   1 <= row && row <= tableHeight
              && 1 <= col && col <= tableWidth
             ) of
            False -> Nothing
            True -> 
                let foundRow = List.head ( List.drop (row - 1) table)
                in foundRow
                    |> Maybe.andThen (\list -> List.head (List.drop (col - 1) list))

cellAbove : Table -> Cell -> Maybe Cell
cellAbove table cell =
    let {col, row} = cell.index
    in findCell {col = col, row = row - 1} table

cellBelow : Table -> Cell -> Maybe Cell
cellBelow table cell =
    let {col, row} = cell.index
    in findCell {col = col, row = row + 1} table

cellLeft : Table -> Cell -> Maybe Cell
cellLeft table cell =
    let {col, row} = cell.index
    in findCell {col = col - 1, row = row} table

cellRight : Table -> Cell -> Maybe Cell
cellRight table cell =
    let {col, row} = cell.index
    in findCell {col = col + 1, row = row} table


type Direction = Up | Down | Left | Right


recolorRegion : Color ->  Table -> Index -> Maybe Table
recolorRegion inpColor inpTable inpIndex =
    let startingCell = findCell inpIndex inpTable
        startingColor = Maybe.map .color startingCell
                
        checkNext : Direction -> Table -> Maybe Table
        checkNext direction nextTable =
            let currentCell = findCell inpIndex inpTable
                nextCell = case direction of
                               Up -> Maybe.andThen (cellAbove nextTable) currentCell
                               Down -> Maybe.andThen (cellBelow nextTable) currentCell
                               Left -> Maybe.andThen (cellLeft nextTable) currentCell
                               Right -> Maybe.andThen (cellRight nextTable) currentCell
            in case startingColor == (Maybe.map .color nextCell) of
                   True ->
                       (Maybe.map .index nextCell)
                           |> Maybe.andThen (recolorRegion inpColor nextTable)
                   False -> Just nextTable

    in Maybe.map3 reColor (Just inpColor) startingCell (Just inpTable)
        |> Maybe.andThen (checkNext Up)
        |> Maybe.andThen (checkNext Right)
        |> Maybe.andThen (checkNext Down)
        |> Maybe.andThen (checkNext Left)

module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, input, button, div, text, table, td, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Random
import Tuple exposing (first)
import Element as El exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events

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
    , isOpen : Bool
    }
    
init : () -> (Model, Cmd Msg)
init _ =
    (Model emptyTable (Settings 10 10 0 "" False) (Index 1 1 ) 0
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
    | OpenSettings
    | CloseSettings

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

           OpenSettings ->
               let newSettings = {settings | isOpen = True}
               in ({model | settings = newSettings}, Cmd.none)

           CloseSettings ->
               let newSettings = {settings | isOpen = False}
               in ({model | settings = newSettings}, Cmd.none)

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none  

-- VIEW

view : Model -> Html Msg
view model =
    El.layout [ Background.color <| El.rgb255 0 0 0] <|
        column [El.height fill, El.width fill]
            [ viewTopBar model.settings
            , viewChangeColorBar model.numberOfSteps
            , viewTable model.table
            , viewGeneratePuzzleButton
            ]

viewTopBar : Settings -> Element Msg
viewTopBar settings =
    El.row [ El.height <| El.px 50
           , El.width fill
           , Background.color <| El.rgb255 92 99 118
           , Font.color <| (El.rgb255 255 255 255)
           ]
           [ viewSettings settings 
           ]

viewSettings : Settings -> Element Msg
viewSettings settings =
    let menuWidth = El.px 200
        menu =
            El.column
                [ El.width fill
                
                ]
                [ viewWidthCounter settings.setWidth
                , viewHeightCounter settings.setHeight
                , viewGenerateTableFromSeedButton
                , viewSeedInputField
                ]
    in case settings.isOpen of
           False -> 
               El.el
                   [ Events.onMouseEnter (UpdateSettings OpenSettings)
                   , El.height fill
                   , El.width menuWidth
                   ]
                   (El.el [El.centerY, El.centerX] (El.text "Settings"))
           True ->
               El.el
                   [ Events.onMouseLeave (UpdateSettings CloseSettings)
                   , El.height fill
                   , El.width menuWidth
                   , El.below menu
                   
                   ]
                   (El.el [El.centerY, El.centerX] (El.text "Settings"))

viewGenerateTableFromSeedButton : El.Element Msg
viewGenerateTableFromSeedButton =
    El.el
        [ El.height (El.px 35)
        , El.width fill
        , Background.color <| El.rgb255 92 99 118
        ] <|
        Input.button
            [ Background.color <| El.rgb255 255 128 0
            , centerX
            , centerY
            , El.height (El.px 25)
            , El.width (El.px 120)
            , Font.color <| El.rgb255 0 0 0
            , Font.center
            , Border.rounded 10
            , noFocusShadow
            ]
            { onPress = Just GenerateTableFromSeed
            , label = El.text "Generate"
            }

viewSeedInputField : El.Element Msg
viewSeedInputField =
    El.el
        [ El.height (El.px 35)
        , El.width fill
        , Background.color <| El.rgb255 92 99 118
        , Border.roundEach
                      { topLeft = 0
                      , topRight = 0
                      , bottomLeft = 30
                      , bottomRight = 30
                      }
        ] <|
        Input.text
            [ El.height (El.px 30)
            , El.width (El.px 120)
            , El.padding 5
            , centerX
            , centerY
            , Font.color <| El.rgb255 0 0 0
            , noFocusShadow
            
            ]
            { onChange = (UpdateSettings << UpdateSeed)
            , text = ""
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }


counterButton : String -> Msg -> El.Element Msg
counterButton buttonText msg =
    Input.button
        [ El.height (El.px 25)
        , El.width (El.px 25)
        , Background.color <| El.rgb255 255 128 0
        , Border.rounded 10
        , Font.center
        , Font.color <| El.rgb255 0 0 0
        , noFocusShadow
        ]
        { onPress = Just  (msg)
        , label = El.text buttonText
        }
        
viewWidthCounter : Int -> El.Element Msg
viewWidthCounter val = 
    El.el
        [ El.height (El.px 35)
        , El.width fill
        , Background.color <| El.rgb255 92 99 118
        ]
        ( El.row [El.centerX]
              [ El.text "Width: "
              , counterButton "-" (UpdateSettings DecrementWidth)
              , El.text (String.fromInt val)
              , counterButton "+" (UpdateSettings IncrementWidth)
              ]
        )

viewHeightCounter : Int -> El.Element Msg
viewHeightCounter val =
    El.el
        [ El.centerX
        , El.height (El.px 35)
        , El.width fill
        , Background.color <| El.rgb255 92 99 118
        ]
        ( El.row [El.centerX]
              [ El.text "Height: "
              , counterButton "-" (UpdateSettings DecrementHeight)
              , El.text (String.fromInt val)
              , counterButton "+" (UpdateSettings IncrementHeight)
              ]
        )

viewCell : Cell -> Element Msg
viewCell cell =
    El.el []( Input.button [ Background.color <| colorToRGB cell.color
                            , El.height <| El.px 35
                            , El.width <| El.px 35
                           , El.focused [Border.shadow {offset = (0,0)
                                                       , size = 3
                                                       , blur = 0
                                                       , color = El.rgb255  255 255 255}]
                           ]
                           { onPress = Just <| NewPosition cell.index
                           , label = El.text ""}
          )

colorToRGB : Color -> El.Color
colorToRGB color =
    case color of
        Red -> El.rgb255 255 0 0
        Blue -> El.rgb255 0 0 255
        Green -> El.rgb255 0 255 0
        Yellow -> El.rgb255 255 255 0
        Purple -> El.rgb255 139 0 139

colorToString : Color -> String
colorToString color =
    case color of
        Red -> "Red"
        Blue -> "Blue"
        Green -> "Green"
        Yellow -> "Yellow"
        Purple -> "Purple"

viewRow : List Cell -> Element Msg
viewRow cells =
    El.row [spacing 3] (List.map viewCell cells)

viewTable : Table -> Element Msg
viewTable rows =
    El.column [spacing 3, padding 30, centerX] (List.map viewRow rows)

viewGeneratePuzzleButton : El.Element Msg
viewGeneratePuzzleButton =
    Input.button
        [ centerX
        , Background.color <| El.rgb255 255 128 0
        , padding 20
        --, Font.color <| El.rgb255 255 255 255
        , Font.letterSpacing 2
        , Border.rounded 20
        , El.mouseOver [Border.glow (El.rgb255 255 128 0) 5]
        , noFocusShadow
        ]
        { onPress = Just GenerateRandomSeed
        , label = El.text "Generate puzzle"
        }
                    
noFocusShadow : El.Attribute msg
noFocusShadow =
     El.focused [Border.shadow {offset = (0,0)
                                    , size = 0
                                    , blur = 0
                                    , color = El.rgb255 139 0 139}]

viewCurrentPosition : Index -> Html Msg
viewCurrentPosition ind =
    div []
        [ Html.text ("The currently selected field is: "
               ++ (String.fromInt ind.row)
               ++ ", "
               ++ (String.fromInt ind.col))
        ]

viewChangeColorButton : Color -> El.Element Msg
viewChangeColorButton color =
    Input.button
        [ centerX
        , Background.color <| colorToRGB color
        , El.width <| El.px 70
        , El.height <| El.px 50
        , Border.rounded 20
        , noFocusShadow
        , Border.color <| El.rgb255 92 99 118
        , Border.widthEach { bottom = 3
                           , left = 0
                           , right = 3
                           , top = 0
                           }
        , El.mouseOver [Border.shadow { offset = (2,2)
                                     , size = 2
                                     , blur = 8
                                     , color = El.rgb255 255 255 255}]
        ]
          { onPress  = Just (ChangeColor color)
          , label = El.text ""
          }

viewChangeColorBar : Int -> El.Element Msg
viewChangeColorBar steps =
    El.column
        [ centerX
        , Font.color <| El.rgb255 255 255 255
        , spacing 30
        , paddingEach {edges | top = 60}
        ]
        [ El.text "Push one of these buttons to recolor the area containing the currently selected square!"
        , El.row
              [ spacing 40
              , centerX
              ]
              [ viewChangeColorButton Red
              , viewChangeColorButton Blue
              , viewChangeColorButton Green
              , viewChangeColorButton Yellow
              , viewChangeColorButton Purple
              ]
        ,El.el
            [ centerX
            , paddingEach {edges | top = 30, bottom = 0}
            ]
             ( El.text <| "Number of steps so far : "
                  ++ String.fromInt steps
             )
             ]

edges = { top = 30
        , left = 30
        , right = 30
        , bottom = 30
        }
             
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

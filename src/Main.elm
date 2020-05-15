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
import Browser.Events
import Json.Decode as Decode

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
    , isMonochrome : Bool
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
    (Model emptyTable (Settings 10 10 0 "" False) (Index 1 1 ) 0 False
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
    | KeyPress Key

type SettingsChanges =
      IncrementWidth
    | DecrementWidth
    | IncrementHeight
    | DecrementHeight
    | UpdateSeed String
    | OpenSettings
    | CloseSettings

type Key =
      Character Char
    | Control String

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
    case msg of
        GenerateRandomSeed ->
            (model , Random.generate GenerateSeedAndTable randInt)

        GenerateSeedAndTable newInt ->
            let settings = model.settings
                newSettings = {settings | seed = newInt }
            in ( updateTable {model | settings = newSettings}
               , Cmd.none)
        
        GenerateTableFromSeed ->
            updateGenerateTableFromSeed model
            
        UpdateSettings changes ->            
            updateSettings changes model
                
        NewPosition newIndex ->
             ({model | currentSelection = newIndex}, Cmd.none)

        ChangeColor color ->
           updateChangeColor model color

        KeyPress key ->
            updateKeyPress model key
            
                              


--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Browser.Events.onKeyPress keyDecoder

                      
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map tokey (Decode.field "key" Decode.string)

tokey : String -> Msg
tokey keyValue =
    case String.uncons keyValue of
        Just ( char, "") ->
            ( KeyPress << Character) char

        _ ->
            ( KeyPress << Control) keyValue

-- VIEW

view : Model -> Html Msg
view model =
    let content = 
          column
            [ El.width fill
            , El.height fill
            ]
            [ viewTopBar model.settings
            , column
                  [ Background.color black
                  , El.scrollbars
                  , El.width fill
                  , El.height fill
                  ]
                  [ viewChangeColorBar model.numberOfSteps
                  , viewTable model.table
                  , viewGeneratePuzzleButton
                  , viewSeedTip model
                  ]
            ]
    in case model.isMonochrome of
           False ->
               El.layout [ El.height fill] <| content
           True ->
               El.layout ( [ El.height fill ] ++ (viewCongrats model) ) <| content
                
--------------------------------------------------
--View helper functions
--------------------------------------------------

--color palett
black = El.rgb255 0 0 0
white = El.rgb255 255 255 255
orange = El.rgb255 255 128 0
grey = El.rgb255 92 99 118

viewCongrats : Model -> List (El.Attribute Msg)
viewCongrats model  =
    [ El.inFront
          (El.el
              [ El.width <| El.px 800
              , El.height <| El.px 100
              , El.centerX
              , El.centerY
              , Background.color grey
              , Border.rounded 40
              , El.alpha 0.93
              ] <| El.none)
    , El.inFront
        ( El.el
            [ El.centerX
            , El.centerY
            ] <|
                El.textColumn
                     [ El.width fill
                     , Font.center
                     ]
                     [ El.el [ Font.size 40 ] <| El.text
                           "Congratulations!"
                     , El.text "You have solved the puzzle in : "
                     , El.text <|  String.fromInt model.numberOfSteps
                     , El.text " steps!"
                     ]
        )
    ]
            
viewTopBar : Settings -> Element Msg
viewTopBar settings =
    El.row [ El.height <| El.px 50
           , El.width fill
           , Background.color grey 
           , Font.color white
           ]
           [ viewSettings settings 
           ]

viewSeedTip : Model -> Element Msg
viewSeedTip model =
    let (rows,cols)  = tableSize model.table
        settingsToString =
               String.fromInt rows
            ++ ","
            ++ String.fromInt cols
            ++ ","
            ++ String.fromInt model.settings.seed
    in El.column
          [ Font.color white
          , Font.center
          , centerX
          , spacing 20
          , padding 30
          ]
          [ El.text "Tip: You can share this exact puzzle with your friends to see who can solve it in fewer steps!\nTo generate a puzzle from a given seed visit the settings menu."
          ,El.el [centerX] <|
              El.text <|  "Your current seed is: " ++ settingsToString
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
                , viewSeedInputField settings
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
    El.column
        [ --El.height (El.px 35)
         El.width fill
        , Background.color grey
        , spacing 5
        ] <|
        [ El.el
              [ Font.center
              , centerX
              ]
              ( El.text "Generate puzzle\nfrom seed")
        , Input.button
              [ Background.color orange
              , centerX
              , centerY
              , El.height (El.px 25)
              , El.width (El.px 120)
              , Font.color black
              , Font.center
              , Border.rounded 10
              , noFocusShadow
              ]
              { onPress = Just GenerateTableFromSeed
              , label = El.text "Generate"
              }
        ]

viewSeedInputField : Settings -> El.Element Msg
viewSeedInputField settings =
    El.el
        [ El.height (El.px 35)
        , El.width fill
        , Background.color grey
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
            , Font.color black
            , noFocusShadow
            
            ]
            { onChange = (UpdateSettings << UpdateSeed)
            , text = settings.seedInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }


counterButton : String -> Msg -> El.Element Msg
counterButton buttonText msg =
    Input.button
        [ El.height (El.px 25)
        , El.width (El.px 25)
        , Background.color orange
        , Border.rounded 10
        , Font.center
        , Font.color black
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
        , Background.color grey
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
        , Background.color grey
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
                                                       , color = white}]
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
        , Background.color orange
        , padding 20
        , Font.letterSpacing 2
        , Border.rounded 20
        , El.mouseOver [Border.glow orange 5]
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

viewChangeColorButton : String -> Color -> El.Element Msg
viewChangeColorButton label color =
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
                                     , color = white}]
        ]
          { onPress  = Just (ChangeColor color)
          , label = El.text label
          }

viewChangeColorBar : Int -> El.Element Msg
viewChangeColorBar steps =
    El.column
        [ centerX
        , Font.color white
        , spacing 30
        , paddingEach {edges | top = 60}
        , Font.center
        ]
        [ El.text "Push one of these buttons, or press the appropriate key to recolor the area\ncontaining the currently selected square!"
        , El.el [centerX] <|
            El.text " The goal is to make all the squares the same color."
        , El.row
              [ spacing 40
              , centerX
              , Font.color black
              ]
              [ viewChangeColorButton "1" Red
              , viewChangeColorButton "2" Blue
              , viewChangeColorButton "3" Green
              , viewChangeColorButton "4" Yellow
              , viewChangeColorButton "5" Purple
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
--------------------------------------------------            
--Table data structure and helper funcitons
--------------------------------------------------

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

tableSize : Table -> (Int,Int)
tableSize table =
    let rows = List.length table
        mcols = Maybe.map List.length (List.head table)
    in case mcols of
           Nothing -> (0,0)
           Just n -> (rows,n)

isMonochrome : Table -> Bool
isMonochrome table = 
    let colorTable = List.concat <| List.map (List.map .color) table
    in case colorTable of
           (x :: xs) -> List.all ((==) x) xs
           [] -> False
--------------------------------------------------
--Random generation
--------------------------------------------------

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

--------------------------------------------------
--Recoloring parts of the table
--------------------------------------------------

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

    in case startingColor == Just inpColor of
           True ->
               Just inpTable
           False ->
               Maybe.map3 reColor (Just inpColor) startingCell (Just inpTable)
                   |> Maybe.andThen (checkNext Up)
                   |> Maybe.andThen (checkNext Right)
                   |> Maybe.andThen (checkNext Down)
                   |> Maybe.andThen (checkNext Left)

--------------------------------------------------
--Update helper functions
--------------------------------------------------

--This function updates the table to match the current settings
updateTable : Model -> Model
updateTable model  =
    let settings = model.settings
        newTable =
            first ( Random.step (genTable settings.setWidth settings.setHeight)
                        (Random.initialSeed settings.seed)
                  ) --This maybe.withdefault should never get a
                    --Nothing value, because we patternmatch on the
                    --settings.seed value before calling updateTable.
        monochromeCheck = isMonochrome newTable
    in { model | currentSelection = (Index 1 1)
               , table = newTable
               , numberOfSteps = 0
               , isMonochrome = monochromeCheck
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

updateGenerateTableFromSeed : Model -> (Model,Cmd Msg)
updateGenerateTableFromSeed model =
    case List.map String.toInt <| String.split "," <| model.settings.seedInput of
        [Just rowNum, Just colNum,Just seedNum] ->
            let settings = model.settings
                newSettings = {settings | seed = seedNum
                                        , setHeight = rowNum
                                        , setWidth = colNum
                              }
            in (updateTable {model | settings = newSettings}, Cmd.none)
        _ -> (model,Cmd.none)
                
updateKeyPress : Model -> Key -> (Model,Cmd Msg)
updateKeyPress model key =
    case model.settings.isOpen of
        True ->
            case key of
                Control "Enter" ->
                    updateGenerateTableFromSeed model
                _ -> (model,Cmd.none)
        False -> 
            case key of
                Character '1' ->
                    updateChangeColor model Red

                Character '2' ->
                    updateChangeColor model Blue

                Character '3' ->
                    updateChangeColor model Green

                Character '4' ->
                    updateChangeColor model Yellow

                Character '5' ->
                    updateChangeColor model Purple
                        
                _ -> (model, Cmd.none)

updateChangeColor : Model -> Color -> (Model, Cmd Msg)
updateChangeColor model color =
     let newTableM = recolorRegion color  model.table model.currentSelection
         currentNumberOfSteps = model.numberOfSteps
            in case newTableM of
                   Nothing -> (model , Cmd.none)
                   Just newTable ->
                       let newModel =  {model | table = newTable
                                       , numberOfSteps =
                                           currentNumberOfSteps + 1
                                       }
                       in case isMonochrome newModel.table of
                              True -> ({newModel | isMonochrome =
                                            True},Cmd.none)
                              False -> ({newModel | isMonochrome =
                                             False}
                                       ,Cmd.none)

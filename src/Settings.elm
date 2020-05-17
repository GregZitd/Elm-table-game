module Settings exposing (Settings,ShouldTableUpdate,Changes,update,subscriptions,view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events
import Browser.Events
import Json.Decode as Decode

-- MODEL

type alias Settings =
    --These are the user input for generating a custom size table, thus they can be different from the actual size of the current table.
    { setWidth : Int
    , setHeight : Int
    , seed : Int
    , seedInput : String
    , isOpen : Bool
    }


-- UPDATE

type alias ShouldTableUpdate = Bool

type Changes  =
      IncrementWidth
    | DecrementWidth
    | IncrementHeight
    | DecrementHeight
    | UpdateSeed String
    | OpenSettings
    | CloseSettings
    | ClickedGenerateTableFromSeed
    | KeyPress Key


update : Changes -> Settings -> (Settings, ShouldTableUpdate)
update changes settings =
    let width = settings.setWidth
        height = settings.setHeight
    in case changes of

           IncrementWidth ->
               ({settings | setWidth = width + 1}, False)
              
           DecrementWidth ->
               ({settings | setWidth = width - 1}, False)
              
           IncrementHeight ->
               ({settings | setHeight = height + 1}, False)
              
           DecrementHeight ->
               ({settings | setHeight = height - 1}, False)
              
           UpdateSeed inpText ->
               ({settings | seedInput = inpText}, False)
              
           OpenSettings ->
               ({settings | isOpen = True}, False)
              
           CloseSettings ->
               ({settings | isOpen = False}, False)

           ClickedGenerateTableFromSeed ->
               updateGenerateTableFromSeed settings

           KeyPress key ->
               case key of
                   Enter ->
                       case settings.isOpen of
                           True ->
                               updateGenerateTableFromSeed settings
                           False -> (settings,False)

                   IgnoreKey ->
                       (settings,False)

updateGenerateTableFromSeed : Settings -> (Settings, ShouldTableUpdate)
updateGenerateTableFromSeed settings =
    case List.map String.toInt <| String.split "," <| settings.seedInput of
                   [Just rowNum, Just colNum,Just seedNum] ->
                       let newSettings = {settings | seed = seedNum
                                         , setHeight = rowNum
                                         , setWidth = colNum
                                         }
                       in (newSettings, True)
                   _ -> (settings,False)
    
-- SUBSCRIPTIONS

type Key = Enter | IgnoreKey

subscriptions : Sub Changes
subscriptions = Browser.Events.onKeyPress enterDecoder

enterDecoder : Decode.Decoder Changes
enterDecoder =
    Decode.map isEnter (Decode.field "key" Decode.string)

isEnter : String -> Changes
isEnter keyValue =
    case keyValue of
        "Enter" -> KeyPress Enter
        _ -> KeyPress IgnoreKey
        
-- VIEW

view : Settings -> Element Changes
view settings =
    let menuWidth = px 200
        menu =
            column
                [ width fill
                
                ]
                [ viewWidthCounter settings.setWidth
                , viewHeightCounter settings.setHeight
                , viewGenerateTableFromSeedButton
                , viewSeedInputField settings
                ]
    in case settings.isOpen of
           False -> 
               el
                   [ Events.onMouseEnter OpenSettings
                   , height fill
                   , width menuWidth
                   ]
                   (el [ centerY
                       , centerX
                       , Events.onClick OpenSettings
                       , pointer
                       ]
                        (text "Settings")
                   )
           True ->
               el
                   [ Events.onMouseLeave CloseSettings
                   , height fill
                   , width menuWidth
                   , below menu
                   
                   ]
                   (el [ centerY
                       , centerX
                       , Events.onClick CloseSettings
                       , pointer
                       ]
                       (text "Settings")
                   )

viewGenerateTableFromSeedButton : Element Changes
viewGenerateTableFromSeedButton =
    column
        [ --height (px 35)
         width fill
        , Background.color grey
        , spacing 5
        ] <|
        [ el
              [ Font.center
              , centerX
              ]
              ( text "Generate puzzle\nfrom seed")
        , Input.button
              [ Background.color orange
              , centerX
              , centerY
              , height (px 25)
              , width (px 120)
              , Font.color black
              , Font.center
              , Border.rounded 10
              , noFocusShadow
              ]
              { onPress = Just ClickedGenerateTableFromSeed
              , label = text "Generate"
              }
        ]

viewSeedInputField : Settings -> Element Changes
viewSeedInputField settings =
    el
        [ height (px 35)
        , width fill
        , Background.color grey
        , Border.roundEach
                      { topLeft = 0
                      , topRight = 0
                      , bottomLeft = 30
                      , bottomRight = 30
                      }
        ] <|
        Input.text
            [ height (px 30)
            , width (px 120)
            , padding 5
            , centerX
            , centerY
            , Font.color black
            , noFocusShadow
            
            ]
            { onChange = UpdateSeed
            , text = settings.seedInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }


counterButton : String -> Changes -> Element Changes
counterButton buttonText msg =
    Input.button
        [ height (px 25)
        , width (px 25)
        , Background.color orange
        , Border.rounded 10
        , Font.center
        , Font.color black
        , noFocusShadow
        ]
        { onPress = Just  (msg)
        , label = text buttonText
        }
        
viewWidthCounter : Int -> Element Changes
viewWidthCounter val = 
    el
        [ height (px 35)
        , width fill
        , Background.color grey
        ]
        ( row [centerX]
              [ text "Width: "
              , counterButton "-" (DecrementWidth)
              , text (String.fromInt val)
              , counterButton "+" (IncrementWidth)
              ]
        )

viewHeightCounter : Int -> Element Changes
viewHeightCounter val =
    el
        [ centerX
        , height (px 35)
        , width fill
        , Background.color grey
        ]
        ( row [centerX]
              [ text "Height: "
              , counterButton "-" (DecrementHeight)
              , text (String.fromInt val)
              , counterButton "+" (IncrementHeight)
              ]
        )

noFocusShadow : Attribute msg
noFocusShadow =
     focused [Border.shadow {offset = (0,0)
                                    , size = 0
                                    , blur = 0
                                    , color = rgb255 139 0 139}]

--color palett
black = rgb255 0 0 0
white = rgb255 255 255 255
orange = rgb255 255 128 0
grey = rgb255 92 99 118

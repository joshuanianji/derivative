module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, fill, height, padding, px, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Encode
import Keyboard exposing (Key(..))
import Math exposing (Expr(..), MathError(..))
import Ports


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { latexStr : String
    , expr : Result MathError Expr
    , derivative : Result MathError Expr
    , debug : Bool
    , tutorial : Bool
    , pressedKeys : List Key
    , blocks : Dict String Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { latexStr = ""
      , expr = Math.initExpr
      , derivative = Math.initExpr
      , debug = False
      , tutorial = True
      , pressedKeys = []
      , blocks =
            Dict.fromList
                [ ( "FunctionInput", True )
                , ( "Derivative", True )
                , ( "Credits", False )
                , ( "Overview", True )
                , ( "Supported Features", False )
                , ( "Caveats", True )
                , ( "Keyboard Shortcuts", True )
                ]
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    (if model.tutorial then
        tutorial model

     else
        derivativeView model
    )
        |> Element.el
            [ width fill
            , height fill
            , Element.paddingXY 50 70
            ]
        -- surrounds with borders
        |> (\el ->
                Element.row
                    [ width fill, height fill ]
                    [ Element.el [ width <| Element.fillPortion 1, height fill ] Element.none
                    , Element.el [ width <| Element.fillPortion 4, height fill ] el
                    , Element.el [ width <| Element.fillPortion 1, height fill ] Element.none
                    ]
           )
        |> Element.layout
            [ Font.family
                [ Font.typeface "Computer Modern" ]
            ]



-- Tutorial


tutorial : Model -> Element Msg
tutorial model =
    Element.column
        [ Element.spacing 32
        , width fill
        , height fill
        ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            ]
            [ text "Symbolic Differentiation Calculator" ]

        -- i NEED to make the Info & Help Guide a paragraph, not an Element.el or else it'll shift it one pixel up lol.
        , Element.paragraph [ Font.center ] [ text "Info & Help Guide" ]
        , Element.el [ centerX ] <| tutorialToggle model
        , block model
            { title = ( 1, "Overview" )
            , body = overview
            , toggleMsg = ToggleBlock "Overview"
            , get = Dict.get "Overview"
            }
        , block model
            { title = ( 2, "Supported Features" )
            , body = supportedFeatures
            , toggleMsg = ToggleBlock "Supported Features"
            , get = Dict.get "Supported Features"
            }
        , block model
            { title = ( 3, "Caveats" )
            , body = caveats
            , toggleMsg = ToggleBlock "Caveats"
            , get = Dict.get "Caveats"
            }
        , block model
            { title = ( 4, "Keyboard Shortcuts" )
            , body = keyboardShortcuts
            , toggleMsg = ToggleBlock "Keyboard Shortcuts"
            , get = Dict.get "Keyboard Shortcuts"
            }
        ]


overview : Element Msg
overview =
    Element.paragraph
        [ spacing 4 ]
        [ text "Welcome to my Derivative Calculator! "
        , text "This project, inspired by "
        , link "this Haskell blog post" "http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html"
        , text ", aims to provide symbolic differentiation with user friendly input and output. Read further to see supported features and caveats, or exit this help guide and get started!"
        ]


supportedFeatures : Element Msg
supportedFeatures =
    let
        supportedFeaturesTable =
            [ { feature = "Four Basic Operations"
              , display = "+ - \\cdot \\frac{a}{b}"
              , typed = "+ - * a/b"
              }
            , { feature = "Exponents"
              , display = "a^b"
              , typed = "a^b"
              }
            , { feature = "Six Major Trigonometric Functions"
              , display = "sinx \\cdot \\tan (3 \\cdot \\pi x)"
              , typed = "sinx * tan(3 * pi x)"
              }
            , { feature = "Square Root"
              , display = "\\sqrt{}"
              , typed = "sqrt"
              }
            , { feature = "Logarithm"
              , display = "\\ln"
              , typed = "ln"
              }
            , { feature = "Variables"
              , display = "a \\cdot a_b \\cdot a_{pple}"
              , typed = "a * a_b * a_pple"
              }
            ]

        header str =
            Element.paragraph
                [ Font.bold
                , Font.size 26
                ]
                [ Element.text str ]

        viewInTable elem =
            Element.paragraph [ centerY ] [ elem ]
    in
    Element.textColumn
        [ spacing 16
        , width fill
        ]
        [ Element.paragraph
            [ spacing 4 ]
            [ text "I used the "
            , link "MathQuill" "http://mathquill.com/"
            , text " library, the same one "
            , link "Desmos" "http://desmos.com"
            , text " uses for its calculator. Because of this, the latex input is just as intuitive as Desmos!"
            ]
        , Element.paragraph
            [ spacing 4 ]
            [ text "As of right now, this program does not support multivariable calculus or implicit differentiation, though because I treat variables like constants, one can make an argument for partial derivatives."
            ]
        , Element.paragraph
            [ spacing 4 ]
            [ text "For a full list of supported functions, refer to the table below."
            ]

        -- make a table
        , Element.table
            [ spacing 16
            , Element.paddingXY 0 16
            ]
            { data = supportedFeaturesTable
            , columns =
                [ { header = header "Function"
                  , width = fill
                  , view =
                        \f -> viewInTable <| Element.text f.feature
                  }
                , { header = header "Display"
                  , width = fill
                  , view =
                        \f -> viewInTable <| staticMath f.display
                  }
                , { header = header "What to Type"
                  , width = fill
                  , view =
                        \f -> viewInTable <| typed f.typed
                  }
                ]
            }
        ]


caveats : Element Msg
caveats =
    Element.textColumn
        [ spacing 16 ]
        [ Element.paragraph
            [ spacing 4 ]
            [ text "As cool as this project and its supporting libraries are, there are some limitations and restrictions." ]
        , Element.
        ]


keyboardShortcuts : Element Msg
keyboardShortcuts =
    let
        shortcutsTable =
            [ { toggle = "Calculate Derivative"
              , typed = "ENTER"
              }
            , { toggle = "Clear"
              , typed = "SHIFT + C"
              }
            , { toggle = "Toggle Debug"
              , typed = "SHIFT + D"
              }
            , { toggle = "Toggle Tutorial"
              , typed = "SHIFT + T"
              }
            ]

        header str =
            Element.paragraph
                [ Font.bold
                , Font.size 26
                ]
                [ Element.text str ]

        viewInTable elem =
            Element.paragraph [ centerY ] [ elem ]
    in
    Element.table
        [ spacing 16
        , Element.paddingXY 0 16
        ]
        { data = shortcutsTable
        , columns =
            [ { header = header "Function"
              , width = fill
              , view =
                    \f -> viewInTable <| Element.text f.toggle
              }
            , { header = header "Keyboard"
              , width = fill
              , view =
                    \f -> viewInTable <| typed f.typed
              }
            ]
        }


tutorialToggle : Model -> Element Msg
tutorialToggle model =
    Element.el
        [ Border.width 1
        , Border.rounded 21
        , Border.color <| Element.rgb 0 0 0
        , height <| px 42
        , width <| px 42
        , Element.pointer
        , Events.onClick ToggleTutorial
        ]
    <|
        Element.el
            [ centerX
            , centerY
            , unselectable
            ]
        <|
            if model.tutorial then
                text "╳"

            else
                text "?"



-- Derivative


derivativeView : Model -> Element Msg
derivativeView model =
    Element.column
        [ Element.spacing 32
        , width fill
        , height fill
        ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            ]
            [ text "Symbolic Differentiation Calculator" ]
        , Element.paragraph
            [ width fill
            , Font.center
            ]
            [ text "Created by "
            , link "Joshua Ji" "https://github.com/joshuanianji/Derivative"
            ]
        , Element.el [ centerX ] <| tutorialToggle model
        , block model
            { title = ( 1, "Function Input" )
            , body =
                Element.column
                    [ width fill, spacing 8 ]
                    [ input model
                    , if model.debug then
                        latexToExpr model.expr

                      else
                        Element.none
                    ]
            , toggleMsg = ToggleBlock "FunctionInput"
            , get = Dict.get "FunctionInput"
            }
        , block model
            { title = ( 2, "Derivative" )
            , body =
                Element.column
                    [ width fill, spacing 8 ]
                    [ derivative model
                    , if model.debug then
                        latexToExpr model.derivative

                      else
                        Element.none
                    ]
            , toggleMsg = ToggleBlock "Derivative"
            , get = Dict.get "Derivative"
            }
        , block model
            { title = ( 3, "Credits" )
            , body = credits
            , toggleMsg = ToggleBlock "Credits"
            , get = Dict.get "Credits"
            }
        , Element.el [ centerX ] <| debugModeToggle model
        ]


debugModeToggle : Model -> Element Msg
debugModeToggle model =
    Input.checkbox
        []
        { onChange = ToggleDebug
        , icon =
            \debugOn ->
                Element.el
                    [ Font.bold
                    , unselectable
                    ]
                <|
                    if debugOn then
                        text "Off"

                    else
                        text "On"
        , checked = model.debug
        , label =
            Input.labelLeft
                [ unselectable ]
                (text "Debug mode:")
        }


input : Model -> Element Msg
input model =
    Element.column
        [ spacing 32
        , centerX
        ]
        [ Element.el [ Element.spacing 16, centerX ] <| functionInput model
        , Element.row
            [ spacing 32
            , centerX
            ]
            [ Input.button
                [ Border.width 1
                , Border.rounded 5
                , Border.color <| Element.rgb 0 0 0
                , Element.padding 12
                ]
                { onPress = Just Clear
                , label = text "Clear"
                }
            , Input.button
                [ Border.width 1
                , Border.rounded 5
                , Border.color <| Element.rgb 0 0 0
                , Element.padding 12
                ]
                { onPress = Just Calculate
                , label = text "Calculate"
                }
            ]
        ]


latexToExpr : Result MathError Expr -> Element Msg
latexToExpr resultExpr =
    Element.column
        [ centerX ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            , spacing 16
            , Element.padding 16
            ]
            [ text "Latex → Expr" ]
        , case resultExpr of
            Ok expr ->
                Element.paragraph
                    [ Font.center ]
                    [ text <| Math.toString expr ]

            Err error ->
                Math.errorToString error
                    |> text
                    |> List.singleton
                    |> Element.paragraph
                        [ Font.center
                        , Element.spacing 16
                        ]
        ]


derivative : Model -> Element Msg
derivative model =
    (case model.derivative of
        Ok der ->
            Math.asLatex der
                |> (\s -> "\\frac{df}{dx}=" ++ s)
                |> staticMath

        Err mathError ->
            Math.errorToString mathError
                |> text
                |> List.singleton
                |> Element.paragraph [ Font.center ]
    )
        |> Element.el
            [ Font.center
            , Element.scrollbarX
            , width fill
            ]
        |> Element.el
            [ spacing 16
            , Element.padding 16
            , width fill
            ]


credits : Element Msg
credits =
    Element.column
        [ spacing 16
        , width fill
        ]
        [ text "Special thanks to:"
        , Element.paragraph [] [ text "Benjamin Kovach, for writing ", link "Symbolic Calculus in Haskell" "http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html", text ", the blog post which inspired this project." ]
        , Element.paragraph [] [ link "MathQuill" "http://mathquill.com/", text ", for making it so easy manage LaTeX input and output." ]
        , Element.paragraph [] [ link "Create Elm App" "https://github.com/halfzebra/create-elm-app", text " with which this project is bootstrapped." ]
        , Element.paragraph [] [ link "Dmy" "https://github.com/dmy", text ", for creating ", link "Elm Pratt Parser" "https://github.com/dmy/elm-pratt-parser.", text ", an awesome library which implements the theoretical parser of the same name in Elm." ]
        , Element.paragraph [] [ link "The Elm Language" "https://elm-lang.org/", text " for being so easy to work with." ]
        ]



-- HELPER FUNCTIONS


type alias BlockData =
    { title : ( Int, String )
    , body : Element Msg
    , toggleMsg : Msg
    , get : Dict String Bool -> Maybe Bool
    }


block : Model -> BlockData -> Element Msg
block model blockData =
    let
        isOpen =
            Maybe.withDefault False (blockData.get model.blocks)

        heading =
            Element.row
                [ width fill
                , Font.bold
                , Font.size 30
                , Element.paddingXY 0 24
                , if isOpen then
                    -- black font
                    Font.color <| Element.rgb 0 0 0

                  else
                    -- gray font
                    Font.color <| Element.rgb255 169 169 169
                , Element.pointer
                , Events.onClick blockData.toggleMsg
                , unselectable
                ]
                [ Tuple.first blockData.title
                    |> String.fromInt
                    |> text
                    |> List.singleton
                    |> Element.paragraph [ width (px 30) ]
                , Element.paragraph [ Element.paddingXY 16 0 ]
                    [ text <| Tuple.second blockData.title ]
                ]
    in
    Element.column
        [ spacing 8
        , width fill
        ]
        [ heading
        , if isOpen then
            blockData.body
                |> Element.el
                    [ Element.paddingXY 46 0
                    , width fill
                    ]

          else
            Element.none
        ]


link : String -> String -> Element Msg
link linkName url =
    Element.newTabLink
        [ width Element.shrink
        , Font.color <| Element.rgb255 0 63 135

        -- , Font.underline
        ]
        { url = url
        , label = text linkName
        }



-- represents a keyboard character


typed : String -> Element Msg
typed char =
    Element.el
        [ Font.family
            [ Font.typeface "Courier New"
            , Font.monospace
            ]
        , Border.color <| Element.rgb 0 0 0
        , Border.width 1
        , Border.rounded 3
        , Background.color <| Element.rgb255 220 220 220
        , Element.paddingXY 4 2
        , Font.size 20
        ]
    <|
        Element.text char


unselectable : Element.Attribute Msg
unselectable =
    Element.htmlAttribute (Html.Attributes.style "user-select" "none")



-- web component stuff


functionInput : Model -> Element Msg
functionInput _ =
    Html.div []
        [ Html.node "mathquill-static"
            [ Html.Attributes.property "latexValue" <|
                Json.Encode.string "f(x)="
            ]
            []
        , Html.span
            [ Html.Attributes.style "display" "inline-block"
            , Html.Attributes.style "height" "shrink"
            , Html.Attributes.style "width" "shrink"
            ]
            [ Html.node "mathquill-input" [] [] ]
        ]
        |> Element.html
        |> Element.el [ Element.scrollbarX ]


staticMath : String -> Element Msg
staticMath latexStr =
    Html.node "mathquill-static"
        [ Html.Attributes.property "latexValue" <|
            Json.Encode.string latexStr
        ]
        []
        |> Element.html



-- UPDATE


type Msg
    = ToggleDebug Bool
    | ToggleTutorial
    | ToggleBlock String -- because I use a string dictionary lel
    | ChangedLatexStr String
    | Calculate
    | Clear
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDebug currentDebugMode ->
            ( { model | debug = currentDebugMode }, Cmd.none )

        ToggleTutorial ->
            ( { model | tutorial = not model.tutorial }, Cmd.none )

        ToggleBlock blockSelect ->
            let
                blocks =
                    model.blocks

                newBlocks =
                    Dict.update
                        blockSelect
                        (Maybe.map not)
                        blocks
            in
            ( { model | blocks = newBlocks }, Cmd.none )

        ChangedLatexStr latexStr ->
            ( { model
                | latexStr = latexStr
                , expr = Math.fromLatex latexStr
              }
            , Cmd.none
            )

        Calculate ->
            ( { model | derivative = Math.derivative model.expr }
            , Cmd.none
            )

        Clear ->
            ( { model
                | latexStr = ""
                , expr = Math.initExpr
                , derivative = Math.initExpr
              }
            , Ports.clear ()
            )

        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.pressedKeys

                -- possible shortcuts in order of precedence. If multiple are valid we enact the first one
                possibleShortcuts =
                    [ ( [ Keyboard.Enter ], Calculate ) --calculate derivative
                    , ( [ Keyboard.Shift, Keyboard.Character "C" ], Clear ) -- clear
                    , ( [ Keyboard.Shift, Keyboard.Character "D" ], ToggleDebug (not model.debug) ) -- debug
                    , ( [ Keyboard.Shift, Keyboard.Character "T" ], ToggleTutorial ) -- tutorial
                    ]

                shortcuts =
                    List.filterMap
                        (\( keys, message ) ->
                            -- if all the keys are currently being held down
                            if List.all (\k -> List.member k newKeys) keys then
                                Just message

                            else
                                Nothing
                        )
                        possibleShortcuts

                newModel =
                    { model | pressedKeys = newKeys }
            in
            case shortcuts of
                message :: _ ->
                    update message newModel

                [] ->
                    ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.changedLatex ChangedLatexStr
        , Sub.map KeyMsg Keyboard.subscriptions
        ]

module Main exposing (main)

import Browser
import Element exposing (Element, centerX, centerY, fill, height, padding, px, spacing, width)
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
    , showCredits : Bool
    , tutorial : Bool
    , pressedKeys : List Key
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { latexStr = ""
      , expr = Math.initExpr
      , derivative = Math.initExpr
      , debug = False
      , showCredits = False
      , tutorial = False
      , pressedKeys = []
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
            [ Element.text "Derivative Calculator with Latex" ]
        , Element.el [ centerX ] <| Element.text "Tutorial & Help Guide"
        , Element.el [ centerX ] <| tutorialToggle model
        ]


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
                Element.text "╳"

            else
                Element.text "?"



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
            [ Element.text "Derivative Calculator with Latex" ]
        , Element.paragraph
            [ width fill
            , Font.center
            ]
            [ Element.text "Created by "
            , link "Joshua Ji" "https://github.com/joshuanianji/Derivative"
            ]
        , Element.el [ centerX ] <| tutorialToggle model
        , heading 1 "Derivative Input"
        , input model
        , if model.debug then
            latexToExpr model.expr

          else
            Element.none
        , heading 2 "Derivative"
        , derivative model
        , if model.debug then
            latexToExpr model.derivative

          else
            Element.none
        , Element.el [ centerX ] <| debugModeToggle model
        , credits model
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
                        Element.text "Off"

                    else
                        Element.text "On"
        , checked = model.debug
        , label =
            Input.labelLeft
                [ unselectable ]
                (Element.text "Turn debug mode")
        }


heading : Int -> String -> Element Msg
heading num str =
    Element.paragraph
        [ width fill
        , Font.bold
        , Font.size 30
        , Element.paddingXY 0 24
        ]
        [ Element.text <| String.fromInt num

        -- use padding to simulate tab
        , Element.el [ Element.paddingXY 30 0 ] <| Element.text str
        ]


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
                , label = Element.text "Clear"
                }
            , Input.button
                [ Border.width 1
                , Border.rounded 5
                , Border.color <| Element.rgb 0 0 0
                , Element.padding 12
                ]
                { onPress = Just Calculate
                , label = Element.text "Calculate"
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
            [ Element.text "Latex → Expr" ]
        , case resultExpr of
            Ok expr ->
                Element.paragraph
                    [ Font.center ]
                    [ Element.text <| Debug.toString expr ]

            Err error ->
                Math.errorToString error
                    |> Element.text
                    |> List.singleton
                    |> Element.paragraph
                        [ Font.center
                        , Element.spacing 16
                        ]
        ]



-- making sure the asLatex function works
-- exprToLatex : Model -> Element Msg
-- exprToLatex model =
--     Element.column
--         [ centerX
--         , spacing 16
--         , Border.width 1
--         , Border.color <| Element.rgb 0 0 0
--         , Element.padding 16
--         ]
--         [ Element.paragraph
--             [ Font.size 32
--             , Font.center
--             ]
--             [ Element.text "Expr -> Latex" ]
--         , Element.el [ centerX ] <|
--             Element.text <|
--                 (Result.map Math.asLatex model.expr
--                     |> Result.withDefault "Errors lol check above"
--                 )
--         ]


derivative : Model -> Element Msg
derivative model =
    (case model.derivative of
        Ok der ->
            Math.asLatex der
                |> (\s -> "\\frac{df}{dx}=" ++ s)
                |> staticMath

        Err mathError ->
            Math.errorToString mathError
                |> Element.text
    )
        |> Element.el
            [ centerX
            , Element.scrollbarX
            , width fill
            ]
        |> Element.el
            [ spacing 16
            , Element.padding 16
            , width fill
            ]


credits : Model -> Element Msg
credits model =
    let
        mainCreditColumn =
            if model.showCredits then
                Element.column
                    [ spacing 16
                    , Element.paddingXY 16 0
                    , Element.scrollbarX
                    , width fill
                    ]
                    [ Element.text "Special thanks to:"
                    , Element.paragraph [] [ link "Desmos" "http://desmos.com", Element.text ", for being such a great graphing calculator." ]
                    , Element.paragraph [] [ link "MathQuill" "http://mathquill.com/", Element.text ", for making it so easy manage LaTeX input and output." ]
                    , Element.paragraph [] [ link "Create Elm App" "https://github.com/halfzebra/create-elm-app", Element.text " with which this project is bootstrapped." ]
                    , Element.paragraph [] [ link "Dmy" "https://github.com/dmy", Element.text ", for creating the beautiful ", link "Elm Pratt Parser" "https://github.com/dmy/elm-pratt-parser." ]
                    , Element.paragraph [] [ link "The Elm Language" "https://elm-lang.org/", Element.text " for being so easy to work with." ]
                    ]

            else
                Element.none
    in
    Element.column
        [ spacing 32 ]
        [ Input.checkbox
            []
            { onChange = ToggleCredits
            , icon =
                \creditsOn ->
                    Element.el
                        [ Font.bold
                        , Element.centerY
                        , Element.alignLeft
                        , unselectable
                        ]
                    <|
                        if creditsOn then
                            Element.text "Hide Credits:"

                        else
                            Element.text "Expand Credits:"
            , checked = model.showCredits
            , label = Input.labelHidden ""
            }
        , mainCreditColumn
        ]



-- HELPER FUNCTIONS


link : String -> String -> Element Msg
link text url =
    Element.newTabLink
        [ width Element.shrink
        , Font.color <| Element.rgb255 0 63 135

        -- , Font.underline
        ]
        { url = url
        , label = Element.text text
        }


unselectable : Element.Attribute Msg
unselectable =
    Element.htmlAttribute (Html.Attributes.style "user-select" "none")


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



-- Mathquill static math using Web Components


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
    | ToggleCredits Bool
    | ToggleTutorial
    | ChangedLatexStr String
    | Calculate
    | Clear
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDebug currentDebugMode ->
            ( { model | debug = currentDebugMode }, Cmd.none )

        ToggleCredits currentCreditMode ->
            ( { model | showCredits = currentCreditMode }, Cmd.none )

        ToggleTutorial ->
            ( { model | tutorial = not model.tutorial }, Cmd.none )

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



-- keyboardSubscription : Keyboard.RawKey -> Msg
-- keyboardSubscription rawKey =
--     case Keyboard.whitespaceKey rawKey of
--         Just Enter ->
--             Calculate
--         _ ->
--             NoOp

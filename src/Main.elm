module Main exposing (main)

import Browser
import Element exposing (Element, centerX, fill, height, padding, px, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Encode
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { latexStr = ""
      , expr = Math.initExpr
      , derivative = Math.initExpr
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    Element.column
        [ width fill
        , height fill
        , Element.spacing 32
        , Element.paddingXY 50 70
        ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            ]
            [ Element.text "Derivative Calculator with Latex" ]
        , heading 1 "Derivative Input"
        , input model
        , heading 2 "Derivative"
        , derivative model
        ]
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


heading : Int -> String -> Element Msg
heading num str =
    Element.paragraph
        [ width fill
        , Font.bold
        , Font.size 30
        , Element.paddingXY 0 36
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


latex : Model -> Element Msg
latex model =
    Element.column
        [ centerX ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            , spacing 16
            , Element.padding 16
            ]
            [ Element.text "Latex representation" ]
        , Element.paragraph
            [ Font.center ]
            [ Element.text model.latexStr ]
        ]


latexToExpr : Model -> Element Msg
latexToExpr model =
    Element.column
        [ centerX ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            , spacing 16
            , Element.padding 16
            ]
            [ Element.text "Latex → Expr" ]
        , case model.expr of
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
            ]
        |> Element.el
            [ centerX
            , spacing 16
            , Element.padding 16
            , width fill
            ]


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
    = ChangedLatexStr String
    | Calculate
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedLatexStr latexStr ->
            ( { model
                | latexStr = latexStr
                , expr = Math.fromLatex latexStr
              }
            , Cmd.none
            )

        Calculate ->
            ( { model
                | derivative = Math.derivative model.expr
              }
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.changedLatex ChangedLatexStr

module Main exposing (main)

import Browser
import Element exposing (Element, centerX, fill, height, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Decode
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
      , expr = Err <| ParserError [ "No Expression provided" ]
      , derivative = Err <| ParserError [ "No Expression provided" ]
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
        ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            ]
            [ Element.text "Derivative?" ]
        , input model
        , latex model
        , latexToExpr model
        , derivative model
        ]
        |> Element.layout []


input : Model -> Element Msg
input model =
    Element.column
        [ Element.padding 24
        , spacing 32
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
            [ Element.text "Latex -> Expr" ]
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
    Element.column
        [ centerX
        , spacing 16
        , Border.width 1
        , Border.color <| Element.rgb 0 0 0
        , Element.padding 16
        , width fill
        ]
        [ Element.paragraph
            [ Font.size 32
            , Font.center
            ]
            [ Element.text "Derivative" ]
        , Element.column
            [ centerX
            , width fill
            ]
            [ Element.paragraph
                [ Font.center ]
                (model.derivative
                    |> (\r ->
                            case r of
                                Ok der ->
                                    Element.text <| Debug.toString der

                                Err DivisionByZero ->
                                    Element.text "Division by zero lol"

                                Err (ExtraVariable str) ->
                                    Element.text <| "Extra variable " ++ str

                                Err (ParserError errors) ->
                                    Element.text "Parser errors lol"

                                Err (DerivativeError error) ->
                                    Element.text <| "derivative error! " ++ error
                       )
                    |> List.singleton
                )
            , model.derivative
                |> Result.map (Math.asLatex >> (\s -> "\\frac{df}{dx}=" ++ s) >> staticMath)
                -- |> Result.map (Math.asLatex >> Element.text)
                |> Result.withDefault (Element.text "Errors lol")
                |> Element.el
                    [ centerX
                    , Element.scrollbarX
                    , width fill
                    ]
            ]
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
                | derivative = Debug.log "derivative calculated" (Math.derivative model.expr)
              }
            , Cmd.none
            )

        Clear ->
            ( { model
                | latexStr = ""
                , expr = Err <| ParserError [ "No Expression provided" ]
                , derivative = Err <| ParserError [ "No Expression provided" ]
              }
            , Ports.clear ()
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.changedLatex ChangedLatexStr

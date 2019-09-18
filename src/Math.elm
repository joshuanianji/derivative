module Math exposing (Expr(..), MathError(..), asLatex, derivative, errorToString, fromLatex, fullSimplify)

{- Module for parsing and dealing with the Math stuff
   Many thanks to http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
   And the Pratt Parser for existing, you've made my life so much easier
-}

import Parser exposing ((|.), (|=), Parser)
import Pratt
import Result
import Set



-- TYPES


type Expr
    = Const Float
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Mult Expr Expr
    | Pow Expr Expr
    | Cos Expr
    | Sin Expr
    | Ln Expr
    | Sqrt Expr


type MathError
    = DivisionByZero
    | ExtraVariable String
    | ParserError (List String)
    | DerivativeError String



-- HELPERS


negate : Expr -> Expr
negate expr =
    case expr of
        Const a ->
            Const -a

        Add a b ->
            Add (negate a) (negate b)

        Sub a b ->
            Sub (negate a) (negate b)

        Mult a b ->
            Mult (negate a) b

        Div a b ->
            Div (negate a) b

        f ->
            Mult (Const -1) f



-- DERIVATIVE


derivative : Result MathError Expr -> Result MathError Expr
derivative =
    fullSimplify
        >> Result.andThen derivativeIter
        >> fullSimplify


derivativeIter : Expr -> Result MathError Expr
derivativeIter expr =
    case expr of
        Const a ->
            Ok <| Const 0

        Var "x" ->
            Ok <| Const 1

        Var str ->
            Ok <| Const 0

        Add a b ->
            Result.map2 Add (derivativeIter a) (derivativeIter b)

        Sub a b ->
            Result.map2 Sub (derivativeIter a) (derivativeIter b)

        Mult a b ->
            Result.map2 Add (Result.map2 Mult (derivativeIter a) (Ok b)) (Result.map2 Mult (Ok a) (derivativeIter b))

        Div a b ->
            Result.map2 Div
                (Result.map2 Sub (Result.map2 Mult (derivativeIter a) (Ok b)) (Result.map2 Mult (Ok a) (derivativeIter b)))
                (Ok <| Mult b b)

        Pow (Var "x") (Const b) ->
            Result.map2 Mult (Ok <| Const b) (Ok <| Pow (Var "x") (Const (b - 1)))

        Pow (Var "e") f ->
            Result.map2 Mult (derivativeIter f) (Ok <| Pow (Var "e") f)

        -- (d/dx) f(x)^n = f'(x) * n * f(x) ^ (n-1)
        Pow a (Const n) ->
            Result.map2 Mult
                (derivativeIter a)
                (Result.map2 Mult (Ok <| Const n) (Ok <| Pow a (Const <| n - 1)))

        -- LOL I JUST USED THIS (https://mathvault.ca/exponent-rule-derivative/#Example_1_pix)
        Pow f g ->
            Result.map2 Mult
                (Ok <| Pow f g)
                (Result.map2 Add
                    (Result.map2 Mult (derivativeIter g) (Ok <| Ln f))
                    (Result.map2 Mult (derivativeIter f) (Ok <| Div g f))
                )

        -- Trig
        Sin f ->
            Result.map2 Mult (derivativeIter f) (Ok <| Cos f)

        Cos f ->
            Result.map2 Mult (derivativeIter f) (Ok <| Mult (Const -1) (Sin f))

        -- Logarithm
        Ln f ->
            Result.map2 Div (derivativeIter f) (Ok f)

        -- Square Root
        Sqrt f ->
            Result.map2 Mult
                (derivativeIter f)
                (Ok <| Mult (Div (Const 1) (Const 2)) (Pow f (negate <| Div (Const 1) (Const 2))))



-- _ ->
--     Err <| DerivativeError "Not implemented yet lol"
-- SIMPLIFY


fullSimplify : Result MathError Expr -> Result MathError Expr
fullSimplify expr =
    let
        fullSimplifyIter cur last =
            if cur == last then
                cur

            else
                let
                    curNext =
                        Result.andThen simplify cur
                in
                fullSimplifyIter curNext cur
    in
    fullSimplifyIter expr (Ok <| Const 0)


{-| Simplify functions based on cases

    bruh this code is loNGGGG because there are so many cases where we can simplify the expression by rip.
    Also its starting to look like lisp lol

-}
simplify : Expr -> Result MathError Expr
simplify expr1 =
    case expr1 of
        -- Addition and Subtraction Identities
        Add (Const a) (Const b) ->
            Ok <| Const (a + b)

        Add a (Const n) ->
            if n == 0 then
                simplify a

            else
                Result.map2 Add (simplify a) (Ok <| Const n)

        Add (Const n) a ->
            if n == 0 then
                simplify a

            else
                Result.map2 Add (Ok <| Const n) (simplify a)

        -- x + 3x or any other combination will add the x's together
        Add (Mult (Var a) (Const n)) (Var b) ->
            if a == b then
                Ok <| Mult (Var a) (Const (n + 1))

            else
                Ok <| Add (Mult (Var a) (Const n)) (Var b)

        Add (Mult (Const n) (Var a)) (Var b) ->
            if a == b then
                Ok <| Mult (Var a) (Const (n + 1))

            else
                Ok <| Add (Mult (Const n) (Var a)) (Var b)

        Add (Var b) (Mult (Const n) (Var a)) ->
            if a == b then
                Ok <| Mult (Const (n + 1)) (Var a)

            else
                Ok <| Add (Var b) (Mult (Const n) (Var a))

        Add (Var b) (Mult (Var a) (Const n)) ->
            if a == b then
                Ok <| Mult (Var a) (Const (n + 1))

            else
                Ok <| Add (Var b) (Mult (Var a) (Const n))

        Sub (Const a) (Const b) ->
            Ok <| Const (a - b)

        Sub a (Const n) ->
            if n == 0 then
                simplify a

            else
                Result.map2 Sub (simplify a) (Ok <| Const n)

        Sub (Const n) a ->
            if n == 0 then
                simplify <| negate a

            else
                Result.map2 Sub (Ok <| Const n) (simplify a)

        Sub (Var a) (Var b) ->
            if a == b then
                Ok <| Const 0

            else
                Ok <| Sub (Var a) (Var b)

        -- Multiplication
        Mult (Const a) (Mult (Const b) expr) ->
            Result.map2 Mult (Ok <| Const <| a * b) (simplify expr)

        Mult (Const a) (Mult expr (Const b)) ->
            Result.map2 Mult (Ok <| Const <| a * b) (simplify expr)

        Mult expr (Mult (Const a) (Const b)) ->
            Result.map2 Mult (Ok <| Const <| a * b) (simplify expr)

        Mult (Const a) (Add b c) ->
            Result.map2 Add (Result.map (Mult (Const a)) (simplify b)) (Result.map (Mult <| Const a) (simplify c))

        Mult (Div a b) c ->
            Result.map2 Div (Result.map2 Mult (simplify a) (simplify c)) (simplify b)

        Mult (Const a) (Const b) ->
            Ok <| Const (a * b)

        Mult a (Const n) ->
            if n == 1 then
                simplify a

            else if n == 0 then
                Ok <| Const 0

            else
                Result.map2 Mult (simplify a) (Ok <| Const n)

        Mult (Const n) a ->
            if n == 1 then
                simplify a

            else if n == 0 then
                Ok <| Debug.log "Zero!" Const 0

            else
                Result.map2 Mult (Ok <| Const n) (simplify a)

        -- Division Identities
        Div (Const a) (Const b) ->
            if b == 0 then
                Err DivisionByZero

            else if a == 0 then
                Ok <| Const 0

            else if a == b then
                Ok <| Const 1

            else
                Ok <| Div (Const a) (Const b)

        Div a (Const n) ->
            if n == 1 then
                simplify a

            else
                Result.map2 Div (simplify a) (Ok <| Const n)

        Div (Const n) a ->
            if n == 0 then
                Ok <| Const 0

            else
                Result.map2 Div (Ok <| Const n) (simplify a)

        --flipping
        Div (Div a b) c ->
            simplify <| Div a (Mult b c)

        Div a (Div b c) ->
            simplify <| Div (Mult a c) b

        -- removing common denominators (Maybe I should make a gcd function for the Expr type??)
        Div (Mult a b) (Mult c d) ->
            if a == c then
                simplify (Div b d)

            else if b == c then
                simplify (Div a d)

            else if a == d then
                simplify (Div b c)

            else if b == d then
                simplify (Div a c)

            else
                Result.map2 Div (simplify <| Mult a b) (simplify <| Mult c d)

        Div (Mult a b) c ->
            if c == a then
                simplify b

            else if b == c then
                simplify a

            else
                Result.map2 Div (simplify <| Mult a b) (simplify c)

        Div a (Mult b c) ->
            if a == b then
                simplify <| Div (Const 1) c

            else if a == c then
                simplify <| Div (Const 1) b

            else
                Result.map2 Div (simplify a) (simplify <| Mult b c)

        -- Exponential Identities
        Pow (Pow c (Const b)) (Const a) ->
            Result.map (Pow c) (Ok <| Const (a * b))

        Pow (Const a) (Const b) ->
            Ok <| Const (a ^ b)

        Pow a (Const n) ->
            if n == 1 then
                simplify a

            else if n == 0 then
                Ok <| Const 1

            else
                Result.map2 Pow (simplify a) (Ok <| Const n)

        -- Simplify arguments
        Add a b ->
            if a == b then
                Result.map2 Mult (simplify a) (Ok <| Const 2)

            else
                Result.map2 Add (simplify a) (simplify b)

        Sub a b ->
            if a == b then
                Ok <| Const 0

            else
                Result.map2 Sub (simplify a) (simplify b)

        Mult a b ->
            if a == b then
                Result.map2 Pow (simplify a) (Ok <| Const 2)

            else
                Result.map2 Mult (simplify a) (simplify b)

        Div a b ->
            if a == b then
                Ok <| Const 1

            else
                Result.map2 Div (simplify a) (simplify b)

        Pow a b ->
            Result.map2 Pow (simplify a) (simplify b)

        Sin a ->
            Result.map Sin (simplify a)

        Cos a ->
            Result.map Cos (simplify a)

        Ln (Var "e") ->
            Ok <| Const 1

        Ln a ->
            Result.map Ln (simplify a)

        x ->
            Ok <| identity x



-- DISPLAY - displays to latex


asLatex : Expr -> String
asLatex expr =
    case expr of
        Const a ->
            String.fromFloat a

        Var str ->
            str

        Add a b ->
            asLatex a ++ "+" ++ asLatex b

        Sub a b ->
            asLatex a ++ "-" ++ asLatex b

        Mult (Const a) (Const b) ->
            String.fromFloat a ++ "\\cdot" ++ String.fromFloat a

        Mult (Const a) b ->
            String.fromFloat a ++ asLatex b

        -- Mult (Add a b) (Sub b c) should have parentheses around them. This checks for all cases.
        Mult a b ->
            case ( shouldHaveParentheses (Mult a b) a, shouldHaveParentheses (Mult a b) b ) of
                ( True, True ) ->
                    "\\left(" ++ asLatex a ++ "\\right)" ++ "\\cdot" ++ "\\left(" ++ asLatex b ++ "\\right)"

                ( True, _ ) ->
                    "\\left(" ++ asLatex a ++ "\\right)" ++ "\\cdot" ++ asLatex b

                ( _, True ) ->
                    asLatex a ++ "\\cdot" ++ "\\left(" ++ asLatex b ++ "\\right)"

                _ ->
                    asLatex a ++ "\\cdot" ++ asLatex b

        Div a b ->
            "\\frac{" ++ asLatex a ++ "}{" ++ asLatex b ++ "}"

        -- only need to take care of the parentheses around the base.
        Pow a b ->
            case ( shouldHaveParentheses (Pow a b) a, shouldHaveBraces b ) of
                ( True, True ) ->
                    "\\left(" ++ asLatex a ++ "\\right)" ++ "^{" ++ asLatex b ++ "}"

                ( True, _ ) ->
                    "\\left(" ++ asLatex a ++ "\\right)" ++ "^" ++ asLatex b

                ( _, True ) ->
                    asLatex a ++ "^{" ++ asLatex b ++ "}"

                _ ->
                    asLatex a ++ "^" ++ asLatex b

        Sin a ->
            "\\sin\\left(" ++ asLatex a ++ "\\right)"

        Cos a ->
            "\\cos\\left(" ++ asLatex a ++ "\\right)"

        Ln a ->
            "\\ln\\left(" ++ asLatex a ++ "\\right)"

        Sqrt f ->
            "\\sqrt{" ++ asLatex f ++ "}"


{-| Parentheses checker

    e.g. `asLatex <| Mult (Add a b) (Sub c d)` should be `(a + b)(c - d)`

    This function helps to guarantee that nested binary operations lower in precedence than the parent
    will have parentheses around them

    Note that `asLatex <| Mult (Var "x") (Sub c d)` would output `x(c - d)`
    **Only binary operations are considered!**

-}
shouldHaveParentheses : Expr -> Expr -> Bool
shouldHaveParentheses parent child =
    let
        precedenceLevel expr =
            case expr of
                Add _ _ ->
                    Just 1

                Sub _ _ ->
                    Just 1

                Div _ _ ->
                    Just 2

                Mult _ _ ->
                    Just 2

                Pow _ _ ->
                    Just 3

                -- disregard unary operations
                _ ->
                    Nothing

        relativePrecedence =
            Maybe.map2 (-) (precedenceLevel parent) (precedenceLevel child)
    in
    case relativePrecedence of
        Just n ->
            if n > 0 then
                True

            else
                False

        _ ->
            False


{-| Braces Checker

    The power function pur braces when its argumen is too complex.
    e.g. x^3, x^{3x+6}
    This function simple checks if the argument needs a brace

-}
shouldHaveBraces : Expr -> Bool
shouldHaveBraces expr =
    case expr of
        Const _ ->
            False

        Var _ ->
            False

        _ ->
            True


errorToString : MathError -> String
errorToString err =
    case err of
        DivisionByZero ->
            "Division by zero!"

        ExtraVariable str ->
            "Extra variable! [" ++ str ++ "]"

        ParserError strs ->
            "Parser Error! " ++ List.foldl (++) "" strs

        DerivativeError str ->
            "Derivative Error! " ++ str



-- PARSER


fromLatex : String -> Result MathError Expr
fromLatex str =
    if str == "" then
        Err <| ParserError [ "No Expression provided" ]

    else
        String.trim str
            |> Parser.run parser
            |> Result.mapError toMathErrors



-- top leve parser - make sure it reaches the end of the string


parser : Parser Expr
parser =
    Parser.succeed identity
        |= expression
        |. Parser.end



-- order matters here, since the parser goes down the list! BEDMAS but going up (highest precedence at bottom)


expression : Parser Expr
expression =
    Pratt.expression
        { oneOf =
            [ negationCheck
            , Pratt.prefix 5 cosine Cos
            , Pratt.prefix 5 sine Sin
            , Pratt.prefix 5 natLog Ln
            , sqrt
            , division
            , parentheses
            , Pratt.literal variable
            , Pratt.literal constant

            -- sometimes we'll have x^{3x-1}, and this parses the braces.
            -- We'll have to hope that an exponent is the only real use case of this parser tho rip
            -- I'm pretty confident??
            , powArgument
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.symbol "+") Add
            , Pratt.infixLeft 1 (Parser.symbol "-") Sub
            , Pratt.infixLeft 2 (Parser.symbol "\\cdot") Mult
            , Pratt.infixRight 4 (Parser.symbol "^") Pow

            -- allows parsing of expressions like 3x
            , Pratt.infixLeft 2 (Parser.symbol "") Mult
            ]
        , spaces = Parser.succeed ()
        }



-- negation only goes to the next term


negationCheck : Pratt.Config Expr -> Parser Expr
negationCheck =
    Pratt.prefix 3 (Parser.symbol "-") negate


cosine : Parser ()
cosine =
    Parser.succeed ()
        |. Parser.keyword "\\cos"
        |. Parser.spaces


sine : Parser ()
sine =
    Parser.succeed ()
        |. Parser.keyword "\\sin"
        |. Parser.spaces


natLog : Parser ()
natLog =
    Parser.succeed ()
        |. Parser.keyword "\\ln"
        |. Parser.spaces



-- because squrt is not a simple prefix operation (it has braces) we need to use more stuff


sqrt : Pratt.Config Expr -> Parser Expr
sqrt config =
    Parser.succeed Sqrt
        |. Parser.symbol "\\sqrt"
        |. Parser.symbol "{"
        |= Pratt.subExpression 0 config
        |. Parser.symbol "}"


division : Pratt.Config Expr -> Parser Expr
division config =
    Parser.succeed Div
        |. Parser.keyword "\\frac"
        |. Parser.symbol "{"
        |= Pratt.subExpression 0 config
        |. Parser.symbol "}"
        |. Parser.symbol "{"
        |= Pratt.subExpression 0 config
        |. Parser.symbol "}"



-- we accept both () and [] parentheses


parentheses : Pratt.Config Expr -> Parser Expr
parentheses config =
    Parser.succeed identity
        |. Parser.keyword "\\left"
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol "("
                |= Pratt.subExpression 0 config
                |. Parser.keyword "\\right"
                |. Parser.symbol ")"
            , Parser.succeed identity
                |. Parser.symbol "["
                |= Pratt.subExpression 0 config
                |. Parser.keyword "\\right"
                |. Parser.symbol "]"
            ]


{-| Variables

    Any alpha character
    Any alpha character followed by an underscore and alphanumeric (e.g. a_1 or a_b)
    Any alpha character followed by an underscore with anything in {} (e.g. a_{thingy\pi 123}
    One of the greek letters (e.g. \theta, \phi, etc) that are not from a "list"

    ALSO HOLY COW THE PARSER ONEOF COMBINATIONS WORKS LOL I CANT BELIEVE IT

-}
variable : Parser Expr
variable =
    Parser.succeed Var
        |= Parser.oneOf
            [ Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf Char.isAlpha
                    |. Parser.oneOf
                        [ Parser.chompIf (\c -> c == '_')
                            |. Parser.oneOf
                                [ Parser.symbol "{"
                                    |. Parser.chompUntil "}"
                                , Parser.chompIf Char.isAlphaNum
                                ]
                        , Parser.succeed ()
                        ]
            , Parser.variable
                { start = \c -> c == '\\'
                , inner = Char.isAlpha
                , reserved = Set.fromList [ "\\cdot", "\\frac", "\\left", "\\right" ]
                }
            ]


constant : Parser Expr
constant =
    Parser.number
        { int = Just (toFloat >> Const)
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just Const
        }


powArgument : Pratt.Config Expr -> Parser Expr
powArgument config =
    Parser.succeed identity
        |. Parser.symbol "{"
        |= Pratt.subExpression 0 config
        |. Parser.symbol "}"


toMathErrors : List Parser.DeadEnd -> MathError
toMathErrors deadends =
    List.map
        (\deadend ->
            problemToString deadend.problem
                |> (++) " "
                |> ((++) <| " at row " ++ String.fromInt deadend.row)
                |> ((++) <| " at col " ++ String.fromInt deadend.col)
        )
        deadends
        |> ParserError


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting str ->
            "Expecting " ++ str

        Parser.ExpectingInt ->
            "ExpectingInt"

        Parser.ExpectingHex ->
            "ExpectingHex"

        Parser.ExpectingOctal ->
            "ExpectingOctal"

        Parser.ExpectingBinary ->
            "ExpectingBinary"

        Parser.ExpectingFloat ->
            "ExpectingFloat"

        Parser.ExpectingNumber ->
            "ExpectingNumber"

        Parser.ExpectingVariable ->
            "ExpectingVariable"

        Parser.ExpectingSymbol str ->
            if str == "-" then
                "Negation failed"

            else
                "ExpectingSymbol " ++ str

        Parser.ExpectingKeyword str ->
            -- if str == "\\frac" then
            --     "Division failed"
            -- else
            "ExpectingKeyword " ++ str

        Parser.ExpectingEnd ->
            "ExpectingEnd"

        Parser.UnexpectedChar ->
            "UnexpectedChar"

        Parser.Problem str ->
            "Problem " ++ str

        Parser.BadRepeat ->
            "BadRepeat"

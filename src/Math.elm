module Math exposing (Expr(..), MathError(..), asLatex, derivative, errorToString, fromLatex, fullSimplify, initExpr)

{- Module for parsing and dealing with the Math stuff
   Many thanks to http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
   And the Pratt Parser for existing, you've made my life so much easier
-}

import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Token(..))
import Pratt.Advanced as Pratt
import Result
import Set



----------------------------------
-- TYPES
----------------------------------


type
    Expr
    -- Trivial functions
    = Const Float
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Mult Expr Expr
    | Pow Expr Expr
      -- Trig
    | Sin Expr
    | Csc Expr
    | Cos Expr
    | Sec Expr
    | Tan Expr
    | Cot Expr
      -- other
    | Ln Expr
    | Sqrt Expr
    | Negative Expr


{-| Not ready yet - I need the absolute value funcion for the derivative of Arccsc and I don't want to do that yet lol.
-- Inverse trig
| Arcsin Expr
| Arccsc Expr
| Arccos Expr
| Arcsec Expr
| Arctan Expr
| Arccot Expr
-- Hyperbolic
| Sinh Expr
| Csch Expr
| Cosh Expr
| Sech Expr
| Tanh Expr
| Coth Expr
-}
type MathError
    = DivisionByZero
    | ExtraVariable String
    | ParserErrors (List String)
    | DerivativeError String



------------------------------------
-- HELPERS
------------------------------------


negate : Expr -> Expr
negate =
    Negative



-- DERIVATIVE


derivative : Result MathError Expr -> Result MathError Expr
derivative =
    fullSimplify
        >> Result.map derivativeIter
        >> fullSimplify


derivativeIter : Expr -> Expr
derivativeIter expr =
    case expr of
        Const _ ->
            Const 0

        Var "x" ->
            Const 1

        Var _ ->
            Const 0

        Add a b ->
            Add (derivativeIter a) (derivativeIter b)

        Sub a b ->
            Sub (derivativeIter a) (derivativeIter b)

        Mult a b ->
            Add (Mult (derivativeIter a) b) (Mult a (derivativeIter b))

        Div a b ->
            Div
                (Sub (Mult (derivativeIter a) b) (Mult a (derivativeIter b)))
                (Mult b b)

        Pow (Var "x") (Const b) ->
            Mult (Const b) (Pow (Var "x") (Const (b - 1)))

        Pow (Var "x") (Var a) ->
            case a of
                "x" ->
                    Mult
                        (Pow (Var "x") (Var "x"))
                        (Add (Ln (Var "x")) (Const 1))

                _ ->
                    Mult (Var a) (Pow (Var "x") (Sub (Var a) (Const 1)))

        Pow (Var "e") f ->
            Mult (derivativeIter f) (Pow (Var "e") f)

        -- (d/dx) f(x)^n = f'(x) * n * f(x) ^ (n-1)
        Pow a (Const n) ->
            Mult (derivativeIter a) (Mult (Const n) (Pow a (Const <| n - 1)))

        -- LOL I JUST USED THIS (https://mathvault.ca/exponent-rule-derivative/#Example_1_pix)
        Pow f g ->
            Mult
                (Pow f g)
                (Add
                    (Mult (derivativeIter g) (Ln f))
                    (Mult (derivativeIter f) (Div g f))
                )

        -- Trig - a lot of chain rules are used
        Sin f ->
            Mult (derivativeIter f) (Cos f)

        Csc f ->
            Mult (Csc f) (Cot f)
                |> Mult (derivativeIter f)
                |> negate

        Cos f ->
            Sin f
                |> Mult (derivativeIter f)
                |> negate

        Sec f ->
            Mult (Sec f) (Tan f)
                |> Mult (derivativeIter f)

        Tan f ->
            Mult (Sec f) (Sec f)
                |> Mult (derivativeIter f)

        Cot f ->
            Mult (Csc f) (Csc f)
                |> Mult (derivativeIter f)

        -- Logarithm
        Ln f ->
            Div (derivativeIter f) f

        -- Square Root
        Sqrt f ->
            Mult
                (derivativeIter f)
                (Mult (Div (Const 1) (Const 2)) (Pow f (negate <| Div (Const 1) (Const 2))))

        Negative a ->
            Mult (Const -1) (derivativeIter a)



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
        Const a ->
            if a < 0 then
                Negative (Const -a)
                    |> Ok

            else
                Ok <| Const a

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

            else if n == -1 then
                Result.map Negative <| simplify a

            else if n == 0 then
                Ok <| Const 0

            else
                Result.map2 Mult (Ok <| Const n) (simplify a)

        --flipping division stuff - putting it before identities so it is guaranteed to fun
        Div (Div a b) c ->
            simplify <| Div a (Mult b c)

        Div a (Div b c) ->
            simplify <| Div (Mult a c) b

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
        Pow a (Negative b) ->
            Result.map2 Div (Ok <| Const 1) (Ok <| Pow a b)

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

        Pow a (Div (Const b) (Const c)) ->
            if b == 1 && c == 2 then
                Result.map Sqrt (Ok a)

            else
                Result.map2 Pow (Ok a) (Result.map2 Div (Ok <| Const b) (Ok <| Const c))

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

        Csc a ->
            Result.map Csc (simplify a)

        Cos a ->
            Result.map Cos (simplify a)

        Sec a ->
            Result.map Sec (simplify a)

        Tan a ->
            Result.map Tan (simplify a)

        Cot a ->
            Result.map Cot (simplify a)

        Ln (Var "e") ->
            Ok <| Const 1

        Ln a ->
            Result.map Ln (simplify a)

        -- simplifying negatives
        Negative (Mult (Const a) b) ->
            if a == 1 then
                Result.map Negative <| simplify b

            else if a < 0 then
                Result.map2 Mult (Ok <| Const -a) (simplify b)

            else
                Result.map2 Mult (simplify b) (Ok <| Const a)
                    |> Result.map Negative

        Negative (Mult a (Const b)) ->
            if b == 1 then
                Result.map Negative <| simplify a

            else if b < 0 then
                Result.map2 Mult (simplify a) (Ok <| Const -b)

            else
                Result.map2 Mult (simplify a) (Ok <| Const b)
                    |> Result.map Negative

        Negative (Negative a) ->
            Ok a

        Negative a ->
            Result.map Negative <| simplify a

        x ->
            Ok <| identity x



------------------------------------
-- DISPLAY - displays to latex
------------------------------------


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
            String.fromFloat a ++ "\\cdot" ++ String.fromFloat b

        Mult (Const a) b ->
            if a == -1 then
                "-" ++ asLatex b
                -- if we need to write 3(x-2) or something

            else if shouldHaveParentheses (Mult (Const a) b) b then
                String.fromFloat a ++ "\\left(" ++ asLatex b ++ "\\right)"

            else
                String.fromFloat a ++ asLatex b

        -- Mult (Add a b) (Sub b c) should have parentheses around them. This checks for all cases.
        Mult a b ->
            case ( shouldHaveParentheses (Mult a b) a, shouldHaveParentheses (Mult a b) b ) of
                ( True, True ) ->
                    "\\left(" ++ asLatex a ++ "\\right)" ++ "\\cdot" ++ "\\left(" ++ asLatex b ++ "\\right)"

                ( True, _ ) ->
                    "\\left(" ++ asLatex a ++ "\\right)" ++ "\\cdot " ++ asLatex b

                ( _, True ) ->
                    asLatex a ++ "\\cdot" ++ "\\left(" ++ asLatex b ++ "\\right)"

                _ ->
                    asLatex a ++ "\\cdot " ++ asLatex b

        Div a b ->
            "\\frac{" ++ asLatex a ++ "}{" ++ asLatex b ++ "}"

        -- only need to take care of the parentheses around the base.
        Pow a b ->
            let
                displayb =
                    "^{" ++ asLatex b ++ "}"
            in
            if isTrig a then
                -- if it's a trig functino then we put the power in between the operator and the content
                trigPower a displayb

            else if shouldHaveParentheses (Pow a b) a then
                -- else we just do the regular stuff
                "\\left(" ++ asLatex a ++ "\\right)" ++ displayb

            else
                asLatex a ++ displayb

        Ln a ->
            "\\ln\\left(" ++ asLatex a ++ "\\right)"

        Sqrt f ->
            "\\sqrt{" ++ asLatex f ++ "}"

        Negative f ->
            "-" ++ asLatex f

        trig ->
            trigToString trig
                |> (\( operator, content ) -> operator ++ content)



-- helper functions for asLatex. Mainly trig stuff lol
-- isTrig is useful because power functions are represented differently when it's a trig function


isTrig : Expr -> Bool
isTrig a =
    case a of
        Sin _ ->
            True

        Csc _ ->
            True

        Cos _ ->
            True

        Sec _ ->
            True

        Tan _ ->
            True

        Cot _ ->
            True

        _ ->
            False



-- displays exponents when it's a trig function


trigPower : Expr -> String -> String
trigPower trig power =
    let
        ( operator, content ) =
            trigToString trig
    in
    operator ++ power ++ content


trigToString : Expr -> ( String, String )
trigToString trig =
    let
        latexA a =
            "\\left(" ++ asLatex a ++ "\\right)"
    in
    case trig of
        Sin a ->
            ( "\\sin", latexA a )

        Csc a ->
            ( "\\csc", latexA a )

        Cos a ->
            ( "\\cos", latexA a )

        Sec a ->
            ( "\\sec", latexA a )

        Tan a ->
            ( "\\tan", latexA a )

        Cot a ->
            ( "\\cot", latexA a )

        _ ->
            ( "", "" )


{-| Parentheses checker

    e.g. `asLatex <| Mult (Add a b) (Sub c d)` should be `(a + b)(c - d)`

    This function helps to guarantee that nested binary operations lower in precedence than the parent
    will have parentheses around them

    Note that `asLatex <| Mult (Var "x") (Sub c d)` would output `x(c - d)`
    **Only binary operations are considered! (except the Negative of course)**

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

                Negative a ->
                    precedenceLevel a

                -- disregard unary operations
                _ ->
                    Nothing

        relativePrecedence =
            Maybe.map2 (-) (precedenceLevel parent) (precedenceLevel child)
    in
    case relativePrecedence of
        Just n ->
            n > 0

        _ ->
            False


errorToString : MathError -> String
errorToString err =
    case err of
        DivisionByZero ->
            "Division by zero!"

        ExtraVariable str ->
            "Extra variable! [" ++ str ++ "]"

        ParserErrors errors ->
            {--}
            -- especially in the oneOf parsers, we will get a bunch of errors stating that each one of the parsers failed.
            -- we only need to display the last one
            case lastElem errors of
                Just e ->
                    "Parser Error: " ++ e

                Nothing ->
                    "Unknown Parser error!"

        --}
        {--
            -- This is for showing ALL the errors
            List.foldl (\error acc -> acc ++ ", " ++ error) "[" errors
                -- |> (\s -> s ++ "]")
            --}
        DerivativeError str ->
            "Derivative Error! " ++ str



-- used for ParserErrors
-- this function is taken from [this reddit post](https://www.reddit.com/r/elm/comments/4j2fg6/finding_the_last_list_element/d33g6ae/)


lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing



------------------------------------
-- PARSER
------------------------------------


{-| Advanced type aliases so we don't have to write out as much
-}
type alias MyParser a =
    Parser Context Problem a


type Problem
    = UnknownOperator
    | BadVariable VariableProblem -- for variable parser
    | BadNumber
    | BadEnding -- when Parser.end fails
    | ExpectingExpression String -- when we're parsing symbols we need a problem type
    | ExpectingOperation String
    | ExpectingSymbol String
    | ExpectingNoSpace -- for multiplication


type VariableProblem
    = ExpectingAlpha
    | Underscore
    | LeftBracket
    | RightBracket
    | ExpectedAlphaNum


type Context
    = Expression
    | BackslashVariable
    | RegularVariable
    | Parentheses


type alias DeadEnd =
    Parser.DeadEnd Context Problem


{-| Advanced Pratt Parser type alias
-}
type alias PrattConfig a =
    Pratt.Config Context Problem a



-- actually running the parser on a latex string


fromLatex : String -> Result MathError Expr
fromLatex str =
    if str == "" then
        initExpr

    else
        String.trim str
            |> Parser.run parser
            |> Result.mapError toMathErrors


initExpr : Result MathError Expr
initExpr =
    Err <| ParserErrors [ "Waiting for expression" ]



-- top level parser - make sure it reaches the end of the string


parser : MyParser Expr
parser =
    Parser.succeed identity
        |= expression
        |. Parser.end BadEnding



-- Thanks to the Pratt Parser for existing!


expression : MyParser Expr
expression =
    Parser.inContext Expression <|
        Pratt.expression
            { oneOf =
                [ negationCheck
                , Pratt.prefix 2 cosine Cos
                , Pratt.prefix 2 sine Sin
                , Pratt.prefix 2 cosecant Csc
                , Pratt.prefix 2 secant Sec
                , Pratt.prefix 2 tangent Tan
                , Pratt.prefix 2 cotangent Cot
                , Pratt.prefix 2 natLog Ln
                , sqrt
                , division
                , parentheses
                , Pratt.literal variable
                , Pratt.literal constant

                -- sometimes we'll have x^{3x-1}, and this parses the braces.
                -- We'll have to hope that an exponent is the only real use case of this parser tho rip
                -- I'm pretty confident??
                , powArgument
                , \_ -> Parser.problem UnknownOperator
                ]
            , andThenOneOf =
                [ Pratt.infixLeft 1 (Parser.symbol (Token "+" (ExpectingOperation "+"))) Add
                , Pratt.infixLeft 1 (Parser.symbol (Token "-" (ExpectingOperation "-"))) Sub
                , Pratt.infixLeft 3 (Parser.symbol (Token "\\cdot" (ExpectingOperation "\\cdot"))) Mult
                , Pratt.infixRight 5 (Parser.symbol (Token "^" (ExpectingOperation "^"))) Pow

                -- allows parsing of expressions like 3x
                , Pratt.infixLeft 3 (Parser.symbol (Token "" ExpectingNoSpace)) Mult
                ]
            , spaces = Parser.spaces
            }



-- negation only goes to the next term


negationCheck : PrattConfig Expr -> MyParser Expr
negationCheck =
    let
        negationToken =
            Token "-" (ExpectingOperation "-")
    in
    Pratt.prefix 3 (Parser.symbol negationToken) negate


{-| Basic trigonometry chompers
-}
sine : MyParser ()
sine =
    unary "\\sin"


cosine : MyParser ()
cosine =
    unary "\\cos"


cosecant : MyParser ()
cosecant =
    unary "\\csc"


secant : MyParser ()
secant =
    unary "\\sec"


tangent : MyParser ()
tangent =
    unary "\\tan"


cotangent : MyParser ()
cotangent =
    unary "\\cot"


natLog : MyParser ()
natLog =
    unary "\\ln"



-- helper function for unary operations


unary : String -> MyParser ()
unary keyword =
    Parser.succeed ()
        |. Parser.keyword (Token keyword (ExpectingExpression keyword))
        |. Parser.spaces



-- because sqrt is not a simple prefix operation (it has braces) we need to use more stuff


sqrt : PrattConfig Expr -> MyParser Expr
sqrt config =
    Parser.succeed Sqrt
        |. Parser.symbol (Token "\\sqrt" (ExpectingExpression "\\sqrt"))
        |. Parser.symbol (Token "{" (ExpectingSymbol "{"))
        |= Pratt.subExpression 0 config
        |. Parser.symbol (Token "}" (ExpectingSymbol "}"))


division : PrattConfig Expr -> MyParser Expr
division config =
    Parser.succeed Div
        |. Parser.keyword (Token "\\frac" (ExpectingExpression "\\frac"))
        |. Parser.symbol (Token "{" (ExpectingSymbol "{"))
        |= Pratt.subExpression 0 config
        |. Parser.symbol (Token "}" (ExpectingSymbol "}"))
        |. Parser.symbol (Token "{" (ExpectingSymbol "{"))
        |= Pratt.subExpression 0 config
        |. Parser.symbol (Token "}" (ExpectingSymbol "}"))



-- we accept both () and [] parentheses


parentheses : PrattConfig Expr -> MyParser Expr
parentheses config =
    Parser.inContext Parentheses <|
        Parser.succeed identity
            |. Parser.keyword (Token "\\left" (ExpectingExpression "\\left"))
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol (Token "(" (ExpectingSymbol "("))
                    |= Pratt.subExpression 0 config
                    |. Parser.keyword (Token "\\right" (ExpectingExpression "\\right"))
                    |. Parser.symbol (Token ")" (ExpectingSymbol ")"))
                , Parser.succeed identity
                    |. Parser.symbol (Token "[" (ExpectingSymbol "["))
                    |= Pratt.subExpression 0 config
                    |. Parser.keyword (Token "\\right" (ExpectingExpression "\\right"))
                    |. Parser.symbol (Token "]" (ExpectingSymbol "]"))

                -- , Parser.problem "I am expecting a pair of parentheses but that failed - I can only handle () and []! If you are trying to type in absolute value, I don't support that"
                ]


{-| Variables

    Any alpha character
    Any alpha character followed by an underscore and alphanumeric (e.g. a_1 or a_b)
    Any alpha character followed by an underscore with anything in {} (e.g. a_{thingy\pi 123}
    One of the greek letters (e.g. \theta, \phi, etc) that are not from a "list" of otherwise defined tokens

    ALSO HOLY COW THE PARSER ONEOF COMBINATIONS WORKS LOL I CANT BELIEVE IT

-}
variable : MyParser Expr
variable =
    Parser.succeed Var
        |= Parser.oneOf
            [ Parser.succeed ()
                |. Parser.chompIf Char.isAlpha (BadVariable ExpectingAlpha)
                |. Parser.oneOf
                    [ Parser.chompIf (\c -> c == '_') (BadVariable Underscore)
                        |. Parser.oneOf
                            [ Parser.chompIf (\c -> c == '{') (BadVariable LeftBracket)
                                |. Parser.chompWhile (\c -> not (c == '}'))
                                |. Parser.chompIf (\c -> c == '}') (BadVariable RightBracket)
                            , Parser.chompIf Char.isAlphaNum (BadVariable ExpectedAlphaNum)
                            ]
                    , Parser.succeed ()
                    ]
                |> Parser.getChompedString
                |> Parser.inContext RegularVariable
            , Parser.variable
                { start = \c -> c == '\\'
                , inner = Char.isAlpha
                , reserved = Set.fromList [ "\\cdot", "\\frac", "\\left", "\\right" ]
                , expecting = BadVariable ExpectingAlpha
                }
                |> Parser.inContext BackslashVariable
            ]



-- Custom parser to only parser numbers!!
-- the elm Parser.number also parses "e" as the exponent which messes things suck as 3e^{5x} up a lot, and it's annoying/
-- Good thing we have Functional Programming, and good thing the Elm Parser is so flexible!


constant : MyParser Expr
constant =
    Parser.getChompedString (Parser.chompWhile (\c -> Char.isDigit c || c == '.'))
        |> Parser.map String.toFloat
        |> Parser.andThen
            (\num ->
                case num of
                    Just n ->
                        Parser.succeed n

                    Nothing ->
                        Parser.problem BadNumber
            )
        |> Parser.map Const


powArgument : PrattConfig Expr -> MyParser Expr
powArgument config =
    Parser.succeed identity
        |. Parser.symbol (Token "{" (ExpectingSymbol "{"))
        |= Pratt.subExpression 0 config
        |. Parser.symbol (Token "}" (ExpectingSymbol "}"))


toMathErrors : List DeadEnd -> MathError
toMathErrors deadends =
    List.map deadendToString deadends
        |> ParserErrors


deadendToString : DeadEnd -> String
deadendToString deadend =
    case deadend.problem of
        UnknownOperator ->
            "There is either an unknown operator, we're waiting for you to fill out an argument, or it's a mistake in the code."

        BadVariable issue ->
            case issue of
                ExpectingAlpha ->
                    if List.member BackslashVariable (List.map .context deadend.contextStack) then
                        "I was trying to parse a backslash variable, but either I didn't see a backslash or there were non-alpha characters in the name" ++ withLocation deadend

                    else
                        "I need variables start off with only alpha characters!" ++ withLocation deadend

                Underscore ->
                    "Expecting something in variable subscript" ++ withLocation deadend

                LeftBracket ->
                    "Uh oh, we're expecting a left bracket when parsing a variable, but this isn't your fault. Please click \" More Info\" to learn more" ++ withLocation deadend

                RightBracket ->
                    "Uh oh, we're expecting a right bracket when parsing a variable, but this isn't your fault. Please click \" More Info\" to learn more" ++ withLocation deadend

                ExpectedAlphaNum ->
                    "Variable subscripts can only contain alphanumeric numbers" ++ withLocation deadend

        BadNumber ->
            "Error parsing a number" ++ withLocation deadend

        BadEnding ->
            "Couldn't finish parsing the expression completely!" ++ withLocation deadend

        ExpectingExpression str ->
            "Expecting expression: " ++ str ++ withLocation deadend

        ExpectingOperation str ->
            "Expecting operation: " ++ str ++ withLocation deadend

        ExpectingSymbol str ->
            "Expecting symbol: " ++ str ++ withLocation deadend

        ExpectingNoSpace ->
            "Expecting no space for multiplication" ++ withLocation deadend


withLocation : DeadEnd -> String
withLocation deadend =
    " (column: " ++ String.fromInt deadend.col ++ ")"

port module Ports exposing (changedLatex, clear)

-- javascript sends us a changedLatex message when the mathquill API detects an edit in the InputMath


port changedLatex : (String -> msg) -> Sub msg



-- when the clear button is set we send a message to javascript so they can use the mathquill API to clear the input


port clear : () -> Cmd msg

port module Ports exposing (changedLatex)


port changedLatex : (String -> msg) -> Sub msg

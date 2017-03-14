module QRCode.Encode.Numeric exposing
    ( regex
    , encode
    )


import Regex exposing (Regex)
import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (listResult, breakStr)



regex : Regex
regex =
    Regex.regex "^[0-9]+$"


encode : String -> Result Error (List ( Int, Int ))
encode str =
    breakStr 3 str
        |> listResult encodeHelp []


encodeHelp : String -> Result Error ( Int, Int )
encodeHelp str =
    String.toInt str
        |> Result.map (flip (,) (numericLength str))
        |> Result.mapError (always InvalidNumericChar)


numericLength : String -> Int
numericLength str =
    case String.length str of
        1 -> 4
        2 -> 7
        _ -> 10

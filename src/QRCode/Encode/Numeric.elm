module QRCode.Encode.Numeric exposing
    ( encode
    , isValid
    )

import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (breakStr, listResult)
import Regex exposing (Regex)


isValid : String -> Bool
isValid input =
    Maybe.map (\r -> Regex.contains r input) onlyNumber
        |> Maybe.withDefault False



-- 0, 1, 2, 3, 4, 5, 6, 7, 8, 9


onlyNumber : Maybe Regex
onlyNumber =
    Regex.fromStringWith
        { caseInsensitive = False, multiline = False }
        "^[0-9]+$"


encode : String -> Result Error (List ( Int, Int ))
encode str =
    breakStr 3 str
        |> listResult encodeHelp []


encodeHelp : String -> Result Error ( Int, Int )
encodeHelp str =
    String.toInt str
        |> Maybe.map (\a -> ( a, numericLength str ))
        |> Result.fromMaybe InvalidNumericChar


numericLength : String -> Int
numericLength str =
    case String.length str of
        1 ->
            4

        2 ->
            7

        _ ->
            10

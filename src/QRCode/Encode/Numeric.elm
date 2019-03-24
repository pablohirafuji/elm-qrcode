module QRCode.Encode.Numeric exposing
    ( encode
    , isValid
    )

import List.Extra as List
import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (breakStr, listResult)
import Regex exposing (Regex)


isValid : String -> Bool
isValid input =
    Maybe.withDefault False
        (Maybe.map (\r -> Regex.contains r input) onlyNumber)



-- 0, 1, 2, 3, 4, 5, 6, 7, 8, 9


onlyNumber : Maybe Regex
onlyNumber =
    Regex.fromStringWith
        { caseInsensitive = False, multiline = False }
        "^[0-9]+$"


encode : String -> Result Error (List ( Int, Int ))
encode str =
    List.foldr (Result.map2 (::))
        (Ok [])
        (List.map encodeHelp
            (List.greedyGroupsOf 3 (String.toList str))
        )


encodeHelp : List Char -> Result Error ( Int, Int )
encodeHelp chars =
    let
        str =
            String.fromList chars
    in
    Result.fromMaybe InvalidNumericChar
        (Maybe.map (\a -> ( a, numericLength str ))
            (String.toInt str)
        )


numericLength : String -> Int
numericLength str =
    case String.length str of
        1 ->
            4

        2 ->
            7

        _ ->
            10

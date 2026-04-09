module QRCode.Encode.Numeric exposing
    ( encode
    , isValid
    )

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
            (greedyGroupsOf3 (String.toList str))
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


greedyGroupsOf3 : List a -> List (List a)
greedyGroupsOf3 list =
    greedyGroupsOf3Help [] list


greedyGroupsOf3Help : List (List a) -> List a -> List (List a)
greedyGroupsOf3Help acc list =
    case list of
        [] ->
            List.reverse acc

        v1 :: v2 :: v3 :: rest ->
            greedyGroupsOf3Help ([ v1, v2, v3 ] :: acc) rest

        _ ->
            List.reverse (list :: acc)

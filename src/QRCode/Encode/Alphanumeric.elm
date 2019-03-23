module QRCode.Encode.Alphanumeric exposing
    ( encode
    , isValid
    )

import Dict exposing (Dict)
import List.Extra as List
import QRCode.Error exposing (Error(..))
import Regex exposing (Regex)


isValid : String -> Bool
isValid input =
    Maybe.withDefault False
        (Maybe.map (\r -> Regex.contains r input) onlyAlphanumeric)



-- 0–9, A–Z [upper-case only], space, $, %, *, +, -, ., /, :


onlyAlphanumeric : Maybe Regex
onlyAlphanumeric =
    Regex.fromStringWith
        { caseInsensitive = False, multiline = False }
        "^[0-9A-Z $%*+\\-.\\/:]+$"


encode : String -> Result Error (List ( Int, Int ))
encode str =
    List.foldl (Result.map2 (::))
        (Ok [])
        (List.map toBinary
            (List.greedyGroupsOf 2 (String.toList str))
        )


toBinary : List Char -> Result Error ( Int, Int )
toBinary chars =
    case chars of
        firstChar :: secondChar :: [] ->
            Result.map2
                (\firstCode secondCode ->
                    ( (firstCode * 45) + secondCode, 11 )
                )
                (toAlphanumericCode firstChar)
                (toAlphanumericCode secondChar)

        char :: [] ->
            Result.map (\a -> ( a, 6 ))
                (toAlphanumericCode char)

        _ ->
            Result.Err InvalidAlphanumericChar


toAlphanumericCode : Char -> Result Error Int
toAlphanumericCode char =
    Result.fromMaybe InvalidAlphanumericChar
        (Dict.get char alphanumericCodes)


alphanumericCodes : Dict Char Int
alphanumericCodes =
    Dict.fromList
        [ ( '0', 0 )
        , ( '1', 1 )
        , ( '2', 2 )
        , ( '3', 3 )
        , ( '4', 4 )
        , ( '5', 5 )
        , ( '6', 6 )
        , ( '7', 7 )
        , ( '8', 8 )
        , ( '9', 9 )
        , ( 'A', 10 )
        , ( 'B', 11 )
        , ( 'C', 12 )
        , ( 'D', 13 )
        , ( 'E', 14 )
        , ( 'F', 15 )
        , ( 'G', 16 )
        , ( 'H', 17 )
        , ( 'I', 18 )
        , ( 'J', 19 )
        , ( 'K', 20 )
        , ( 'L', 21 )
        , ( 'M', 22 )
        , ( 'N', 23 )
        , ( 'O', 24 )
        , ( 'P', 25 )
        , ( 'Q', 26 )
        , ( 'R', 27 )
        , ( 'S', 28 )
        , ( 'T', 29 )
        , ( 'U', 30 )
        , ( 'V', 31 )
        , ( 'W', 32 )
        , ( 'X', 33 )
        , ( 'Y', 34 )
        , ( 'Z', 35 )
        , ( ' ', 36 )
        , ( '$', 37 )
        , ( '%', 38 )
        , ( '*', 39 )
        , ( '+', 40 )
        , ( '-', 41 )
        , ( '.', 42 )
        , ( '/', 43 )
        , ( ':', 44 )
        ]

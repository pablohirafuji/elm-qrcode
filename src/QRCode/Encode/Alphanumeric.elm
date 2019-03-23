module QRCode.Encode.Alphanumeric exposing
    ( encode
    , isValid
    )

import Dict exposing (Dict)
import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (breakStr, listResult)
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
    listResult toBinary [] (breakStr 2 str)


toBinary : String -> Result Error ( Int, Int )
toBinary str =
    case String.toList str of
        firstChar :: secondChar :: [] ->
            Result.map (\a -> ( a, 11 ))
                (Result.map
                    (\( first, second ) ->
                        (first * 45) + second
                    )
                    (Result.andThen
                        (\firstCode ->
                            Result.map (\b -> ( firstCode, b ))
                                (toAlphanumericCode secondChar)
                        )
                        (toAlphanumericCode firstChar)
                    )
                )

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

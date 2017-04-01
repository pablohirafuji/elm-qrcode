module QRCode.Encode.Alphanumeric exposing
    ( isValid
    , encode
    )


import Dict exposing (Dict)
import Regex exposing (Regex)
import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (listResult, breakStr)



isValid : String -> Bool
isValid input =
    Regex.contains isValidRegex input


isValidRegex : Regex
isValidRegex =
    Regex.regex "^[0-9A-Z $%*+\\-.\\/:]+$"


encode : String -> Result Error (List ( Int, Int ))
encode str =
    breakStr 2 str
        |> listResult toBinary []


toBinary : String -> Result Error ( Int, Int )
toBinary str =
    case String.toList str of
        firstChar :: secondChar :: [] ->
            toAlphanumericCode firstChar
                |> Result.andThen (\firstCode ->
                        toAlphanumericCode secondChar
                            |> Result.map ((,) firstCode))
                |> Result.map (\(first, second) ->
                        (first * 45) + second)
                |> Result.map (flip (,) 11)


        char :: [] ->
            toAlphanumericCode char
                |> Result.map (flip (,) 6)

        _ ->
            Result.Err InvalidAlphanumericChar


toAlphanumericCode : Char -> Result Error Int
toAlphanumericCode char =
    Dict.get char alphanumericCodes
        |> Result.fromMaybe InvalidAlphanumericChar


alphanumericCodes : Dict Char Int
alphanumericCodes =
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
    ] |> Dict.fromList

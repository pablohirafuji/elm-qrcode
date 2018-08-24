module QRCode.Encode.Byte exposing
    ( encode
    , isValid
    )

import Char
import QRCode.Error exposing (Error(..))
import Regex exposing (Regex)


isValid : String -> Bool
isValid input =
    Maybe.map (\r -> Regex.contains r input) only8Bit
        |> Maybe.withDefault False



-- 8-bit bytes, from 0 to 255 char code.


only8Bit : Maybe Regex
only8Bit =
    Regex.fromStringWith
        { caseInsensitive = False, multiline = False }
        "^[\\u0000-\\u00ff]+$"


encode : String -> Result Error (List ( Int, Int ))
encode str =
    encodeHelp str []
        |> Result.map (List.map (\a -> ( a, 8 )))


encodeHelp : String -> List Int -> Result Error (List Int)
encodeHelp str bytes =
    case String.uncons str of
        Just ( char, strTail ) ->
            Char.toCode char
                |> (\a -> (::) a bytes)
                |> encodeHelp strTail

        Nothing ->
            List.reverse bytes
                |> Result.Ok

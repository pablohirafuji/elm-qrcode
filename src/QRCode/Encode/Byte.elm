module QRCode.Encode.Byte exposing
    ( encode
    , isValid
    )

import Char
import QRCode.Error exposing (Error(..))
import Regex exposing (Regex)


isValid : String -> Bool
isValid input =
    Maybe.withDefault False
        (Maybe.map (\r -> Regex.contains r input) only8Bit)



-- 8-bit bytes, from 0 to 255 char code.


only8Bit : Maybe Regex
only8Bit =
    Regex.fromStringWith
        { caseInsensitive = False, multiline = False }
        "^[\\u0000-\\u00ff]+$"


encode : String -> Result Error (List ( Int, Int ))
encode str =
    Result.map (List.map (\a -> ( a, 8 )))
        (encodeHelp str [])


encodeHelp : String -> List Int -> Result Error (List Int)
encodeHelp str bytes =
    case String.uncons str of
        Just ( char, strTail ) ->
            encodeHelp strTail (Char.toCode char :: bytes)

        Nothing ->
            Result.Ok (List.reverse bytes)

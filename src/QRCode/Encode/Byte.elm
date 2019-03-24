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
    Ok (List.map (\a -> ( Char.toCode a, 8 )) (String.toList str))

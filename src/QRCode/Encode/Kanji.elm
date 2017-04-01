module QRCode.Encode.Kanji exposing
    ( isValid
    , encode
    )


import Char
import Regex exposing (Regex)
import Bitwise as Bit exposing (shiftRightBy)
import QRCode.Error exposing (Error(..))



isValid : String -> Bool
isValid input =
    Regex.contains regex input


-- Shift JIS Kanji characters
-- Annex H - ISO/IEC18004:2006(E)
regex : Regex
regex =
    Regex.regex ""


encode : String -> Result Error (List ( Int, Int ))
encode str =
    encodeHelp str []
        |> Result.map (List.map (flip (,) 13))


encodeHelp : String -> List Int -> Result Error (List Int)
encodeHelp str bits =
    case String.uncons str of
        Just ( char, strTail ) ->
            Char.toCode char
                |> convertToShiftJIS
                |> prepareCode
                |> finishCode
                |> flip (::) bits
                |> encodeHelp strTail

        Nothing ->
            List.reverse bits
                |> Result.Ok


-- How to convert unicode to Shift JIS?
-- Is this table correct?
-- ftp://ftp.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/SHIFTJIS.TXT
convertToShiftJIS : Int -> Int
convertToShiftJIS unicode =
    unicode -- TODO


prepareCode : Int -> Int
prepareCode shiftJIS =
    if 33088 <= shiftJIS && shiftJIS <= 40956 then
        shiftJIS - 33088

    else --if 57408 <= shiftJIS && shiftJIS <= 60351 then
        shiftJIS - 49472

    -- If out of range, return Error.InvalidKanji


finishCode : Int -> Int
finishCode code =
    let
        mostSigByte = shiftRightBy 8 code
        leastSigByte = Bit.and 255 code

    in
        (mostSigByte * 192) + leastSigByte



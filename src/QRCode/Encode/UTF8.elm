module QRCode.Encode.UTF8 exposing (encode)

import Bitwise as Bit exposing (shiftLeftBy, shiftRightBy)
import Char
import QRCode.Error exposing (Error(..))


encode : String -> Result Error (List ( Int, Int ))
encode str =
    Result.map (List.map (\a -> ( a, 8 )))
        (encodeHelp (String.toList str) [])


encodeHelp : List Char -> List Int -> Result Error (List Int)
encodeHelp chars list =
    case chars of
        char :: charsTail ->
            utf8ToByte list charsTail (Char.toCode char)

        [] ->
            Result.Ok (List.reverse list)



-- From: http://stackoverflow.com/questions/18729405/how-to-convert-utf8-string-to-byte-array


utf8ToByte : List Int -> List Char -> Int -> Result Error (List Int)
utf8ToByte list remainStr charCode =
    if charCode < 128 then
        encodeHelp remainStr (charCode :: list)

    else if charCode < 2048 then
        encodeHelp remainStr
            (Bit.or 128 (and63 charCode)
                :: Bit.or 192 (shiftRightBy 6 charCode)
                :: list
            )

    else if charCode < 55296 || charCode >= 57344 then
        encodeHelp remainStr
            (Bit.or 128 (and63 charCode)
                :: Bit.or 128 (and63 (shiftRightBy 6 charCode))
                :: Bit.or 224 (shiftRightBy 12 charCode)
                :: list
            )

    else
        case remainStr of
            char :: strTail ->
                let
                    nextCharCode =
                        Char.toCode char

                    charC =
                        65536
                            + Bit.or (Bit.and 1023 nextCharCode)
                                (Bit.shiftLeftBy 10
                                    (Bit.and 1023 charCode)
                                )

                    byte1 =
                        Bit.or 240 (shiftRightBy 18 charC)

                    byte2 =
                        Bit.or 128 (and63 (shiftRightBy 12 charC))

                    byte3 =
                        Bit.or 128 (and63 (shiftRightBy 6 charC))

                    byte4 =
                        Bit.or 128 (and63 charC)
                in
                encodeHelp strTail
                    (byte4
                        :: byte3
                        :: byte2
                        :: byte1
                        :: list
                    )

            [] ->
                Result.Err InvalidUTF8Char



-- 63 == 0x3f == 111111


and63 : Int -> Int
and63 =
    Bit.and 63

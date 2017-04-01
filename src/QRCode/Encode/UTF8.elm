module QRCode.Encode.UTF8 exposing (encode)


import Char
import Bitwise as Bit exposing (shiftLeftBy, shiftRightBy)
import QRCode.Error exposing (Error(..))



encode : String -> Result Error (List ( Int, Int ))
encode str =
    encodeHelp str []
        |> Result.map (List.map (flip (,) 8))


encodeHelp : String -> List Int -> Result Error (List Int)
encodeHelp str list =
    case String.uncons str of
        Just ( char, strTail ) ->
            Char.toCode char
                |> utf8ToByte list strTail

        Nothing ->
            List.reverse list
                |> Result.Ok


-- http://stackoverflow.com/questions/18729405/how-to-convert-utf8-string-to-byte-array

utf8ToByte : List Int -> String -> Int -> Result Error (List Int)
utf8ToByte list remainStr charCode =
    if charCode < 128 then
        charCode :: list
            |> encodeHelp remainStr

    else if charCode < 2048 then
        list
            |> (::) (Bit.or 192 (shiftRightBy 6 charCode))
            |> (::) (Bit.or 128 (and63 charCode))
            |> encodeHelp remainStr

    else if charCode < 55296 || charCode >= 57344 then
        list
            |> (::) (Bit.or 224 (shiftRightBy 12 charCode))
            |> (::) (Bit.or 128 (and63 (shiftRightBy 6 charCode)))
            |> (::) (Bit.or 128 (and63 charCode))
            |> encodeHelp remainStr

    else case String.uncons remainStr of
        Just ( char, strTail ) ->
            let
                nextCharCode = Char.toCode char

                charC =
                    Bit.and 1023 charCode
                        |> Bit.shiftLeftBy 10
                        |> Bit.or (Bit.and 1023 nextCharCode)
                        |> (+) 65536


                byte1 = Bit.or 240 (shiftRightBy 18 charC)
                byte2 = Bit.or 128 (and63 (shiftRightBy 12 charC))
                byte3 = Bit.or 128 (and63 (shiftRightBy 6 charC))
                byte4 = Bit.or 128 (and63 charC)


            in
                byte4 :: byte3 :: byte2 :: byte1 :: list
                    |> encodeHelp strTail

        Nothing ->
            Result.Err InvalidUTF8Char


-- 63 == 0x3f == 111111
and63 : Int -> Int
and63 = Bit.and 63

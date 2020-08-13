module QRCode.Encode.UTF8 exposing (encode)

import Bitwise as Bit exposing (shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes)
import Bytes.Decode as BytesD
import Bytes.Encode as BytesE
import Char
import QRCode.Error exposing (Error(..))


encode : String -> Result Error (List ( Int, Int ))
encode str =
    let
        utf8BytesWidth =
            BytesE.getStringWidth str

        decoder =
            BytesD.loop ( utf8BytesWidth, [] ) step
    in
    BytesE.string str
        |> BytesE.encode
        |> BytesD.decode decoder
        |> Result.fromMaybe InvalidUTF8Char


step : ( Int, List ( Int, Int ) ) -> BytesD.Decoder (BytesD.Step ( Int, List ( Int, Int ) ) (List ( Int, Int )))
step ( n, xs ) =
    if n <= 0 then
        BytesD.succeed (BytesD.Done (List.reverse xs))

    else
        BytesD.map (\x -> BytesD.Loop ( n - 1, ( x, 8 ) :: xs )) BytesD.unsignedInt8

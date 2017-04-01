module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import QRCode.Encode as Encode
import QRCode.Encode.Numeric as Numeric
import QRCode.Encode.Alphanumeric as Alphanumeric
import QRCode.ECLevel as ECLevel
import ParseInt exposing (toRadixUnsafe)


all : Test
all =
    describe "QRCode Test Suite"
        [ describe "Encode"
            [ -- From Annex I.1 - ISO/IEC 18004:2006(E)
              test "Numeric" <| \() ->
                Numeric.encode "01234567"
                    |> Result.withDefault []
                    |> Expect.equal
                        [ ( 12 , 10 )
                        , ( 345, 10 )
                        , ( 67, 7 )
                        ]
            , test "Full Numeric" <| \() ->
                Encode.encode "01234567" ECLevel.M
                    |> Result.map Tuple.second
                    |> Result.withDefault []
                    |> List.map (toRadixUnsafe 2)
                    |> List.map (String.padLeft 8 '0')
                    |> Expect.equal
                        [ "00010000"
                        , "00100000"
                        , "00001100"
                        , "01010110"
                        , "01100001"
                        , "10000000"
                        , "11101100"
                        , "00010001"
                        , "11101100"
                        , "00010001"
                        , "11101100"
                        , "00010001"
                        , "11101100"
                        , "00010001"
                        , "11101100"
                        , "00010001"
                        , "10100101"
                        , "00100100"
                        , "11010100"
                        , "11000001"
                        , "11101101"
                        , "00110110"
                        , "11000111"
                        , "10000111"
                        , "00101100"
                        , "01010101"
                        ]           
            -- From http://www.thonky.com/qr-code-tutorial/data-encoding
            , test "Full Numeric" <| \() ->
                Alphanumeric.encode "HELLO WORLD"
                    |> Result.withDefault []
                    |> List.map (\(code, length) ->
                        toRadixUnsafe 2 code
                            |> String.padLeft length '0'
                        )
                    |> String.concat
                    |> Expect.equal "0110000101101111000110100010111001011011100010011010100001101"
            , test "Giant Alphanumeric" <| \() ->
                Encode.encode "123456789ABCDEFGHIJKLMNOPQRSTUVWXY $%*+-./:" ECLevel.L
                    |> Result.map (Tuple.first >> .mode >> toString)
                    |> Expect.equal (Result.Ok "Alphanumeric")
            , fuzz string "Fuzz ECLevel M" <| \rndStr ->
                Encode.encode rndStr ECLevel.M
                    |> Result.map (\_ -> ())
                    |> Expect.equal (Result.Ok ())
            , fuzz string "Fuzz ECLevel Q" <| \rndStr ->
                Encode.encode (String.toUpper rndStr) ECLevel.Q
                    |> Result.map (\_ -> ())
                    |> Expect.equal (Result.Ok ())
            , fuzz (Fuzz.intRange 0 10000000000) "Fuzz ECLevel L" <| \rndInt ->
                Encode.encode (toString rndInt) ECLevel.L
                    |> Result.map (Tuple.first >> .mode >> toString)
                    |> Expect.equal (Result.Ok "Numeric")
            ]
        ]

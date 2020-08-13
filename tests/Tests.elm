module Tests exposing (all)

import Expect
import Fuzz exposing (int, list, string, tuple)
import ParseInt exposing (toRadixUnsafe)
import QRCode.ECLevel as ECLevel
import QRCode.Encode as Encode
import QRCode.Encode.Alphanumeric as Alphanumeric
import QRCode.Encode.Numeric as Numeric
import String
import Test exposing (..)


all : Test
all =
    describe "QRCode Test Suite"
        [ describe "Encode"
            [ -- From Annex I.1 - ISO/IEC 18004:2006(E)
              test "Numeric" <|
                \() ->
                    Numeric.encode "01234567"
                        |> Result.withDefault []
                        |> Expect.equal
                            [ ( 12, 10 )
                            , ( 345, 10 )
                            , ( 67, 7 )
                            ]
            , test "Full Numeric" <|
                \() ->
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
            , test "All Numeric" <|
                \() ->
                    Encode.encode "1234567890" ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Expect.equal (Result.Ok "Numeric")
            , test "Giant Numeric" <|
                \() ->
                    Encode.encode (String.repeat 1000 "1234567890") ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Result.mapError Debug.toString
                        |> Expect.equal (Result.Err "InputLengthOverflow")

            --From http://www.thonky.com/qr-code-tutorial/data-encoding
            , test "All caps" <|
                \() ->
                    Alphanumeric.encode "HELLO WORLD"
                        |> Result.withDefault []
                        |> List.map
                            (\( code, length ) ->
                                toRadixUnsafe 2 code
                                    |> String.padLeft length '0'
                            )
                        |> String.concat
                        |> Expect.equal "0110000101101111000110100010111001011011100010011010100001101"
            , test "All Alphanumerics" <|
                \() ->
                    Encode.encode "123456789ABCDEFGHIJKLMNOPQRSTUVWXY $%*+-./:" ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Expect.equal (Result.Ok "Alphanumeric")
            , test "Giant Alphanumeric" <|
                \() ->
                    Encode.encode (String.repeat 100 "123456789ABCDEFGHIJKLMNOPQRSTUVWXY $%*+-./:") ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Result.mapError Debug.toString
                        |> Expect.equal (Result.Err "InputLengthOverflow")
            , test "Byte encoder" <|
                \() ->
                    Encode.encode "coração" ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Expect.equal (Result.Ok "Byte")
            , test "Giant Byte" <|
                \() ->
                    Encode.encode (String.repeat 500 "Coração") ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Result.mapError Debug.toString
                        |> Expect.equal (Result.Err "InputLengthOverflow")
            , test "UTF8 encoder" <|
                \() ->
                    Encode.encode " © ® ™ • ½ ¼ ¾ ⅓ ⅔" ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Expect.equal (Result.Ok "UTF8")
            , test "Giant UTF8" <|
                \() ->
                    Encode.encode (String.repeat 150 " © ® ™ • ½ ¼ ¾ ⅓ ⅔") ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Result.mapError Debug.toString
                        |> Expect.equal (Result.Err "InputLengthOverflow")
            , fuzz string "Fuzz ECLevel M" <|
                \rndStr ->
                    Encode.encode rndStr ECLevel.M
                        |> Result.map (\_ -> ())
                        |> Expect.equal (Result.Ok ())
            , fuzz string "Fuzz ECLevel Q" <|
                \rndStr ->
                    Encode.encode (String.toUpper rndStr) ECLevel.Q
                        |> Result.map (\_ -> ())
                        |> Expect.equal (Result.Ok ())
            , fuzz (Fuzz.intRange 0 10000000000) "Fuzz ECLevel L" <|
                \rndInt ->
                    Encode.encode (Debug.toString rndInt) ECLevel.L
                        |> Result.map (Tuple.first >> .mode >> Debug.toString)
                        |> Expect.equal (Result.Ok "Numeric")
            ]
        ]

module QRCode.Encode exposing (..)


import Char
import Dict exposing (Dict)
import Regex exposing (Regex)
import Bitwise as Bit exposing (shiftLeftBy, shiftRightBy)
import ParseInt
import QRCode.GroupInfo as Group exposing (GroupInfo)
import QRCode.ErrorCorrection as ErrorCorrection
import QRCode.Error exposing (Error(..))



type alias Model =
    { inputStr  : String
    , ecLevel   : ECLevel
    , mode      : Mode
    , groupInfo : GroupInfo
    }


type Mode
    = Numeric
    | Alphanumeric
    | Byte
    | UTF8


encode : String -> ECLevel -> Result Error ( Model, List Int )
encode inputStr ecLevel =
    let
        mode = selectMode inputStr

        encodedBits =
            case mode of
                Numeric      -> numeric inputStr
                Alphanumeric -> alphanumeric inputStr
                Byte         -> byte inputStr []
                UTF8         -> utf8 inputStr []

    in
        encodedBits
            |> Result.map String.concat
            |> Result.andThen (selectVersion inputStr ecLevel mode)
            |> Result.andThen infoAndFinalBits
            |> Result.andThen toBlocks
            |> Result.andThen errorCorrection
            |> Result.map finalFormat



---------------------------------------------------------------------
-- Error Correction
---------------------------------------------------------------------


type ECLevel = L | M | Q | H


ecLevelToInt : ECLevel -> Int
ecLevelToInt ecLevel =
    case ecLevel of
        L -> 1
        M -> 0
        Q -> 3
        H -> 2


errorCorrection : ( Model, List (List Int) ) -> Result Error ( Model, List (List Int), List (List Int) )
errorCorrection ( model, dataBlocks ) =
    ErrorCorrection.get model.groupInfo.ecPerBlock dataBlocks
        |> Result.map ((,,) model dataBlocks)



---------------------------------------------------------------------
-- Mode
---------------------------------------------------------------------


selectMode : String -> Mode
selectMode inputStr =
    if Regex.contains numericRegex inputStr then
        Numeric

    else if Regex.contains alphanumericMode inputStr then
        Alphanumeric

    else if Regex.contains byteMode inputStr then
        Byte

    else
        UTF8


numericRegex : Regex
numericRegex =
    Regex.regex "^[0-9]+$"


alphanumericMode : Regex
alphanumericMode =
    Regex.regex "^[0-9A-Z $%*+\\-.\\/:]+$"


byteMode : Regex
byteMode =
    Regex.regex "^[\\u0000-\\u00ff]+$"


modeIndicator : Mode -> String
modeIndicator mode =
    case mode of
        Numeric      -> "0001"
        Alphanumeric -> "0010"
        Byte         -> "0100"
        UTF8         -> "0100"



---------------------------------------------------------------------
-- Numeric
---------------------------------------------------------------------


numeric : String -> Result Error (List String)
numeric str =
    breakStr 3 str
        |> listResult encodeNumeric []


encodeNumeric : String -> Result Error String
encodeNumeric str =
    ParseInt.parseInt str
        |> Result.map (ParseInt.toRadixUnsafe 2)
        |> Result.map (numericToBinary str)
        |> Result.mapError (always InvalidNumericChar)


numericToBinary : String -> String -> String
numericToBinary originalStr =
    case String.length originalStr of
        1 -> String.padLeft 4 '0'
        2 -> String.padLeft 7 '0'
        _ -> String.padLeft 10 '0'



---------------------------------------------------------------------
-- Alphanumeric
---------------------------------------------------------------------


alphanumeric : String -> Result Error (List String)
alphanumeric str =
    breakStr 2 str
        |> listResult alphanumericToBinary []


alphanumericToBinary : String -> Result Error String
alphanumericToBinary str =
    case String.toList str of
        firstChar :: secondChar :: [] ->
            toAlphanumericCode firstChar
                |> Result.andThen (\firstCode ->
                        toAlphanumericCode secondChar
                            |> Result.map ((,) firstCode))
                |> Result.map (\(first, second) ->
                        (first * 45) + second)
                |> Result.map (ParseInt.toRadixUnsafe 2)
                |> Result.map (String.padLeft 11 '0')


        char :: [] ->
            toAlphanumericCode char
                |> Result.map (ParseInt.toRadixUnsafe 2)
                |> Result.map (String.padLeft 6 '0')

        _ ->
            Result.Err InvalidAlphanumericChar


toAlphanumericCode : Char -> Result Error Int
toAlphanumericCode char =
    Dict.get char alphanumericCode
        |> Result.fromMaybe InvalidAlphanumericChar


alphanumericCode : Dict Char Int
alphanumericCode =
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



---------------------------------------------------------------------
-- Byte
---------------------------------------------------------------------


byte : String -> List Int -> Result Error (List String)
byte str list =
    case String.uncons str of
        Just ( char, strTail ) ->
            Char.toCode char
                |> flip (::) list
                |> byte strTail

        Nothing ->
            List.reverse list
                |> List.map (ParseInt.toRadixUnsafe 2)
                |> List.map (String.padLeft 8 '0')
                |> Result.Ok



---------------------------------------------------------------------
-- UTF-8
---------------------------------------------------------------------


utf8 : String -> List Int -> Result Error (List String)
utf8 str list =
    case String.uncons str of
        Just ( char, strTail ) ->
            Char.toCode char
                |> utf8ToByte list strTail

        Nothing ->
            List.reverse list
                |> List.map (ParseInt.toRadixUnsafe 2)
                |> List.map (String.padLeft 8 '0')
                |> Result.Ok


-- http://stackoverflow.com/questions/18729405/how-to-convert-utf8-string-to-byte-array

utf8ToByte : List Int -> String -> Int -> Result Error (List String)
utf8ToByte list remainStr charCode =
    if charCode < 128 then
        charCode :: list
            |> utf8 remainStr

    else if charCode < 2048 then
        list
            |> (::) (Bit.or 192 (shiftRightBy 6 charCode))
            |> (::) (Bit.or 128 (and63 charCode))
            |> utf8 remainStr

    else if charCode < 55296 || charCode >= 57344 then
        list
            |> (::) (Bit.or 224 (shiftRightBy 12 charCode))
            |> (::) (Bit.or 128 (and63 (shiftRightBy 6 charCode)))
            |> (::) (Bit.or 128 (and63 charCode))
            |> utf8 remainStr

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
                    |> utf8 strTail


        Nothing ->
            Result.Err InvalidUTF8Char


-- 63 == 0x3f
and63 : Int -> Int
and63 = Bit.and 63



{--------------------------------------------------------------------
-- Version selector
--------------------------------------------------------------------}


selectVersion : String -> ECLevel -> Mode -> String -> Result Error ( String, Model )
selectVersion inputStr ecLevel mode encodedStr =
    4 + String.length encodedStr
        |> getVersion ecLevel mode
        |> Result.map (versionToModel inputStr ecLevel mode)
        |> Result.map ((,) encodedStr)


versionToModel : String -> ECLevel -> Mode -> GroupInfo -> Model
versionToModel inputStr ecLevel mode groupInfo =
    { inputStr  = inputStr
    , ecLevel   = ecLevel
    , mode      = mode
    , groupInfo = groupInfo
    }


getVersion : ECLevel -> Mode -> Int -> Result Error GroupInfo
getVersion ecLevel mode dataLength =
    getGroupData ecLevel
        |> List.filter (filterCapacity mode dataLength)
        |> List.sortBy .capacity
        |> List.head
        |> Result.fromMaybe InputLengthOverflow


getGroupData : ECLevel -> List GroupInfo
getGroupData ecLevel =
    case ecLevel of
        L -> Group.dataL
        M -> Group.dataM
        Q -> Group.dataQ
        H -> Group.dataH


filterCapacity : Mode -> Int -> GroupInfo -> Bool
filterCapacity mode dataLength { version, capacity } =
    charCountIndicatorLength mode version + dataLength
        |> \length -> length <= capacity



{--------------------------------------------------------------------
-- Add information and final bits:
Mode indicator, char count indicator, terminator, padding and filler.
--------------------------------------------------------------------}


infoAndFinalBits : ( String, Model ) -> Result Error ( Model, List Int)
infoAndFinalBits ( bitsStr, model ) =
    bitsStr
        |> (++) (charCountIndicator model bitsStr)
        |> (++) (modeIndicator model.mode)
        |> addTerminator model.groupInfo.capacity
        |> toMultipleOf8
        |> addFiller model.groupInfo.capacity
        |> breakStr 8
        |> listResult (ParseInt.parseIntRadix 2) []
        |> Result.map ((,) model)
        |> Result.mapError (always InvalidBinaryConversion)


charCountIndicator : Model -> String -> String
charCountIndicator { groupInfo, inputStr, mode } bitsStr =
    let
        charCount =
            if mode == UTF8
                then String.length bitsStr // 8
                else String.length inputStr

        length =
            charCountIndicatorLength mode groupInfo.version

    in
        charCount
            |> ParseInt.toRadixUnsafe 2
            |> String.padLeft length '0'


charCountIndicatorLength : Mode -> Int -> Int
charCountIndicatorLength mode version =
    if version <= 9 then
        case mode of
            Numeric      -> 10
            Alphanumeric -> 9
            Byte         -> 8
            UTF8         -> 8

    else if version <= 26 then
        case mode of
            Numeric      -> 12
            Alphanumeric -> 11
            Byte         -> 16
            UTF8         -> 16

    else
        case mode of
            Numeric      -> 14
            Alphanumeric -> 13
            Byte         -> 16
            UTF8         -> 16


addTerminator : Int -> String -> String
addTerminator totalLength bits =
    totalLength - String.length bits
        |> min 4
        |> flip String.repeat "0"
        |> (++) bits


toMultipleOf8 : String -> String
toMultipleOf8 bits =
    let
        remainer =
            String.length bits % 8

        repeatInt =
            if remainer == 0
                then 0
                else 8 - remainer

    in
        String.repeat repeatInt "0"
            |> (++) bits


addFiller : Int -> String -> String
addFiller totalLength bits =
    let
        fillerLength =
            (totalLength - String.length bits) // 8

    in
        firstFillerByte ++ secondFillerByte
            |> String.repeat (fillerLength // 2)
            |> (if fillerLength % 2 == 0
                    then identity
                    else flip (++) firstFillerByte)
            |> (++) bits


firstFillerByte : String
firstFillerByte =
    ParseInt.toRadixUnsafe 2 236
        |> String.padLeft 8 '0'


secondFillerByte : String
secondFillerByte =
    ParseInt.toRadixUnsafe 2 17
        |> String.padLeft 8 '0'



---------------------------------------------------------------------
-- To Blocks
---------------------------------------------------------------------


toBlocks : ( Model, List a ) -> Result Error ( Model, List (List a))
toBlocks (({ groupInfo } as model), byteList ) =
    case groupInfo.maybeGroup2 of
        Just group2 ->
            breakList False groupInfo.group1 ( byteList, [] )
                |> Result.andThen (breakList True group2)
                |> Result.map (Tuple.second >> List.reverse)
                |> Result.map ((,) model)

        Nothing ->
            breakList True groupInfo.group1 ( byteList, [] )
                |> Result.map (Tuple.second >> List.reverse)
                |> Result.map ((,) model)


breakList : Bool -> ( Int, Int ) -> ( List a, List (List a) ) -> Result Error ( List a, List (List a) )
breakList checkFinish ( times, itemCount ) ( byteList, progress ) =
    if times > 0 then
        let
            block =
                List.take itemCount byteList

            remainList =
                List.drop itemCount byteList

        in
            ( remainList, block :: progress )
                |> breakList checkFinish ( (times - 1), itemCount )

    else if checkFinish && List.length byteList > 0 then
        Result.Err InputLengthOverflow

    else
        Result.Ok ( byteList, progress )



---------------------------------------------------------------------
-- Final formatation
---------------------------------------------------------------------


finalFormat : ( Model, List (List Int), List (List Int) ) -> ( Model, List Int )
finalFormat ( model, dataBlocks, ecBlocks ) =
    transpose ecBlocks
        |> (++) (transpose dataBlocks)
        |> List.concat
        |> (,) model



---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------


listResult : ( a -> Result x b ) -> List b -> List a -> Result x (List b)
listResult fun listb lista =
    case lista of
        head :: tail ->
            fun head
                |> Result.map (\r -> r :: listb)
                |> Result.andThen (flip (listResult fun) tail)

        [] ->
            Result.Ok (List.reverse listb)


-- From elm-community/string-extra

breakStr : Int -> String -> List String
breakStr width string =
    if width == 0 || string == ""
        then [ string ]
        else breaker width string []


breaker : Int -> String -> List String -> List String
breaker width string acc =
    case string of
        "" ->
            List.reverse acc

        _ ->
            breaker width
                (String.dropLeft width string)
                ((String.slice 0 width string) :: acc)


transpose : List (List a) -> List (List a)
transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
                (x :: heads) :: transpose (xs :: tails)

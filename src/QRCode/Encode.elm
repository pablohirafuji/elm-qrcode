module QRCode.Encode exposing
    ( Model
    , encode
    )

import Bitwise as Bit exposing (shiftLeftBy, shiftRightBy)
import QRCode.ECLevel exposing (ECLevel)
import QRCode.Encode.Alphanumeric as Alphanumeric
import QRCode.Encode.Byte as Byte
import QRCode.Encode.Numeric as Numeric
import QRCode.Encode.UTF8 as UTF8
import QRCode.Error exposing (Error(..))
import QRCode.ErrorCorrection as ErrorCorrection
import QRCode.GroupInfo as Group exposing (GroupInfo, getGroupData)
import QRCode.Helpers exposing (breakStr, listResult, transpose)


encode : String -> ECLevel -> Result Error ( Model, List Int )
encode inputStr ecLevel =
    let
        mode =
            selectMode inputStr
    in
    Result.map concatTranspose
        (Result.andThen getErrorCorrection
            (Result.andThen toBlocks
                (Result.map addInfoAndFinalBits
                    (Result.andThen
                        (selectVersion inputStr ecLevel mode)
                        (encoder mode inputStr)
                    )
                )
            )
        )


encoder : Mode -> (String -> Result Error (List ( Int, Int )))
encoder mode =
    case mode of
        Numeric ->
            Numeric.encode

        Alphanumeric ->
            Alphanumeric.encode

        Byte ->
            Byte.encode

        UTF8 ->
            UTF8.encode



---------------------------------------------------------------------
-- Mode
---------------------------------------------------------------------


type Mode
    = Numeric
    | Alphanumeric
    | Byte
    | UTF8


selectMode : String -> Mode
selectMode input =
    if Numeric.isValid input then
        Numeric

    else if Alphanumeric.isValid input then
        Alphanumeric

    else if Byte.isValid input then
        Byte

    else
        UTF8


modeIndicator : Mode -> Int
modeIndicator mode =
    case mode of
        Numeric ->
            1

        Alphanumeric ->
            2

        Byte ->
            4

        UTF8 ->
            4



---------------------------------------------------------------------
-- Version selector
---------------------------------------------------------------------


type alias Model =
    { inputStr : String
    , ecLevel : ECLevel
    , mode : Mode
    , groupInfo : GroupInfo
    , bitsCount : Int
    }


selectVersion : String -> ECLevel -> Mode -> List ( Int, Int ) -> Result Error ( List ( Int, Int ), Model )
selectVersion inputStr ecLevel mode encodedStr =
    let
        partialBitsCount =
            4 + List.foldl (\a b -> Tuple.second a + b) 0 encodedStr

        -- Add mode indicator bits
    in
    Result.map (\b -> ( encodedStr, b ))
        (Result.map
            (versionToModel inputStr
                ecLevel
                mode
                partialBitsCount
            )
            (getVersion ecLevel mode partialBitsCount)
        )


versionToModel : String -> ECLevel -> Mode -> Int -> GroupInfo -> Model
versionToModel inputStr ecLevel mode partialBitsCount groupInfo =
    { inputStr = inputStr
    , ecLevel = ecLevel
    , mode = mode
    , groupInfo = groupInfo
    , bitsCount =
        partialBitsCount
            + charCountIndicatorLength mode groupInfo.version
    }


getVersion : ECLevel -> Mode -> Int -> Result Error GroupInfo
getVersion ecLevel mode dataLength =
    Result.fromMaybe InputLengthOverflow
        (List.head
            (List.sortBy .capacity
                (List.filter (filterCapacity mode dataLength)
                    (getGroupData ecLevel)
                )
            )
        )


filterCapacity : Mode -> Int -> GroupInfo -> Bool
filterCapacity mode dataLength { version, capacity } =
    (charCountIndicatorLength mode version
        + dataLength
    )
        <= capacity



---------------------------------------------------------------------
-- Add information and trailing bits
---------------------------------------------------------------------


addInfoAndFinalBits : ( List ( Int, Int ), Model ) -> ( Model, List Int )
addInfoAndFinalBits ( bits, model ) =
    ( model
    , addFiller model.groupInfo.capacity
        (bitsToBytes
            (addTerminator model.groupInfo.capacity
                model.bitsCount
                (( modeIndicator model.mode, 4 )
                    :: charCountIndicator model bits
                    :: bits
                )
            )
        )
    )


charCountIndicator : Model -> List ( Int, Int ) -> ( Int, Int )
charCountIndicator { groupInfo, inputStr, mode } bits =
    let
        charCount =
            if mode == UTF8 then
                List.length bits

            else
                String.length inputStr

        length =
            charCountIndicatorLength mode groupInfo.version
    in
    ( charCount
    , length
    )


charCountIndicatorLength : Mode -> Int -> Int
charCountIndicatorLength mode version =
    if version <= 9 then
        case mode of
            Numeric ->
                10

            Alphanumeric ->
                9

            Byte ->
                8

            UTF8 ->
                8

    else if version <= 26 then
        case mode of
            Numeric ->
                12

            Alphanumeric ->
                11

            Byte ->
                16

            UTF8 ->
                16

    else
        case mode of
            Numeric ->
                14

            Alphanumeric ->
                13

            Byte ->
                16

            UTF8 ->
                16


addTerminator : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
addTerminator capacity bitsCount bits =
    bits ++ [ ( 0, min 4 (capacity - bitsCount) ) ]


bitsToBytes : List ( Int, Int ) -> List Int
bitsToBytes bits =
    bitsToBytes1 bits ( ( 0, 0 ), [] )


bitsToBytes1 : List ( Int, Int ) -> ( ( Int, Int ), List Int ) -> List Int
bitsToBytes1 bits ( ( remBits, remLength ), bytes ) =
    case bits of
        head :: tail ->
            bitsToBytes1 tail
                (bitsToBytes2 head ( ( remBits, remLength ), bytes ))

        [] ->
            if remLength == 0 then
                List.reverse bytes

            else
                List.reverse
                    (Bit.shiftLeftBy (8 - remLength) remBits
                        :: bytes
                    )


bitsToBytes2 : ( Int, Int ) -> ( ( Int, Int ), List Int ) -> ( ( Int, Int ), List Int )
bitsToBytes2 ( curBits, curLength ) ( ( remBits, remLength ), bytes ) =
    let
        lengthSum =
            curLength + remLength

        bitsSum =
            Bit.or curBits
                (Bit.shiftLeftBy curLength remBits)
    in
    bitsToBytes3 ( ( bitsSum, lengthSum ), bytes )


bitsToBytes3 : ( ( Int, Int ), List Int ) -> ( ( Int, Int ), List Int )
bitsToBytes3 ( ( bits, length ), bytes ) =
    if length >= 8 then
        let
            remLength =
                length - 8

            remBits =
                Bit.and bits
                    (Bit.shiftLeftBy remLength 1 - 1)

            byte =
                Bit.shiftRightBy remLength bits
        in
        bitsToBytes3
            ( ( remBits, remLength )
            , byte :: bytes
            )

    else
        ( ( bits, length )
        , bytes
        )


addFiller : Int -> List Int -> List Int
addFiller capacity bytes =
    let
        fillerLength =
            (capacity // 8) - List.length bytes

        ns =
            List.concat
                (List.repeat (fillerLength // 2)
                    [ firstFillerByte, secondFillerByte ]
                )
    in
    if modBy 2 fillerLength == 0 then
        bytes ++ ns

    else
        bytes ++ ns ++ [ firstFillerByte ]


firstFillerByte : Int
firstFillerByte =
    236


secondFillerByte : Int
secondFillerByte =
    17



---------------------------------------------------------------------
-- To Blocks
---------------------------------------------------------------------


toBlocks : ( Model, List a ) -> Result Error ( Model, List (List a) )
toBlocks ( { groupInfo } as model, byteList ) =
    case groupInfo.maybeGroup2 of
        Just group2 ->
            Result.map (\b -> ( model, b ))
                (Result.map (Tuple.second >> List.reverse)
                    (Result.andThen (breakList True group2)
                        (breakList False groupInfo.group1 ( byteList, [] ))
                    )
                )

        Nothing ->
            Result.map (\b -> ( model, b ))
                (Result.map (Tuple.second >> List.reverse)
                    (breakList True groupInfo.group1 ( byteList, [] ))
                )


breakList : Bool -> ( Int, Int ) -> ( List a, List (List a) ) -> Result Error ( List a, List (List a) )
breakList checkFinish ( times, itemCount ) ( byteList, progress ) =
    if times > 0 then
        let
            block =
                List.take itemCount byteList

            remainList =
                List.drop itemCount byteList
        in
        breakList checkFinish
            ( times - 1, itemCount )
            ( remainList, block :: progress )

    else if checkFinish && List.length byteList > 0 then
        Result.Err InputLengthOverflow

    else
        Result.Ok ( byteList, progress )



---------------------------------------------------------------------
-- Error Correction
---------------------------------------------------------------------


getErrorCorrection : ( Model, List (List Int) ) -> Result Error ( Model, List (List Int), List (List Int) )
getErrorCorrection ( model, dataBlocks ) =
    Result.map (\c -> ( model, dataBlocks, c ))
        (ErrorCorrection.get model.groupInfo.ecPerBlock dataBlocks)



---------------------------------------------------------------------
-- Final formatation
---------------------------------------------------------------------


concatTranspose : ( Model, List (List Int), List (List Int) ) -> ( Model, List Int )
concatTranspose ( model, dataBlocks, ecBlocks ) =
    ( model
    , List.concat (transpose dataBlocks ++ transpose ecBlocks)
    )

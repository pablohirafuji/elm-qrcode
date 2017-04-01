module QRCode.Encode exposing
    ( encode
    , Model
    )


import Bitwise as Bit exposing (shiftLeftBy, shiftRightBy)
import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (listResult, breakStr, transpose)
import QRCode.ECLevel exposing (ECLevel)
import QRCode.GroupInfo as Group exposing (GroupInfo, getGroupData)
import QRCode.ErrorCorrection as ErrorCorrection
import QRCode.Encode.Numeric as Numeric
import QRCode.Encode.Alphanumeric as Alphanumeric
import QRCode.Encode.Byte as Byte
import QRCode.Encode.UTF8 as UTF8



encode : String -> ECLevel -> Result Error ( Model, List Int )
encode inputStr ecLevel =
    let
        mode = selectMode inputStr

    in
        inputStr
            |> encoder mode
            |> Result.andThen (selectVersion inputStr ecLevel mode)
            |> Result.map addInfoAndFinalBits
            |> Result.andThen toBlocks
            |> Result.andThen getErrorCorrection
            |> Result.map concatTranspose


encoder : Mode -> ( String -> Result Error (List ( Int, Int )) )
encoder mode =
    case mode of
        Numeric      -> Numeric.encode
        Alphanumeric -> Alphanumeric.encode
        Byte         -> Byte.encode
        UTF8         -> UTF8.encode



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
        Numeric      -> 1
        Alphanumeric -> 2
        Byte         -> 4
        UTF8         -> 4



---------------------------------------------------------------------
-- Version selector
---------------------------------------------------------------------


type alias Model =
    { inputStr  : String
    , ecLevel   : ECLevel
    , mode      : Mode
    , groupInfo : GroupInfo
    , bitsCount : Int
    }


selectVersion : String -> ECLevel -> Mode -> List ( Int, Int ) -> Result Error ( List ( Int, Int ), Model )
selectVersion inputStr ecLevel mode encodedStr =
    let
        partialBitsCount = 
            List.foldl (\a b -> Tuple.second a + b) 0 encodedStr
                |> (+) 4 -- Add mode indicator bits

    in
        partialBitsCount
            |> getVersion ecLevel mode
            |> Result.map (versionToModel inputStr ecLevel
                mode partialBitsCount)
            |> Result.map ((,) encodedStr)


versionToModel : String -> ECLevel -> Mode -> Int -> GroupInfo -> Model
versionToModel inputStr ecLevel mode partialBitsCount groupInfo =
    { inputStr  = inputStr
    , ecLevel   = ecLevel
    , mode      = mode
    , groupInfo = groupInfo
    , bitsCount = partialBitsCount
        + charCountIndicatorLength mode groupInfo.version
    }


getVersion : ECLevel -> Mode -> Int -> Result Error GroupInfo
getVersion ecLevel mode dataLength =
    getGroupData ecLevel
        |> List.filter (filterCapacity mode dataLength)
        |> List.sortBy .capacity
        |> List.head
        |> Result.fromMaybe InputLengthOverflow


filterCapacity : Mode -> Int -> GroupInfo -> Bool
filterCapacity mode dataLength { version, capacity } =
    charCountIndicatorLength mode version + dataLength
        |> \length -> length <= capacity



---------------------------------------------------------------------
-- Add information and trailing bits
---------------------------------------------------------------------


addInfoAndFinalBits : ( List ( Int, Int ), Model ) -> ( Model, List Int)
addInfoAndFinalBits ( bits, model ) =
    bits
        |> (::) (charCountIndicator model bits)
        |> (::) ( modeIndicator model.mode, 4 )
        |> addTerminator model.groupInfo.capacity model.bitsCount
        |> bitsToBytes
        |> addFiller model.groupInfo.capacity
        |> (,) model


charCountIndicator : Model -> List ( Int, Int ) -> ( Int, Int )
charCountIndicator { groupInfo, inputStr, mode } bits =
    let
        charCount =
            if mode == UTF8
                then List.length bits
                else String.length inputStr

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


addTerminator : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
addTerminator capacity bitsCount bits =
    capacity - bitsCount
        |> min 4
        |> (,) 0
        |> flip (::) []
        |> (++) bits


bitsToBytes : List ( Int, Int ) -> List Int
bitsToBytes bits =
    bitsToBytes1 bits ( ( 0, 0 ), [] )


bitsToBytes1 : List ( Int, Int ) -> ( ( Int, Int ), List Int ) -> List Int
bitsToBytes1 bits ( ( remBits, remLength ), bytes ) =
    case bits of
        head :: tail ->
            bitsToBytes2 head ( ( remBits, remLength ), bytes )
                |> bitsToBytes1 tail

        [] ->
            if remLength == 0 then
                List.reverse bytes

            else
                Bit.shiftLeftBy (8 - remLength) remBits
                    |> flip (::) bytes
                    |> List.reverse


bitsToBytes2 : ( Int, Int ) -> ( ( Int, Int ), List Int ) -> ( ( Int, Int ), List Int )
bitsToBytes2 ( curBits, curLength ) ( ( remBits, remLength ), bytes ) =
    let
        lengthSum = curLength + remLength

        bitsSum =
            Bit.shiftLeftBy curLength remBits
                |> Bit.or curBits

    in
        bitsToBytes3 ( ( bitsSum, lengthSum ), bytes )


bitsToBytes3 : ( ( Int, Int ), List Int ) -> ( ( Int, Int ), List Int )
bitsToBytes3 ( ( bits, length ), bytes ) =
    if length >= 8 then
        let
            remLength = length - 8

            remBits = 
                Bit.shiftLeftBy remLength 1
                    |> flip (-) 1
                    |> Bit.and bits

            byte =
                Bit.shiftRightBy remLength bits

        in
            ( ( remBits, remLength)
            , byte :: bytes
            ) |> bitsToBytes3

    else
        ( ( bits, length )
        , bytes
        )


addFiller : Int -> List Int -> List Int
addFiller capacity bytes =
    let
        fillerLength =
            (capacity // 8) - List.length bytes

    in
        [ firstFillerByte, secondFillerByte ]
            |> List.repeat (fillerLength // 2)
            |> List.concat
            |> (if fillerLength % 2 == 0
                    then identity
                    else flip (++) [ firstFillerByte ])
            |> (++) bytes


firstFillerByte : Int
firstFillerByte = 236


secondFillerByte : Int
secondFillerByte = 17



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
-- Error Correction
---------------------------------------------------------------------


getErrorCorrection : ( Model, List (List Int) ) -> Result Error ( Model, List (List Int), List (List Int) )
getErrorCorrection ( model, dataBlocks ) =
    ErrorCorrection.get model.groupInfo.ecPerBlock dataBlocks
        |> Result.map ((,,) model dataBlocks)



---------------------------------------------------------------------
-- Final formatation
---------------------------------------------------------------------


concatTranspose : ( Model, List (List Int), List (List Int) ) -> ( Model, List Int )
concatTranspose ( model, dataBlocks, ecBlocks ) =
    transpose ecBlocks
        |> (++) (transpose dataBlocks)
        |> List.concat
        |> (,) model


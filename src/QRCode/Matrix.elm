module QRCode.Matrix exposing
    ( Model
    , apply
    )


import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy, shiftRightZfBy)
import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (transpose)
import QRCode.Encode as Encode
import QRCode.ECLevel exposing (ECLevel(..))


type alias Model = List (List Bool)


type alias Matrix = Array (Maybe Module)


type alias Module = ( Bool, Bool ) -- ( isReserved, isDark )


apply : ( Encode.Model, List Int ) -> Result Error Model
apply ( { ecLevel, groupInfo }, bytes )  =
    let
        version = groupInfo.version

        size = ((version - 1) * 4) + 21

    in
        Array.initialize (size * size) (always Nothing)
            |> finderPattern size -1 -1
            |> finderPattern size (size - 8) -1
            |> finderPattern size -1 (size - 8)
            |> reserveFormatInfo size
            |> setVersionInfo version size
            |> darkModule version size
            |> timingPattern size
            |> alignmentPattern version size
            |> Result.map (addData size bytes)
            |> Result.map (getBestMask ecLevel size)
            --|> Result.map (setFormatInfo ecLevel size)
            --|> Result.map (Model size)



---------------------------------------------------------------------
-- Finder Pattern
---------------------------------------------------------------------


finderPattern : Int -> Int -> Int -> Matrix -> Matrix
finderPattern size rowOffset colOffset matrix =
    getAreaCoord finderRange finderRange
        |> List.foldl (setFinder size rowOffset colOffset) matrix


finderRange : List Int
finderRange =
    List.range 0 8


setFinder : Int -> Int -> Int -> ( Int, Int ) -> Matrix -> Matrix
setFinder size rowOffset colOffset ( row, col ) matrix =
    let
        finalRow = row + rowOffset
        finalCol = col + colOffset

    in
        if finalRow < 0 || finalCol < 0
            || finalRow >= size || finalCol >= size then
                matrix

        else
            Array.set
                (getIndex size finalRow finalCol)
                (Just ( True, finderColor row col ))
                matrix


finderColor : Int -> Int -> Bool
finderColor row col =
    (1 <= row && row <= 7 && (col == 1 || col == 7))
        || (1 <= col && col <= 7 && (row == 1 || row == 7))
        || (3 <= row && row <= 5 && 3 <= col && col <= 5)



---------------------------------------------------------------------
-- Format Information Area
---------------------------------------------------------------------


reserveFormatInfo : Int -> Matrix -> Matrix
reserveFormatInfo size matrix =
    setFormatInfo_ size (always True) 0 matrix


setFormatInfo : ECLevel -> Int -> Mask -> Matrix -> Matrix
setFormatInfo ecLevel size mask matrix =
    let
        bits = encodeFormatInfo ecLevel mask

        isBlack bits_ count =
            shiftRightBy count bits_
                |> Bitwise.and 1
                |> (==) 1

    in
        setFormatInfo_ size (isBlack bits) 0 matrix


setFormatInfo_ : Int -> ( Int -> Bool ) -> Int -> Matrix -> Matrix
setFormatInfo_ size isBlackFn count matrix =
    if count < 15 then
        let
            ( x1, y1 ) = formatInfoHorizontal size count

            ( x2, y2 ) =  formatInfoVertical size count

            isBlack = isBlackFn count

        in
            setFormatModule size isBlack x1 y1 matrix
                |> setFormatModule size isBlack x2 y2
                |> setFormatInfo_ size isBlackFn (count + 1)

    else
        matrix


formatInfoHorizontal : Int -> Int -> ( Int, Int )
formatInfoHorizontal size count =
    if count < 8 then
        ( 8, size - count - 1 )

    else if count < 9 then
        ( 8, 15 - count)

    else
        ( 8, 15 - count - 1 )


formatInfoVertical : Int -> Int -> ( Int, Int )
formatInfoVertical size count =
    if count < 6 then
        ( count, 8 )

    else if count < 8 then
        ( count + 1, 8 )

    else
        ( size - 15 + count, 8 )


setFormatModule : Int -> Bool -> Int -> Int -> Matrix -> Matrix
setFormatModule size isBlack row col =
    Array.set (getIndex size row col) (Just ( True, isBlack ))


encodeFormatInfo : ECLevel -> Mask -> Int
encodeFormatInfo ecLevel mask =
    let
        formatInfoInt =
            shiftLeftBy 3 (ecLevelToInt ecLevel)
                |> Bitwise.or (maskToInt mask)

        d = shiftLeftBy 10 formatInfoInt

        g15Int = 1335

        g15Digit = getBCHDigit g15Int

        g15Mask = 21522

        helper d_ =
            if getBCHDigit d_ - g15Digit >= 0 then
                getBCHDigit d_ - g15Digit
                    |> flip shiftLeftBy g15Int
                    |> Bitwise.xor d_
                    |> helper

            else
                shiftLeftBy 10 formatInfoInt
                    |> Bitwise.or d_
                    |> Bitwise.xor g15Mask

    in
        helper d


ecLevelToInt : ECLevel -> Int
ecLevelToInt ecLevel =
    case ecLevel of
        L -> 1
        M -> 0
        Q -> 3
        H -> 2



---------------------------------------------------------------------
-- Version Information Area
---------------------------------------------------------------------


setVersionInfo : Int -> Int -> Matrix -> Matrix
setVersionInfo version size matrix =
    if version >= 7 then
        let
            bits = encodeVersionInfo version

            isBlack bits_ count =
                shiftRightBy count bits_
                    |> Bitwise.and 1
                    |> (==) 1

        in
            setVersionInfo_ size (isBlack bits) 0 matrix

    else
        matrix


setVersionInfo_ : Int -> ( Int -> Bool ) -> Int -> Matrix -> Matrix
setVersionInfo_ size isBlackFn count matrix =
    if count < 18 then
        let
            topRight =
                ( floor (toFloat count / 3)
                , count % 3 + size - 8 - 3
                )

            bottomLeft =
                ( count % 3 + size - 8 - 3
                , floor (toFloat count / 3)
                )

            isBlack = isBlackFn count

        in
            setVersionModule size isBlack topRight matrix
                |> setVersionModule size isBlack bottomLeft
                |> setVersionInfo_ size isBlackFn (count + 1)

    else
        matrix


setVersionModule : Int -> Bool -> ( Int, Int ) -> Matrix -> Matrix
setVersionModule size isBlack ( row, col ) =
    Array.set (getIndex size row col) (Just ( True, isBlack ))


encodeVersionInfo : Int -> Int
encodeVersionInfo version =
    let
        d = shiftLeftBy 12 version

        g18Int = 7973

        g18Digit = getBCHDigit g18Int

        helper d_ =
            if getBCHDigit d_ - g18Digit >= 0 then
                getBCHDigit d_ - g18Digit
                    |> flip shiftLeftBy g18Int
                    |> Bitwise.xor d_
                    |> helper

            else
                shiftLeftBy 12 version
                    |> Bitwise.or d_

    in
        helper d



---------------------------------------------------------------------
-- Dark Module
---------------------------------------------------------------------


darkModule : Int -> Int -> Matrix -> Matrix
darkModule version size =
    Array.set
        (getIndex size ((4 * version) + 9) 8)
        (Just ( True, True ))



---------------------------------------------------------------------
-- Timing Pattern
---------------------------------------------------------------------


timingPattern : Int -> Matrix -> Matrix
timingPattern size matrix =
    let range = List.range 8 (size - 9)
    in List.foldl (setTiming size 6) matrix range
        |> flip (List.foldl (flip (setTiming size) 6)) range


setTiming : Int -> Int -> Int -> Matrix -> Matrix
setTiming size row col =
    Array.set (getIndex size row col) (timingColor row col)


timingColor : Int -> Int -> Maybe Module
timingColor row col =
    if (row + col) % 2 == 0
        then Just ( True, True )
        else Just ( True, False )



---------------------------------------------------------------------
-- Alignment Pattern
---------------------------------------------------------------------


alignmentPattern : Int -> Int -> Matrix -> Result Error Matrix
alignmentPattern version size matrix =
    Array.get (version - 1) alignmentPatternData
        |> Result.fromMaybe AlignmentPatternNotFound
        |> Result.map (flip (setAlignments size) matrix)


setAlignments : Int -> List Int -> Matrix -> Matrix
setAlignments size locations matrix =
    getAreaCoord locations locations
        |> List.filter (isValidAlign size)
        |> List.foldl (setAlignment size) matrix


isValidAlign : Int -> ( Int, Int ) -> Bool
isValidAlign size ( row, col ) =
    (row > 10 || (10 < col && col < size - 10))
        && (row < size - 10 || col > 10)


setAlignment : Int -> ( Int, Int ) -> Matrix -> Matrix
setAlignment size ( row, col ) matrix =
    getAreaCoord alignmentRange alignmentRange
        |> List.foldl (setAlignModule size row col) matrix


alignmentRange : List Int
alignmentRange =
    List.range -2 2


setAlignModule : Int -> Int -> Int -> ( Int, Int ) -> Matrix -> Matrix
setAlignModule size rowPos colPos ( row, col ) =
    Array.set
        (getIndex size (row + rowPos) (col + colPos))
        (alignmentColor row col)


alignmentColor : Int -> Int -> Maybe Module
alignmentColor row col =
    if row == -2 || row == 2 || col == -2 || col == 2
        || (row == 0 && col == 0)
            then Just ( True, True )
            else Just ( True, False )



---------------------------------------------------------------------
-- Data Module Placement
---------------------------------------------------------------------


type alias Placement =
    { size : Int
    , row : Int
    , col : Int
    , isRight : Bool
    , isUp : Bool
    }


initPlacement : Int -> Placement
initPlacement size =
    { size = size
    , row = size + 1
    , col = size + 1
    , isRight = True
    , isUp = True
    }


addData : Int -> List Int -> Matrix -> Matrix
addData size bytes matrix =
    addDataModule (initPlacement size) bytes 0 matrix


addDataModule : Placement -> List Int -> Int -> Matrix -> Matrix
addDataModule ({ size, row, col } as placement) bytes offset matrix =
    case bytes of
        [] ->
            matrix

        head :: tail ->
            if offset >= 8 then
                addDataModule placement tail 0 matrix

            else if col == 6 then
                addDataModule
                    { placement
                        | col = col - 1
                        , isRight = True
                    } bytes offset matrix

            else if row < 0 then
                addDataModule
                    { placement
                        | row = 0
                        , col = col - 2
                        , isRight = True
                        , isUp = False
                    } bytes offset matrix

            else if row >= size then
                addDataModule
                    { placement
                        | row = size - 1
                        , col = col - 2
                        , isRight = True
                        , isUp = True
                    } bytes offset matrix

            else if isOccupy row col size matrix then
                addDataModule
                    (nextModule placement)
                    bytes offset matrix

            else
                setDataModule placement head offset matrix
                    |> addDataModule
                        (nextModule placement)
                        bytes (offset + 1)


nextModule : Placement -> Placement
nextModule ({ row, col, isRight, isUp } as placement) =
    if isRight then
        { placement
            | col = col - 1
            , isRight = False
        }

    else if isUp then
        { placement
            | row = row - 1
            , col = col + 1
            , isRight = True
        }

    else
        { placement
            | row = row + 1
            , col = col + 1
            , isRight = True
        }


setDataModule : Placement -> Int -> Int -> Matrix -> Matrix
setDataModule { size, row, col } byte offset =
    Array.set (getIndex size row col)
        (Just ( False, bitToColor byte offset ))


bitToColor : Int -> Int -> Bool
bitToColor byte offset =
    Bitwise.shiftRightBy (7 - offset) byte
        |> Bitwise.and 1
        |> (==) 1



---------------------------------------------------------------------
-- Mask
---------------------------------------------------------------------


type Mask
    = Pattern0
    | Pattern1
    | Pattern2
    | Pattern3
    | Pattern4
    | Pattern5
    | Pattern6
    | Pattern7


patternList : List Mask
patternList =
    [ Pattern0
    , Pattern1
    , Pattern2
    , Pattern3
    , Pattern4
    , Pattern5
    , Pattern6
    , Pattern7
    ]


maskToInt : Mask -> Int
maskToInt mask =
    case mask of
        Pattern0 -> 0
        Pattern1 -> 1
        Pattern2 -> 2
        Pattern3 -> 3
        Pattern4 -> 4
        Pattern5 -> 5
        Pattern6 -> 6
        Pattern7 -> 7


maskFunction : Mask -> ( ( Int, Int ) -> Bool )
maskFunction mask =
    case mask of
        Pattern0 -> \(row, col) ->
            (row + col) % 2 == 0

        Pattern1 -> \(row, _) ->
            row % 2 == 0

        Pattern2 -> \(_, col) ->
            col % 3 == 0

        Pattern3 -> \(row, col) ->
            (row + col) % 3 == 0

        Pattern4 -> \(row, col) ->
            (floor (toFloat row / 2) + floor (toFloat col / 3))
                % 2 == 0

        Pattern5 -> \(row, col) ->
            (row * col) % 2 + (row * col) % 3 == 0

        Pattern6 -> \(row, col) ->
            ((row * col) % 2 + (row * col) % 3) % 2 == 0

        Pattern7 -> \(row, col) ->
            ((row * col) % 3 + (row + col) % 2) % 2 == 0


getBestMask : ECLevel -> Int -> Matrix -> Model
getBestMask ecLevel size matrix =
    patternList
        |> List.foldl (getBestMask_ ecLevel size matrix) ( [], -1 )
        |> Tuple.first


getBestMask_ : ECLevel -> Int -> Matrix -> Mask -> ( Model, Int ) -> ( Model, Int )
getBestMask_ ecLevel size matrix mask ( minSMatrix, minScore ) =
    let
        maskedMatrix =
            applyMask size mask matrix
                |> setFormatInfo ecLevel size mask

        ( maskSMatrix, maskScore ) =
            getMaskScore size maskedMatrix

    in
        if minScore < maskScore && minScore /= -1 then
            ( minSMatrix, minScore )

        else
            ( maskSMatrix, maskScore )


getMaskScore : Int -> Matrix -> ( Model, Int )
getMaskScore size matrix =
    let
        list =
            Array.toList matrix
                |> List.map isDarkModule

        rowList =
            breakList size list []


        transposedRowList =
            transpose rowList

    in
        rule1Score rowList
            |> (+) (rule1Score transposedRowList)
            |> (+) (rule2Score rowList 0)
            |> (+) (rule3Score rowList)
            |> (+) (rule3Score transposedRowList)
            |> (+) (rule4Score size list)
            |> (,) rowList


breakList : Int -> List a -> List (List a) -> List (List a)
breakList width list acc =
    case list of
        [] ->
            List.reverse acc

        _ ->
            breakList width
                (List.drop width list)
                ((List.take width list) :: acc)


rule1Score : List (List Bool) -> Int
rule1Score =
    List.map (flip rule1Score_ ( False, 0, 0 ))
        >> List.sum


rule1Score_ : List Bool -> ( Bool, Int, Int ) -> Int
rule1Score_ simplifiedList ( last, partialScore, score ) =
    case simplifiedList of
        [] ->
            if partialScore >= 5
                then score + partialScore - 2
                else score

        head :: tail ->
            if last == head then
                ( last, partialScore + 1, score )
                    |> rule1Score_ tail

            else if partialScore >= 5 then
                ( head, 0, score + partialScore - 2 )
                    |> rule1Score_ tail

            else
                ( head, 0, score )
                    |> rule1Score_ tail


rule2Score : List (List Bool) -> Int -> Int
rule2Score list score =
    case list of
        head1 :: head2 :: tail ->
            rule2Score_ head1 head2 Nothing 0
                |> (+) score
                |> rule2Score tail

        _ ->
            score


rule2Score_ : List Bool -> List Bool -> Maybe Bool -> Int -> Int
rule2Score_ row1 row2 maybeLast score =
    case row1 of
        [] ->
            score

        head :: tail ->
            case row2 of
                [] ->
                    score

                head2 :: tail2 ->
                    if head == head2 then
                        if (Just head) == maybeLast then
                            score + 3
                                |> rule2Score_ tail tail2 (Just head)

                        else
                            rule2Score_ tail tail2 (Just head) score

                    else
                        rule2Score_ tail tail2 Nothing score



rule3Score : List (List Bool) -> Int
rule3Score =
    List.foldl rule3Score_ 0


rule3Score_ : List Bool -> Int -> Int
rule3Score_ simplifiedList score =
    case simplifiedList of
        [] -> score

        False :: False :: False :: False :: True :: False
            :: True :: True :: True :: False :: True :: tail ->
                rule3Score_ tail (score + 40)

        True :: False :: True :: True :: True :: False :: True
            :: False :: False :: False :: False :: tail ->
                rule3Score_ tail (score + 40)

        head :: tail ->
            rule3Score_ tail score


rule4Score : Int -> List Bool -> Int
rule4Score size simplifiedList =
    let
        darkCount =
            List.filter identity simplifiedList
                |> List.length

        moduleCount = toFloat (size * size)

        darkPerc =
            round (toFloat (100 * darkCount) / moduleCount)

        remOf5 =
            rem darkPerc 5

        prevMult5 =
            darkPerc - remOf5
                |> flip (-) 50
                |> abs
                |> toFloat
                |> flip (/) 5
                |> round

        nextMult5 =
            darkPerc + (5 - remOf5)
                |> flip (-) 50
                |> abs
                |> toFloat
                |> flip (/) 5
                |> round

    in
        min prevMult5 nextMult5 * 10


isDarkModule : Maybe Module -> Bool
isDarkModule =
    Maybe.map Tuple.second
        >> Maybe.withDefault False


applyMask : Int -> Mask -> Matrix -> Matrix
applyMask size mask matrix =
    applyMaskFunction (maskFunction mask) size
        |> flip Array.indexedMap matrix


applyMaskFunction : ( ( Int, Int ) -> Bool ) -> Int -> Int -> Maybe Module -> Maybe Module
applyMaskFunction function size index maybeModule =
    getCoord size index
        |> function
        |> applyMaskColor maybeModule


applyMaskColor : Maybe Module -> Bool -> Maybe Module
applyMaskColor maybeModule isChange =
    if isChange then
        case maybeModule of
            Just ( False, isDark ) ->
                Just ( False, not isDark )

            _ ->
                maybeModule

    else
        maybeModule



---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------


getIndex : Int -> Int -> Int -> Int
getIndex size row col =
    (size * row) + col


getCoord : Int -> Int -> ( Int, Int )
getCoord size index =
    ( index // size
    , index % size
    )


isOccupy : Int -> Int -> Int -> Matrix -> Bool
isOccupy row col size matrix =
    case Array.get (getIndex size row col) matrix of
        Just module_ ->
            case module_ of
                Just _ -> True
                _      -> False

        Nothing ->
            False


getAreaCoord : List Int -> List Int -> List ( Int, Int )
getAreaCoord rows cols =
    List.foldl (\row list ->
        List.foldl (\col list_ ->
            ( row, col ) :: list_
        ) list cols
    ) [] rows


getBCHDigit : Int -> Int
getBCHDigit int =
    let
        helper digit int = 
            if int /= 0 then
                helper (digit + 1) (shiftRightZfBy 1 int)

            else
                digit

    in
        helper 0 int



---------------------------------------------------------------------
-- Data
---------------------------------------------------------------------


alignmentPatternData : Array (List Int)
alignmentPatternData =
    [ []
    , [ 6, 18 ]
    , [ 6, 22 ]
    , [ 6, 26 ]
    , [ 6, 30 ]
    , [ 6, 34 ]
    , [ 6, 22, 38 ]
    , [ 6, 24, 42 ]
    , [ 6, 26, 46 ]
    , [ 6, 28, 50 ]
    , [ 6, 30, 54 ]
    , [ 6, 32, 58 ]
    , [ 6, 34, 62 ]
    , [ 6, 26, 46, 66 ]
    , [ 6, 26, 48, 70 ]
    , [ 6, 26, 50, 74 ]
    , [ 6, 30, 54, 78 ]
    , [ 6, 30, 56, 82 ]
    , [ 6, 30, 58, 86 ]
    , [ 6, 34, 62, 90 ]
    , [ 6, 28, 50, 72, 94 ]
    , [ 6, 26, 50, 74, 98 ]
    , [ 6, 30, 54, 78, 102 ]
    , [ 6, 28, 54, 80, 106 ]
    , [ 6, 32, 58, 84, 110 ]
    , [ 6, 30, 58, 86, 114 ]
    , [ 6, 34, 62, 90, 118 ]
    , [ 6, 26, 50, 74, 98, 122 ]
    , [ 6, 30, 54, 78, 102, 126 ]
    , [ 6, 26, 52, 78, 104, 130 ]
    , [ 6, 30, 56, 82, 108, 134 ]
    , [ 6, 34, 60, 86, 112, 138 ]
    , [ 6, 30, 58, 86, 114, 142 ]
    , [ 6, 34, 62, 90, 118, 146 ]
    , [ 6, 30, 54, 78, 102, 126, 150 ]
    , [ 6, 24, 50, 76, 102, 128, 154 ]
    , [ 6, 28, 54, 80, 106, 132, 158 ]
    , [ 6, 32, 58, 84, 110, 136, 162 ]
    , [ 6, 26, 54, 82, 110, 138, 166 ]
    , [ 6, 30, 58, 86, 114, 142, 170 ]
    ] |> Array.fromList

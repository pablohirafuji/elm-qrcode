module QRCode.GroupInfo exposing
    ( GroupInfo
    , getGroupData
    )


import QRCode.ECLevel exposing (ECLevel(..))



getGroupData : ECLevel -> List GroupInfo
getGroupData ecLevel =
    case ecLevel of
        L -> dataL
        M -> dataM
        Q -> dataQ
        H -> dataH


type alias GroupInfo =
    { version     : Int
    , ecPerBlock  : Int -- Error Correction Bytes Per Block
    , group1      : ( Int, Int ) -- Nº of Blocks, Bytes/Block
    , maybeGroup2 : Maybe ( Int, Int ) -- Nº of Blocks, Bytes/Block
    , capacity    : Int -- Bit capacity
    }


newGroupInfo : ( Int, Int, ( Int, Int ), Maybe ( Int, Int ) ) -> GroupInfo
newGroupInfo ( version, ecPerBlock, group1, maybeGroup2 ) =
    { version     = version
    , ecPerBlock  = ecPerBlock
    , group1      = group1
    , maybeGroup2 = maybeGroup2
    , capacity    = byteCapacity group1 maybeGroup2 * 8
    }

byteCapacity : ( Int, Int ) -> Maybe ( Int, Int ) -> Int
byteCapacity group1 maybeGroup2 =
    case maybeGroup2 of
        Just block2 ->
            blockByteCapacity group1 + blockByteCapacity block2

        Nothing ->
            blockByteCapacity group1


blockByteCapacity : ( Int, Int ) -> Int
blockByteCapacity ( blockCount, bytePerBlock ) =
    blockCount * bytePerBlock


dataL : List GroupInfo
dataL =
    [ ( 1, 7, ( 1, 19 ), Nothing )
    , ( 2, 10, ( 1, 34 ), Nothing )
    , ( 3, 15, ( 1, 55 ), Nothing )
    , ( 4, 20, ( 1, 80 ), Nothing )
    , ( 5, 26, ( 1, 108 ), Nothing )
    , ( 6, 18, ( 2, 68 ), Nothing )
    , ( 7, 20, ( 2, 78 ), Nothing )
    , ( 8, 24, ( 2, 97 ), Nothing )
    , ( 9, 30, ( 2, 116 ), Nothing )
    , ( 10, 18, ( 2, 68 ), (Just ( 2, 69 )) )
    , ( 11, 20, ( 4, 81 ), Nothing )
    , ( 12, 24, ( 2, 92 ), (Just ( 2, 93 )) )
    , ( 13, 26, ( 4, 107 ), Nothing )
    , ( 14, 30, ( 3, 115 ), (Just ( 1, 116 )) )
    , ( 15, 22, ( 5, 87 ), (Just ( 1, 88 )) )
    , ( 16, 24, ( 5, 98 ), (Just ( 1, 99 )) )
    , ( 17, 28, ( 1, 107 ), (Just ( 5, 108 )) )
    , ( 18, 30, ( 5, 120 ), (Just ( 1, 121 )) )
    , ( 19, 28, ( 3, 113 ), (Just ( 4, 114 )) )
    , ( 20, 28, ( 3, 107 ), (Just ( 5, 108 )) )
    , ( 21, 28, ( 4, 116 ), (Just ( 4, 117 )) )
    , ( 22, 28, ( 2, 111 ), (Just ( 7, 112 )) )
    , ( 23, 30, ( 4, 121 ), (Just ( 5, 122 )) )
    , ( 24, 30, ( 6, 117 ), (Just ( 4, 118 )) )
    , ( 25, 26, ( 8, 106 ), (Just ( 4, 107 )) )
    , ( 26, 28, ( 10, 114 ), (Just ( 2, 115 )) )
    , ( 27, 30, ( 8, 122 ), (Just ( 4, 123 )) )
    , ( 28, 30, ( 3, 117 ), (Just ( 10, 118 )) )
    , ( 29, 30, ( 7, 116 ), (Just ( 7, 117 )) )
    , ( 30, 30, ( 5, 115 ), (Just ( 10, 116 )) )
    , ( 31, 30, ( 13, 115 ), (Just ( 3, 116 )) )
    , ( 32, 30, ( 17, 115 ), Nothing )
    , ( 33, 30, ( 17, 115 ), (Just ( 1, 116 )) )
    , ( 34, 30, ( 13, 115 ), (Just ( 6, 116 )) )
    , ( 35, 30, ( 12, 121 ), (Just ( 7, 122 )) )
    , ( 36, 30, ( 6, 121 ), (Just ( 14, 122 )) )
    , ( 37, 30, ( 17, 122 ), (Just ( 4, 123 )) )
    , ( 38, 30, ( 4, 122 ), (Just ( 18, 123 )) )
    , ( 39, 30, ( 20, 117 ), (Just ( 4, 118 )) )
    , ( 40, 30, ( 19, 118 ), (Just ( 6, 119 )) )
    ] |> List.map newGroupInfo


dataM : List GroupInfo
dataM =
    [ ( 1, 10, ( 1, 16), Nothing )
    , ( 2, 16, ( 1, 28), Nothing )
    , ( 3, 26, ( 1, 44), Nothing )
    , ( 4, 18, ( 2, 32), Nothing )
    , ( 5, 24, ( 2, 43), Nothing )
    , ( 6, 16, ( 4, 27), Nothing )
    , ( 7, 18, ( 4, 31), Nothing )
    , ( 8, 22, ( 2, 38), (Just ( 2, 39 )) )
    , ( 9, 22, ( 3, 36), (Just ( 2, 37 )) )
    , ( 10, 26, ( 4, 43), (Just ( 1, 44 )) )
    , ( 11, 30, ( 1, 50), (Just ( 4, 51 )) )
    , ( 12, 22, ( 6, 36), (Just ( 2, 37 )) )
    , ( 13, 22, ( 8, 37), (Just ( 1, 38 )) )
    , ( 14, 24, ( 4, 40), (Just ( 5, 41 )) )
    , ( 15, 24, ( 5, 41), (Just ( 5, 42 )) )
    , ( 16, 28, ( 7, 45), (Just ( 3, 46 )) )
    , ( 17, 28, ( 10, 46), (Just ( 1, 47 )) )
    , ( 18, 26, ( 9, 43), (Just ( 4, 44 )) )
    , ( 19, 26, ( 3, 44), (Just ( 11, 45 )) )
    , ( 20, 26, ( 3, 41), (Just ( 13, 42 )) )
    , ( 21, 26, ( 17, 42), Nothing )
    , ( 22, 28, ( 17, 46), Nothing )
    , ( 23, 28, ( 4, 47), (Just ( 14, 48 )) )
    , ( 24, 28, ( 6, 45), (Just ( 14, 46 )) )
    , ( 25, 28, ( 8, 47), (Just ( 13, 48 )) )
    , ( 26, 28, ( 19, 46), (Just ( 4, 47 )) )
    , ( 27, 28, ( 22, 45), (Just ( 3, 46 )) )
    , ( 28, 28, ( 3, 45), (Just ( 23, 46 )) )
    , ( 29, 28, ( 21, 45), (Just ( 7, 46 )) )
    , ( 30, 28, ( 19, 47), (Just ( 10, 48 )) )
    , ( 31, 28, ( 2, 46), (Just ( 29, 47 )) )
    , ( 32, 28, ( 10, 46), (Just ( 23, 47 )) )
    , ( 33, 28, ( 14, 46), (Just ( 21, 47 )) )
    , ( 34, 28, ( 14, 46), (Just ( 23, 47 )) )
    , ( 35, 28, ( 12, 47), (Just ( 26, 48 )) )
    , ( 36, 28, ( 6, 47), (Just ( 34, 48 )) )
    , ( 37, 28, ( 29, 46), (Just ( 14, 47 )) )
    , ( 38, 28, ( 13, 46), (Just ( 32, 47 )) )
    , ( 39, 28, ( 40, 47), (Just ( 7, 48 )) )
    , ( 40, 28, ( 18, 47), (Just ( 31, 48 )) )
    ] |> List.map newGroupInfo


dataQ : List GroupInfo
dataQ =
    [ ( 1, 13, ( 1, 13), Nothing )
    , ( 2, 22, ( 1, 22), Nothing )
    , ( 3, 18, ( 2, 17), Nothing )
    , ( 4, 26, ( 2, 24), Nothing )
    , ( 5, 18, ( 2, 15), (Just ( 2, 16 )) )
    , ( 6, 24, ( 4, 19), Nothing )
    , ( 7, 18, ( 2, 14), (Just ( 4, 15 )) )
    , ( 8, 22, ( 4, 18), (Just ( 2, 19 )) )
    , ( 9, 20, ( 4, 16), (Just ( 4, 17 )) )
    , ( 10, 24, ( 6, 19), (Just ( 2, 20 )) )
    , ( 11, 28, ( 4, 22), (Just ( 4, 23 )) )
    , ( 12, 26, ( 4, 20), (Just ( 6, 21 )) )
    , ( 13, 24, ( 8, 20), (Just ( 4, 21 )) )
    , ( 14, 20, ( 11, 16), (Just ( 5, 17 )) )
    , ( 15, 30, ( 5, 24), (Just ( 7, 25 )) )
    , ( 16, 24, ( 15, 19), (Just ( 2, 20 )) )
    , ( 17, 28, ( 1, 22), (Just ( 15, 23 )) )
    , ( 18, 28, ( 17, 22), (Just ( 1, 23 )) )
    , ( 19, 26, ( 17, 21), (Just ( 4, 22 )) )
    , ( 20, 30, ( 15, 24), (Just ( 5, 25 )) )
    , ( 21, 28, ( 17, 22), (Just ( 6, 23 )) )
    , ( 22, 30, ( 7, 24), (Just ( 16, 25 )) )
    , ( 23, 30, ( 11, 24), (Just ( 14, 25 )) )
    , ( 24, 30, ( 11, 24), (Just ( 16, 25 )) )
    , ( 25, 30, ( 7, 24), (Just ( 22, 25 )) )
    , ( 26, 28, ( 28, 22), (Just ( 6, 23 )) )
    , ( 27, 30, ( 8, 23), (Just ( 26, 24 )) )
    , ( 28, 30, ( 4, 24), (Just ( 31, 25 )) )
    , ( 29, 30, ( 1, 23), (Just ( 37, 24 )) )
    , ( 30, 30, ( 15, 24), (Just ( 25, 25 )) )
    , ( 31, 30, ( 42, 24), (Just ( 1, 25 )) )
    , ( 32, 30, ( 10, 24), (Just ( 35, 25 )) )
    , ( 33, 30, ( 29, 24), (Just ( 19, 25 )) )
    , ( 34, 30, ( 44, 24), (Just ( 7, 25 )) )
    , ( 35, 30, ( 39, 24), (Just ( 14, 25 )) )
    , ( 36, 30, ( 46, 24), (Just ( 10, 25 )) )
    , ( 37, 30, ( 49, 24), (Just ( 10, 25 )) )
    , ( 38, 30, ( 48, 24), (Just ( 14, 25 )) )
    , ( 39, 30, ( 43, 24), (Just ( 22, 25 )) )
    , ( 40, 30, ( 34, 24), (Just ( 34, 25 )) )
    ] |> List.map newGroupInfo


dataH : List GroupInfo
dataH =
    [ ( 1, 17, ( 1, 9), Nothing )
    , ( 2, 28, ( 1, 16), Nothing )
    , ( 3, 22, ( 2, 13), Nothing )
    , ( 4, 16, ( 4, 9), Nothing )
    , ( 5, 22, ( 2, 11), (Just ( 2, 12 )) )
    , ( 6, 28, ( 4, 15), Nothing )
    , ( 7, 26, ( 4, 13), (Just ( 1, 14 )) )
    , ( 8, 26, ( 4, 14), (Just ( 2, 15 )) )
    , ( 9, 24, ( 4, 12), (Just ( 4, 13 )) )
    , ( 10, 28, ( 6, 15), (Just ( 2, 16 )) )
    , ( 11, 24, ( 3, 12), (Just ( 8, 13 )) )
    , ( 12, 28, ( 7, 14), (Just ( 4, 15 )) )
    , ( 13, 22, ( 12, 11), (Just ( 4, 12 )) )
    , ( 14, 24, ( 11, 12), (Just ( 5, 13 )) )
    , ( 15, 24, ( 11, 12), (Just ( 7, 13 )) )
    , ( 16, 30, ( 3, 15), (Just ( 13, 16 )) )
    , ( 17, 28, ( 2, 14), (Just ( 17, 15 )) )
    , ( 18, 28, ( 2, 14), (Just ( 19, 15 )) )
    , ( 19, 26, ( 9, 13), (Just ( 16, 14 )) )
    , ( 20, 28, ( 15, 15), (Just ( 10, 16 )) )
    , ( 21, 30, ( 19, 16), (Just ( 6, 17 )) )
    , ( 22, 24, ( 34, 13), Nothing )
    , ( 23, 30, ( 16, 15), (Just ( 14, 16 )) )
    , ( 24, 30, ( 30, 16), (Just ( 2, 17 )) )
    , ( 25, 30, ( 22, 15), (Just ( 13, 16 )) )
    , ( 26, 30, ( 33, 16), (Just ( 4, 17 )) )
    , ( 27, 30, ( 12, 15), (Just ( 28, 16 )) )
    , ( 28, 30, ( 11, 15), (Just ( 31, 16 )) )
    , ( 29, 30, ( 19, 15), (Just ( 26, 16 )) )
    , ( 30, 30, ( 23, 15), (Just ( 25, 16 )) )
    , ( 31, 30, ( 23, 15), (Just ( 28, 16 )) )
    , ( 32, 30, ( 19, 15), (Just ( 35, 16 )) )
    , ( 33, 30, ( 11, 15), (Just ( 46, 16 )) )
    , ( 34, 30, ( 59, 16), (Just ( 1, 17 )) )
    , ( 35, 30, ( 22, 15), (Just ( 41, 16 )) )
    , ( 36, 30, ( 2, 15), (Just ( 64, 16 )) )
    , ( 37, 30, ( 24, 15), (Just ( 46, 16 )) )
    , ( 38, 30, ( 42, 15), (Just ( 32, 16 )) )
    , ( 39, 30, ( 10, 15), (Just ( 67, 16 )) )
    , ( 40, 30, ( 20, 15), (Just ( 61, 16 )) )
    ] |> List.map newGroupInfo


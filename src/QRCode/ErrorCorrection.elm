module QRCode.ErrorCorrection exposing (get)


import Array exposing (Array)
import Bitwise
import QRCode.Error exposing (Error(..))
import QRCode.Helpers exposing (listResult)



get : Int -> List (List Int) -> Result Error (List (List Int))
get ecPerBlock byteBlocks =
    getECPolynomial ecPerBlock
        |> Result.andThen (get_ byteBlocks)


get_ : List (List Int) -> Polynomial -> Result Error (List (List Int))
get_ byteBlocks rsPoly =
    listResult (get__ rsPoly) [] byteBlocks


get__ : Polynomial -> List Int -> Result Error (List Int)
get__ rsPoly dataCodewords =
    newPolynomial dataCodewords (Array.length rsPoly - 1)
        |> flip mod rsPoly
        |> Result.map (get___ (Array.length rsPoly - 1))


get___ : Int -> Polynomial -> List Int
get___ ecLength modPoly =
    Array.initialize ecLength (\index ->
            let
                modIndex = index + Array.length modPoly - ecLength

            in
                if modIndex >= 0 then
                    Array.get modIndex modPoly
                        |> Maybe.withDefault 0

                else
                    0

        ) |> Array.toList



---------------------------------------------------------------------
-- Polynomial
---------------------------------------------------------------------


type alias Polynomial = Array Int


getECPolynomial : Int -> Result Error Polynomial
getECPolynomial ecLength =
    let
        generate count polyResult =
            if count < ecLength then
                polyResult
                    |> Result.andThen (flip multiply
                        (newPolynomial [ 1, getExp count ] 0))
                    |> generate (count + 1)

            else
                polyResult

    in
        Result.Ok (newPolynomial [1] 0)
            |> generate 0


newPolynomial : List Int -> Int -> Polynomial
newPolynomial num shift =
    let
        offset = getOffset ( num, 0 )
        numArray = Array.fromList num

    in
        Array.initialize
            (List.length num - offset + shift)
            (\index ->
                Array.get (index + offset) numArray
                    |> Maybe.withDefault 0
            )


getOffset : ( List Int, Int ) -> Int
getOffset ( num, offset ) =
    case num of
        head :: tail ->
            if head == 0
                then getOffset ( tail, offset + 1 )
                else offset

        [] -> offset


multiply : Polynomial -> Polynomial -> Result Error Polynomial
multiply poly1 poly2 =
    let
        num =
            Array.initialize
                (Array.length poly1 + Array.length poly2 - 1)
                (always 0)

        valuesArray =
            List.indexedMap
                (\index1 value1 ->
                    List.indexedMap
                        (\index2 value2 ->
                            ( index1 + index2, value1, value2 )
                        ) (Array.toList poly2)
                ) (Array.toList poly1)

        process args numResult =
            Result.andThen (process_ args) numResult
        
        process_ ( indexSum, value1, value2 ) num_ =
            Result.map2 (+) (getLog value1) (getLog value2)
                |> Result.map getExp
                |> Result.andThen (process__ indexSum num_)
                |> Result.map (\r -> Array.set indexSum r num_)

        process__ indexSum num_ exp =
            Array.get indexSum num_
                |> Maybe.map (Bitwise.xor exp)
                |> Result.fromMaybe PolynomialMultiplyException

    in
        valuesArray
            |> List.concat
            |> List.foldl process (Result.Ok num)
            |> Result.map Array.toList
            |> Result.map (flip newPolynomial 0)


mod : Polynomial -> Polynomial -> Result Error Polynomial
mod poly1 poly2 =
    if Array.length poly1 - Array.length poly2 < 0 then
        Result.Ok poly1

    else
        let
            getHead poly =
                Array.get 0 poly
                    |> Result.fromMaybe PolynomialModException
                    |> Result.andThen getLog

            ratio =
                Result.map2 (-) (getHead poly1) (getHead poly2)

            numResult =
                Array.indexedMap (,) poly2
                    |> Array.foldl numFold (Result.Ok poly1)

            numFold args poly1Result =
                Result.andThen (helper args) poly1Result

            helper ( index2, value2 ) poly1_ =
                getLog value2
                    |> Result.map2 (+) ratio
                    |> Result.map getExp
                    |> Result.andThen (helper_ index2 poly1_)
                    |> Result.map (\r -> Array.set index2 r poly1_)

            helper_ index2 poly1_ exp =
                Array.get index2 poly1_
                    |> Maybe.map (Bitwise.xor exp)
                    |> Result.fromMaybe PolynomialModException

        in
            numResult
                |> Result.map Array.toList
                |> Result.map (flip newPolynomial 0)
                |> Result.andThen (flip mod poly2)


expTable : Array Int
expTable =
    [1,2,4,8,16,32,64,128,29,58,116,232,205,135,19,38,76,152,45,90,180,117,234,201,143,3,6,12,24,48,96,192,157,39,78,156,37,74,148,53,106,212,181,119,238,193,159,35,70,140,5,10,20,40,80,160,93,186,105,210,185,111,222,161,95,190,97,194,153,47,94,188,101,202,137,15,30,60,120,240,253,231,211,187,107,214,177,127,254,225,223,163,91,182,113,226,217,175,67,134,17,34,68,136,13,26,52,104,208,189,103,206,129,31,62,124,248,237,199,147,59,118,236,197,151,51,102,204,133,23,46,92,184,109,218,169,79,158,33,66,132,21,42,84,168,77,154,41,82,164,85,170,73,146,57,114,228,213,183,115,230,209,191,99,198,145,63,126,252,229,215,179,123,246,241,255,227,219,171,75,150,49,98,196,149,55,110,220,165,87,174,65,130,25,50,100,200,141,7,14,28,56,112,224,221,167,83,166,81,162,89,178,121,242,249,239,195,155,43,86,172,69,138,9,18,36,72,144,61,122,244,245,247,243,251,235,203,139,11,22,44,88,176,125,250,233,207,131,27,54,108,216,173,71,142,1]
        |> Array.fromList


getExp : Int -> Int
getExp index =
    Array.get (index % 255) expTable
        |> Maybe.withDefault 0


logTable : Array Int
logTable =
    [0,1,25,2,50,26,198,3,223,51,238,27,104,199,75,4,100,224,14,52,141,239,129,28,193,105,248,200,8,76,113,5,138,101,47,225,36,15,33,53,147,142,218,240,18,130,69,29,181,194,125,106,39,249,185,201,154,9,120,77,228,114,166,6,191,139,98,102,221,48,253,226,152,37,179,16,145,34,136,54,208,148,206,143,150,219,189,241,210,19,92,131,56,70,64,30,66,182,163,195,72,126,110,107,58,40,84,250,133,186,61,202,94,155,159,10,21,121,43,78,212,229,172,115,243,167,87,7,112,192,247,140,128,99,13,103,74,222,237,49,197,254,24,227,165,153,119,38,184,180,124,17,68,146,217,35,32,137,46,55,63,209,91,149,188,207,205,144,135,151,178,220,252,190,97,242,86,211,171,20,42,93,158,132,60,57,83,71,109,65,162,31,45,67,216,183,123,164,118,196,23,73,236,127,12,111,246,108,161,59,82,41,157,85,170,251,96,134,177,187,204,62,90,203,89,95,176,156,169,160,81,11,245,22,235,122,117,44,215,79,174,213,233,230,231,173,232,116,214,244,234,168,80,88,175]
        |> Array.fromList


getLog : Int -> Result Error Int
getLog index =
    if index < 1 then
        Result.Err (LogTableException index)

    else
        Array.get (index - 1) logTable
            |> Result.fromMaybe (LogTableException index)

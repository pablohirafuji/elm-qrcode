module Main exposing (..)

import Benchmark.Runner exposing (BenchmarkProgram, program)

import Benchmark exposing (Benchmark, describe)
import QRCode


main : BenchmarkProgram
main =
    program qrCodeBench


qrCodeBench : Benchmark
qrCodeBench =
    [ Benchmark.benchmark1 "Byte" QRCode.toSvg byte
    , Benchmark.benchmark1 "Alphanumeric" QRCode.toSvg alpha
    , Benchmark.benchmark1 "Numeric" QRCode.toSvg numeric
    ] |> Benchmark.describe "QRCode"


numeric : String
numeric =
    "65465465465"


alpha : String
alpha =
    "ELM QR CODE"


byte : String
byte =
    "Elm QR Code"



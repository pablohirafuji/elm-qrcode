module QRCode.Error exposing (Error(..))

{-| # Error

@docs Error
-}


{-| Possible errors.
-}


type Error
    = AlignmentPatternNotFound
    | InvalidNumericChar
    | InvalidAlphanumericChar
    | InvalidUTF8Char
    | InvalidBinaryConversion
    | LogTableException Int
    | PolynomialMultiply
    | PolynomialMod
    | InputLengthOverflow

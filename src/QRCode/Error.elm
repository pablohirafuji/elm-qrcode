module QRCode.Error exposing (Error(..))

{-|
@docs Error
-}


{-| Possible errors.
-}

type Error
    = AlignmentPatternNotFound
    | InvalidNumericChar
    | InvalidAlphanumericChar
    | InvalidUTF8Char
    | LogTableException Int
    | PolynomialMultiplyException
    | PolynomialModException
    | InputLengthOverflow

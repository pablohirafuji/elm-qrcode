module QRCode.Error exposing (Error(..))


type Error
    = AlignmentPatternNotFound
    | InvalidNumericChar
    | InvalidAlphanumericChar
    | InvalidUTF8Char
    | LogTableException Int
    | PolynomialMultiplyException
    | PolynomialModException
    | InputLengthOverflow

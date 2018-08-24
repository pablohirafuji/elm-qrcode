module QRCode exposing
    ( QRCode, ErrorCorrection(..)
    , encode, encodeWith
    , toSvg, toString
    , Error(..)
    )

{-| QR Code encoding and rendering.

@docs QRCode, ErrorCorrection


# Encoding

@docs encode, encodeWith


# Rendering

@docs toSvg, toString


# Error

@docs Error

-}

import Html exposing (Html)
import QRCode.ECLevel as ECLevel exposing (ECLevel)
import QRCode.Encode as Encode
import QRCode.Error as Error
import QRCode.Matrix as Matrix exposing (Model)
import QRCode.Render.String as String_
import QRCode.Render.Svg as Svg


{-| QRCode type.
-}
type QRCode
    = QRCode (List (List Bool))


{-| Error correction level. Provides the following error
correction capability:

  - **Low**: 7% of codewords can be restored.
  - **Medium**: 15% of codewords can be restored.
  - **Quartile**: 25% of codewords can be restored.
  - **High**: 30% of codewords can be restored.

-}
type ErrorCorrection
    = Low
    | Medium
    | Quartile
    | High


{-| Transform a string into a result [Error](#Error)
or a [QRCode](#QRCode) using `Quartile` [ErrorCorrection](#ErrorCorrection).
-}
encode : String -> Result Error QRCode
encode =
    encodeWith Quartile


{-| Transform a string with a given [ErrorCorrection](#ErrorCorrection)
into a result [Error](#Error) or a [QRCode](#QRCode).
-}
encodeWith : ErrorCorrection -> String -> Result Error QRCode
encodeWith ecLevel input =
    convertEC ecLevel
        |> Encode.encode input
        |> Result.andThen Matrix.apply
        |> Result.map QRCode
        |> Result.mapError convertError


convertEC : ErrorCorrection -> ECLevel
convertEC ec =
    case ec of
        Low ->
            ECLevel.L

        Medium ->
            ECLevel.M

        Quartile ->
            ECLevel.Q

        High ->
            ECLevel.H


{-| Transform a [QRCode](#QRCode) into a svg element.

    qrCodeView : String -> Html msg
    qrCodeView message =
        QRCode.encode message
            |> Result.map QRCode.toSvg
            |> Result.withDefault
                (Html.text "Error while encoding to QRCode.")

**Tip**: You can determine the size of the generated svg setting
`width` and `height` styles.

-}
toSvg : QRCode -> Html msg
toSvg (QRCode qrCode) =
    Svg.view qrCode


{-| Transform a [QRCode](#QRCode) into a string.

    "Hello World!"
        |> QRCode.encode
        |> Result.map QRCode.toString
        |> Result.withDefault "Error while encoding to QRCode."

Returns:

    ■■■■■■■ ■■■■  ■■■■■■■
    ■     ■ ■■■■  ■     ■
    ■ ■■■ ■ ■■■■■ ■ ■■■ ■
    ■ ■■■ ■ ■   ■ ■ ■■■ ■
    ■ ■■■ ■ ■■■■  ■ ■■■ ■
    ■     ■    ■  ■     ■
    ■■■■■■■ ■ ■ ■ ■■■■■■■
            ■ ■■
     ■■ ■ ■■  ■   ■ ■■■■■
    ■■ ■■■   ■■■■ ■■■  ■■
     ■ ■■ ■■ ■■■■■ ■■■■■■
     ■■      ■■■■   ■  ■
    ■  ■ ■■■  ■■■■ ■■
            ■■ ■■■    ■■
    ■■■■■■■ ■ ■    ■■ ■■■
    ■     ■  ■  ■  ■    ■
    ■ ■■■ ■ ■■■     ■
    ■ ■■■ ■   ■■  ■■■ ■■
    ■ ■■■ ■ ■ ■ ■ ■ ■ ■ ■
    ■     ■ ■  ■    ■  ■
    ■■■■■■■   ■■■  ■   ■■

-}
toString : QRCode -> String
toString (QRCode qrCode) =
    String_.view qrCode


{-| Possible enconding errors.
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


convertError : Error.Error -> Error
convertError e =
    case e of
        Error.AlignmentPatternNotFound ->
            AlignmentPatternNotFound

        Error.InvalidNumericChar ->
            InvalidNumericChar

        Error.InvalidAlphanumericChar ->
            InvalidAlphanumericChar

        Error.InvalidUTF8Char ->
            InvalidUTF8Char

        Error.LogTableException n ->
            LogTableException n

        Error.PolynomialMultiplyException ->
            PolynomialMultiplyException

        Error.PolynomialModException ->
            PolynomialModException

        Error.InputLengthOverflow ->
            InputLengthOverflow

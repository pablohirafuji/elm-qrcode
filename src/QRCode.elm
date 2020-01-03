module QRCode exposing
    ( QRCode, ErrorCorrection(..)
    , encode, encodeWith
    , toSvg, toSvgWithoutQuietZone, toString, toMatrix, toImage, toImageWithOptions, ImageOptions, defaultImageOptions
    , Error(..)
    )

{-| QR Code encoding and rendering.

@docs QRCode, ErrorCorrection


# Encoding

@docs encode, encodeWith


# Rendering

@docs toSvg, toSvgWithoutQuietZone, toString, toImage, toImageWithOptions, ImageOptions, defaultImageOptions


# Error

@docs Error

-}

import Html exposing (Html)
import Image exposing (Image)
import QRCode.ECLevel as ECLevel exposing (ECLevel)
import QRCode.Encode as Encode
import QRCode.Error as Error
import QRCode.Matrix as Matrix exposing (Model)
import QRCode.Render.Raster as Raster
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
    Result.mapError convertError
        (Result.map QRCode
            (Result.andThen Matrix.apply
                (Encode.encode input (convertEC ecLevel))
            )
        )


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


{-| Same as [toSvg](#toSvg), but without the [quiet zone](https://en.wikipedia.org/wiki/QR_code#/media/File:QR_Code_Structure_Example_3.svg).
-}
toSvgWithoutQuietZone : QRCode -> Html msg
toSvgWithoutQuietZone (QRCode qrCode) =
    Svg.viewWithoutQuietZone qrCode


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

{-| Transform a [QRCode](#QRCode) into a list of list of booleans.

    "Hello World!"
        |> QRCode.encode
        |> Result.map QRCode.toMatrix
        |> Result.withDefault "Error while encoding to QRCode."

-}
toMatrix : QRCode -> List (List Bool)
toMatrix (QRCode qrCode) =
    qrCode


{-| Transform a [QRCode](#QRCode) into an [Image](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#Image). You can transform the Image into a [PNG](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#toPngUrl) or [BMP](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#toBmpUrl).

    import Html
    import Html.Attribute
    import Image
    import QRCode

    viewQRCode : String -> Html msg
    viewQRCode message =
        QRCode.encode message
            |> Result.map
                (\qrCode ->
                    Html.img
                        [ QRCode.toImage qrCode
                            |> Image.toPngUrl
                            |> Html.Attribute.src
                        ]
                        []
                )
            |> Result.withDefault
                (Html.text "Error while encoding to QRCode.")

**Note**: You must install the [`justgook/elm-image`](https://package.elm-lang.org/packages/justgook/elm-image/latest) package in order to use the functions to convert the `Image` type to something else:

```sh
elm install "justgook/elm-image"
```

-}
toImage : QRCode -> Image
toImage =
    toImageWithOptions defaultImageOptions


{-| Transform a [QRCode](#QRCode) into an [Image](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#Image) with an [ImageOptions](#ImageOptions).
-}
toImageWithOptions : ImageOptions -> QRCode -> Image
toImageWithOptions config (QRCode qrCode) =
    Raster.toImageWithOptions config qrCode


{-| Available options to transform a [QRCode](#QRCode) into an [Image](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#Image) with [toImageWithOptions](#toImageWithOptions).

  - `moduleSize` is the size of the module (the dark square) in px;
  - `moduleColor` and `emptyColor` expects an `Int` as `0xRRGGBBAA`;
  - `quietZoneSize` is the number of modules the [quiet zone](https://en.wikipedia.org/wiki/QR_code#Standards) should have.

-}
type alias ImageOptions =
    { moduleSize : Int
    , moduleColor : Int
    , emptyColor : Int
    , quietZoneSize : Int
    }


{-| Default options used by [toImage](#toImage).

    defaultImageOptions =
        { moduleSize = 5
        , moduleColor = 0xFF
        , emptyColor = 0xFFFFFFFF
        , quietZoneSize = 4
        }

-}
defaultImageOptions : ImageOptions
defaultImageOptions =
    { moduleSize = 5
    , moduleColor = 0xFF
    , emptyColor = 0xFFFFFFFF
    , quietZoneSize = 4
    }


{-| Possible encoding errors.
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

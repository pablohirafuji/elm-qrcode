module QRCode exposing
    ( QRCode, ErrorCorrection(..)
    , fromString, fromStringWith
    , toSvg, toSvgWithoutQuietZone, toImage, toImageWithOptions, ImageOptions, defaultImageOptions
    , toMatrix, version
    , Error(..)
    )

{-| QR Code encoding and rendering.

@docs QRCode, ErrorCorrection


# Encoding

@docs fromString, fromStringWith


# Rendering

@docs toSvg, toSvgWithoutQuietZone, toImage, toImageWithOptions, ImageOptions, defaultImageOptions


# Extracting

@docs toMatrix, version


# Error

@docs Error

-}

import Html exposing (Html)
import Image exposing (Image)
import QRCode.ECLevel as ECLevel exposing (ECLevel)
import QRCode.Encode as Encode
import QRCode.Error as Error
import QRCode.GroupInfo exposing (GroupInfo)
import QRCode.Matrix as Matrix exposing (Model)
import QRCode.Render.Raster as Raster
import QRCode.Render.String as String_
import QRCode.Render.Svg as Svg_
import Svg


{-| QRCode type.
-}
type QRCode
    = QRCode
        { version : Int
        , matrix : List (List Bool)
        }


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
fromString : String -> Result Error QRCode
fromString =
    fromStringWith Quartile


{-| Transform a string with a given [ErrorCorrection](#ErrorCorrection)
into a result [Error](#Error) or a [QRCode](#QRCode).
-}
fromStringWith : ErrorCorrection -> String -> Result Error QRCode
fromStringWith ecLevel input =
    Encode.encode input (convertEC ecLevel)
        |> Result.andThen
            (\( encodeModel, encodedData ) ->
                Matrix.apply ( encodeModel, encodedData )
                    |> Result.map
                        (\matrix ->
                            QRCode
                                { version = encodeModel.groupInfo.version
                                , matrix = matrix
                                }
                        )
            )
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

    import Svg.Attributes as SvgA

    qrCodeView : String -> Html msg
    qrCodeView message =
        QRCode.fromString message
            |> Result.map
                (QRCode.toSvg
                    [ SvgA.width "500px"
                    , SvgA.height "500px"
                    ]
                )
            |> Result.withDefault
                (Html.text "Error while encoding to QRCode.")

**Beware**: You **must** set some width and height to render anything.
You can set it by defining `width` and `height` svg's CSS properties or
by passing the `Svg.Attributes.width` and `Svg.Attributes.height`
svg attributes.

-}
toSvg : List (Svg.Attribute msg) -> QRCode -> Html msg
toSvg extraAttrs (QRCode { matrix }) =
    Svg_.view extraAttrs matrix


{-| Same as [toSvg](#toSvg), but without the [quiet zone](https://en.wikipedia.org/wiki/QR_code#/media/File:QR_Code_Structure_Example_3.svg).
-}
toSvgWithoutQuietZone : List (Svg.Attribute msg) -> QRCode -> Html msg
toSvgWithoutQuietZone extraAttrs (QRCode { matrix }) =
    Svg_.viewWithoutQuietZone extraAttrs matrix


{-| Transform a [QRCode](#QRCode) into an [Image](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#Image). You can transform the Image into a [PNG](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#toPngUrl) or [BMP](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#toBmpUrl).

    import Html
    import Html.Attribute
    import Image
    import QRCode

    viewQRCode : String -> Html msg
    viewQRCode message =
        QRCode.fromString message
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
toImageWithOptions config (QRCode { matrix }) =
    Raster.toImageWithOptions config matrix


{-| Available options to transform a [QRCode](#QRCode) into an [Image](https://package.elm-lang.org/packages/justgook/elm-image/latest/Image#Image) with [toImageWithOptions](#toImageWithOptions).

  - `moduleSize` is the size of the module (the QRCode "pixel") in px;
  - `darkColor` and `lightColor` expects an `Int` as `0xRRGGBBAA`;
  - `quietZoneSize` is the number of modules the [quiet zone](https://en.wikipedia.org/wiki/QR_code#Standards) should have.

-}
type alias ImageOptions =
    { moduleSize : Int
    , darkColor : Int
    , lightColor : Int
    , quietZoneSize : Int
    }


{-| Default options used by [toImage](#toImage).

    defaultImageOptions =
        { moduleSize = 5
        , darkColor = 0xFF
        , lightColor = 0xFFFFFFFF
        , quietZoneSize = 4
        }

-}
defaultImageOptions : ImageOptions
defaultImageOptions =
    { moduleSize = 5
    , darkColor = 0xFF
    , lightColor = 0xFFFFFFFF
    , quietZoneSize = 4
    }


{-| Transform a [QRCode](#QRCode) into a list of list of booleans.

    "Hello World!"
        |> QRCode.fromString
        |> Result.map QRCode.toMatrix

-}
toMatrix : QRCode -> List (List Bool)
toMatrix (QRCode { matrix }) =
    matrix


{-| Get the [version](https://www.qrcode.com/en/about/version.html) of the [QRCode](#QRCode).

    "Hello World!"
        |> QRCode.fromString
        |> Result.map QRCode.version
        -- (==) Ok 1

-}
version : QRCode -> Int
version (QRCode r) =
    r.version


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

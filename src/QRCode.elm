module QRCode
    exposing
        ( QRCode
        , encode
        , encodeWithECLevel
        , toSvg
        , toString
        , toCanvas
        , toCanvasWithModuleSize
        , toCanvasWithAbsoluteSize
        , toBoolList
        , fromBoolList
        )

{-| QR Code encoding and rendering.

@docs QRCode


# Encoding

@docs encode, encodeWithECLevel


# Rendering

@docs toCanvas, toCanvasWithModuleSize, toCanvasWithAbsoluteSize, toSvg, toString


# Utilities

@docs toBoolList, fromBoolList

-}

import Html exposing (Html)
import QRCode.ECLevel exposing (ECLevel)
import QRCode.Encode as Encode
import QRCode.Matrix as Matrix exposing (Model)
import QRCode.Error exposing (Error)
import QRCode.Render.Canvas as Canvas
import QRCode.Render.Svg as Svg
import QRCode.Render.String as String_


{-| QRCode type.
-}
type QRCode
    = QRCode (List (List Bool))


{-| Transform a string into a result [Error](./QRCode-Error#Error)
or a [QRCode](#QRCode) using [`ECLevel.Q`](./QRCode-ECLevel#ECLevel)
(25% of codewords can be restored).
-}
encode : String -> Result Error QRCode
encode input =
    encodeWithECLevel input QRCode.ECLevel.Q


{-| Transform a string with a given [EClevel](./QRCode-ECLevel#ECLevel)
into a result [Error](./QRCode-Error#Error) or a [QRCode](#QRCode).
-}
encodeWithECLevel : String -> ECLevel -> Result Error QRCode
encodeWithECLevel input ecLevel =
    Encode.encode input ecLevel
        |> Result.andThen Matrix.apply
        |> Result.map QRCode


{-| Transform a QRCode into a canvas element. The default module size is 5px.

    qrCodeView : String -> Html msg
    qrCodeView message =
        QRCode.encode message
            |> Result.map QRCode.toCanvas
            |> Result.withDefault
                (Html.text "Error while encoding to QRCode.")

-}
toCanvas : QRCode -> Html msg
toCanvas (QRCode qrCode) =
    Canvas.view qrCode


{-| Transform a QRCode into a canvas element with a given module size in pixels.
-}
toCanvasWithModuleSize : Int -> QRCode -> Html msg
toCanvasWithModuleSize moduleSize (QRCode qrCode) =
    Canvas.viewWithModuleSize moduleSize qrCode


{-| Transform a QRCode into a canvas element with a given canvas size in pixels.
-}
toCanvasWithAbsoluteSize : Int -> QRCode -> Html msg
toCanvasWithAbsoluteSize canvasSize (QRCode qrCode) =
    Canvas.viewWithAbsoluteSize canvasSize qrCode


{-| Transform a QRCode into a svg element.

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


{-| Transform a QRCode into a string.

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


{-| Transform a QRCode into a list of list of bools. The list of bools are the rows. `True` represents a dark module, `False` a light one.
-}
toBoolList : QRCode -> List (List Bool)
toBoolList (QRCode qrCode) =
    qrCode


{-| Transform a list of list of bools into a QRCode.
-}
fromBoolList : List (List Bool) -> QRCode
fromBoolList ns =
    QRCode ns

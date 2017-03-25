module QRCode exposing (toSvg, toSvgWithECLevel)

{-| QR Code encoder and renderer.

# Rendering
@docs toSvg, toSvgWithECLevel

-}

import Html exposing (Html)
import QRCode.ECLevel exposing (ECLevel(..))
import QRCode.Encode as Encode
import QRCode.Matrix as Matrix exposing (Model)
import QRCode.Error exposing (Error)
import QRCode.View as View



toMatrix : String -> ECLevel -> Result Error Model
toMatrix inputStr ecLevel =
    Encode.encode inputStr ecLevel
        |> Result.andThen Matrix.apply


{-| Transform a string into a result [Error](./QRCode-Error#Error)
or a QR Code svg element using [`ECLevel.Q`](./QRCode-ECLevel#ECLevel)
(25% of codewords can be restored).

```
qrCode : Html msg
qrCode =
    let
        resultQRCode = toSvg "Hello world"

    in
        case resultQRCode of
            Result.Ok view -> view
            Result.Err err -> Html.text (toString err)
```

**Tip**: You can determine the size of the generated svg setting
`width` and `height` styles.
-}
toSvg : String -> Result Error (Html msg)
toSvg inputStr =
    toMatrix inputStr Q
        |> Result.map View.toSvg


{-| Transform a string with a given [EClevel](./QRCode-ECLevel#ECLevel)
into a result [Error](./QRCode-Error#Error) or a QR Code svg element.
-}
toSvgWithECLevel : String -> ECLevel -> Result Error (Html msg)
toSvgWithECLevel inputStr ecLevel =
    toMatrix inputStr ecLevel
        |> Result.map View.toSvg


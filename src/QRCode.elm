module QRCode exposing (toSvg, toSvg2)

{-| QR Code encoder and renderer.

# Rendering
@docs toSvg, toSvg2

-}

import Html exposing (Html)
import QRCode.Encode as Encode exposing (ECLevel)
import QRCode.Matrix as Matrix exposing (Model)
import QRCode.Error exposing (Error)
import QRCode.View as View


toMatrix : String -> ECLevel -> Result Error Model
toMatrix inputStr ecLevel =
    Encode.encode inputStr ecLevel
        |> Result.andThen Matrix.apply


{-| Transform a string into a result Error or svg element.

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
    toMatrix inputStr Encode.Q
        |> Result.map View.toSvg


{-| Transform a string with given EC level into a result Error or svg element.
-}
toSvg2 : String -> ECLevel -> Result Error (Html msg)
toSvg2 inputStr ecLevel =
    toMatrix inputStr ecLevel
        |> Result.map View.toSvg

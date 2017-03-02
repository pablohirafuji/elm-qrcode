# QR Code

QR Code encoder and renderer. [Demo](https://pablohirafuji.github.io/elm-qrcode/).

## Basic Usage

```
import QRCode

qrCode : Html msg
qrCode =
    let
        resultQRCode = QRCode.toSvg "Hello world"

    in
        case resultQRCode of
            Result.Ok view -> view
            Result.Err err -> Html.text (toString err)
```

If you would like more functionalities, please open an issue on github.


## Thanks

Thank you Carolyn Eby, for creating [this great tutorial](http://www.thonky.com/qr-code-tutorial/) on how QR Code works.

Thank you Evan for bringing joy to the frontend.

# QR Code

QR Code encoding and rendering. [Demo](https://pablohirafuji.github.io/elm-qrcode/).

## Basic Usage

```elm
import QRCode

qrCodeView : String -> Html msg
qrCodeView message =
    QRCode.encode message
        |> Result.map QRCode.toSvg
        |> Result.withDefault
            (Html.text "Error while encoding to QRCode.")

```

If you would like more functionalities, please open an issue on github.


## Changelog

- **2.1.0**:
    - Add `QRCode.toCanvas`
- **2.0.0**:
    - *TL;DR*: Separate encoding from rendering, add string rendering.
    - Add `QRCode.encode`;
    - Add `QRCode.encodeWithECLevel`;
    - Add `QRCode.toString`;
    - Change `QRCode.toSvg`;
    - Change `QRCode.Error.Error`;
    - Remove `QRCode.toSvgWithECLevel`;
- **1.1.0**
    - Add `toSvgWithECLevel`;
    - Expose `ECLevel`;
    - Thanks again @joshmh!
- **1.0.2**
    - Fix bit to byte [conversion bug](https://github.com/pablohirafuji/elm-qrcode/issues/1). Thanks @joshmh!
- **1.0.1**
	- Refactored encoder to use bitwise operations instead of string manipulation;
	- Refactored matrix to use bitwise operations instead of string manipulation;
	- Dropped ParseInt dependence;
	- Perfomance improvements.


## Thanks

Thank you Carolyn Eby, for creating [this great tutorial](http://www.thonky.com/qr-code-tutorial/) on how QR Code works.

Thank you Evan for bringing joy to the frontend.

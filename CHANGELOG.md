# Changelog


### 4.0.0

- Don't set the `width` and `height` of the svg (Thanks @uebayasi!)
- Modify `toSvg` and `toSvgWithoutQuietZone` to accept `List (Svg.Attribute msg)` - Fix [#9](https://github.com/pablohirafuji/elm-qrcode/issues/9) (Thanks @MartinSStewart!)
- Rename `encode` -> `fromString`
- Rename `encodeWith` -> `fromStringWith`
- Rename `moduleColor` -> `darkColor`
- Rename `emptyColor` -> `lightColor`
- Add `version`
- Remove `toString`


### 3.3.0

- Add `toMatrix` (Thanks @pravdomil!)


### 3.2.0

Use https://package.elm-lang.org/packages/justgook/elm-image/latest package to add raster images rendering option.

- Add `toImage`
- Add `toImageWithOptions`
- Add `ImageOptions`
- Add `defaultImageOptions`


### 3.1.1

Remove pipelines from the code because of [this issue](https://github.com/elm/compiler/issues/1770).


### 3.1.0

Add `toSvgWithoutQuietZone` rendering option (Thanks @ahstro for the idea!)


### 3.0.1

Add `shape-rendering` to svg rendering (Thanks @ahstro!)


## 3.0.0

Update to 0.19

- Move `QRCode.ECLevel.ECLevel` type to `QRCode`
- Rename `ECLevel` to `ErrorCorrection`
- Rename `L` to `Low`, `M` to `Medium`, `Q` to `Quartile` and `H` to `High` of `ErrorCorrection` type
- Rename `encodeWithECLevel` to `encodeWith`
- Move `QRCode.Error.Error` type to `QRCode`
- Remove `toCanvasWithModuleSize`*
- Remove `toCanvasWithAbsoluteSize`*
- Remove `toBoolList`**
- Remove `fromBoolList`**


* Waiting for a `Canvas` package in 0.19 to add `toCanvasWithModuleSize` and `toCanvasWithAbsoluteSize` again.

** Removed `toBoolList` and `fromBoolList` to avoid invalid QR Codes to be rendered. If one day a decoder is added, the bool list can be validated and these functions can be added again.

### 2.2.1

Fix issue [#4](https://github.com/pablohirafuji/elm-qrcode/issues/4).


### 2.2.0

Add canvas sizing and utilities.

- Add `toCanvasWithModuleSize`
- Add `toCanvasWithAbsoluteSize`
- Add `toBoolList`
- Add `fromBoolList`


### 2.1.0

Add `QRCode.toCanvas`


## 2.0.0

Separate encoding from rendering, add string rendering.

- Add `QRCode.encode`
- Add `QRCode.encodeWithECLevel`
- Add `QRCode.toString`
- Change `QRCode.toSvg`
- Change `QRCode.Error.Error`
- Remove `QRCode.toSvgWithECLevel`

### 1.1.0

- Add `toSvgWithECLevel`
- Expose `ECLevel`

Thanks again @joshmh!

### 1.0.2

Fix bit to byte [conversion bug](https://github.com/pablohirafuji/elm-qrcode/issues/1). Thanks @joshmh!

### 1.0.1

- Refactor encoder to use bitwise operations instead of string manipulation
- Refactor matrix to use bitwise operations instead of string manipulation
- Drop `ParseInt` dependence
- Perfomance improvements

## 1.0.0

Initial release.

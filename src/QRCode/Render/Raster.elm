module QRCode.Render.Raster exposing (toImageWithOptions)

import Image exposing (Image)


type alias Options =
    { moduleSize : Int
    , moduleColor : Int
    , emptyColor : Int
    , quietZoneSize : Int
    }


toImageWithOptions : Options -> List (List Bool) -> Image
toImageWithOptions options matrix =
    let
        moduleSize =
            max 1 options.moduleSize
    in
    matrix
        |> addQuietZone options.quietZoneSize
        |> List.map
            (List.map
                (moduleToPixel options
                    >> List.repeat moduleSize
                )
                >> List.concat
                >> List.repeat moduleSize
            )
        |> List.concat
        |> Image.fromList2d


addQuietZone : Int -> List (List Bool) -> List (List Bool)
addQuietZone quietZoneSize matrix =
    if quietZoneSize <= 0 then
        matrix

    else
        let
            matrixWithQZLength =
                List.length matrix + (2 * quietZoneSize)

            rows =
                List.repeat
                    quietZoneSize
                    (List.repeat matrixWithQZLength False)

            cols =
                List.repeat quietZoneSize False
        in
        rows
            ++ List.map
                (\row ->
                    cols
                        ++ row
                        ++ cols
                )
                matrix
            ++ rows


moduleToPixel : Options -> Bool -> Image.Pixel
moduleToPixel options isDark =
    if isDark then
        options.moduleColor

    else
        options.emptyColor

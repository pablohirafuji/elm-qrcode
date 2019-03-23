module QRCode.Render.Svg exposing (view, viewWithoutQuietZone)

import Html exposing (Html)
import QRCode.Matrix as Matrix
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, shapeRendering, viewBox, width, x, y)


moduleSize : Int
moduleSize =
    5


view : Matrix.Model -> Html msg
view matrix =
    viewBase 4 matrix


viewWithoutQuietZone : Matrix.Model -> Html msg
viewWithoutQuietZone matrix =
    viewBase 0 matrix


viewBase : Int -> Matrix.Model -> Html msg
viewBase quietZoneSize matrix =
    let
        quietZone =
            (2 * quietZoneSize) * moduleSize

        sizePx =
            String.fromInt (List.length matrix * moduleSize + quietZone)
    in
    matrix
        |> List.indexedMap
            (\rowIndex row ->
                List.indexedMap (moduleView quietZoneSize rowIndex) row
            )
        |> List.concat
        |> List.filterMap identity
        |> svg
            [ width sizePx
            , height sizePx
            , viewBox ("0 0 " ++ sizePx ++ " " ++ sizePx)
            , shapeRendering "crispEdges"
            ]


moduleView : Int -> Int -> Int -> Bool -> Maybe (Html msg)
moduleView quietZoneSize rowIndex colIndex isDark =
    if isDark then
        Just (rectView (rowIndex + quietZoneSize) (colIndex + quietZoneSize))

    else
        Nothing


rectView : Int -> Int -> Html msg
rectView row col =
    rect
        [ y (String.fromInt (row * moduleSize))
        , x (String.fromInt (col * moduleSize))
        , width (String.fromInt moduleSize)
        , height (String.fromInt moduleSize)
        , fill "black"
        ]
        []

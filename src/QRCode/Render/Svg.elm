module QRCode.Render.Svg exposing (view)

import Html exposing (Html)
import QRCode.Matrix as Matrix
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)


moduleSize : Int
moduleSize =
    5


view : Matrix.Model -> Html msg
view matrix =
    let
        quietZone =
            8 * moduleSize

        sizePx =
            String.fromInt (List.length matrix * moduleSize + quietZone)
    in
    matrix
        |> List.indexedMap
            (\rowIndex row ->
                List.indexedMap (moduleView rowIndex) row
            )
        |> List.concat
        |> List.filterMap identity
        |> svg
            [ width sizePx
            , height sizePx
            , viewBox ("0 0 " ++ sizePx ++ " " ++ sizePx)
            ]


moduleView : Int -> Int -> Bool -> Maybe (Html msg)
moduleView rowIndex colIndex isDark =
    if isDark then
        -- Add 4 considering quiet zone
        Just (rectView (rowIndex + 4) (colIndex + 4))

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

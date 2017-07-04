module QRCode.Render.Svg
    exposing
        ( view
        )

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill)
import QRCode.Matrix as Matrix


moduleSize : Int
moduleSize =
    5


view : Matrix.Model -> Html msg
view matrix =
    let
        quietZone =
            8 * moduleSize

        sizePx =
            toString (List.length matrix * moduleSize + quietZone)
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
        [ y (toString (row * moduleSize))
        , x (toString (col * moduleSize))
        , width (toString moduleSize)
        , height (toString moduleSize)
        , fill "black"
        ]
        []

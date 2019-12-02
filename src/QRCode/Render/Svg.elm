module QRCode.Render.Svg exposing (view, viewWithoutQuietZone)

import Html exposing (Html)
import List.Extra as ListE
import QRCode.Matrix as Matrix
import Svg exposing (svg)
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
        |> List.map
            (ListE.indexedFoldl toRects ( Nothing, [] )
                >> appendLastRect
                >> List.reverse
            )
        |> List.indexedMap (viewRow quietZoneSize)
        |> List.concat
        |> svg
            [ width sizePx
            , height sizePx
            , viewBox ("0 0 " ++ sizePx ++ " " ++ sizePx)
            , shapeRendering "crispEdges"
            , fill "black"
            ]


type alias Rect =
    { x : Int
    , width : Int
    }


toRects : Int -> Bool -> ( Maybe Rect, List Rect ) -> ( Maybe Rect, List Rect )
toRects colIndex isDark ( maybeLastRect, rects ) =
    if isDark then
        case maybeLastRect of
            Just rect ->
                ( Just { rect | width = rect.width + 1 }
                , rects
                )

            Nothing ->
                ( Just
                    { x = colIndex
                    , width = 1
                    }
                , rects
                )

    else
        case maybeLastRect of
            Just rect ->
                ( Nothing, rect :: rects )

            Nothing ->
                ( Nothing, rects )


appendLastRect : ( Maybe Rect, List Rect ) -> List Rect
appendLastRect ( maybeLastRect, rects ) =
    case maybeLastRect of
        Just rect ->
            rect :: rects

        Nothing ->
            rects


viewRow : Int -> Int -> List Rect -> List (Html msg)
viewRow quietZoneSize row rects =
    List.map (viewRect quietZoneSize row) rects


viewRect : Int -> Int -> Rect -> Html msg
viewRect quietZoneSize row rect =
    Svg.rect
        [ y (String.fromInt ((row + quietZoneSize) * moduleSize))
        , x (String.fromInt ((rect.x + quietZoneSize) * moduleSize))
        , width (String.fromInt (rect.width * moduleSize))
        , height (String.fromInt moduleSize)
        ]
        []

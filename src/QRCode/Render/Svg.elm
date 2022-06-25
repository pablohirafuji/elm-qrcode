module QRCode.Render.Svg exposing (view, viewWithoutQuietZone)

import Html exposing (Html)
import List.Extra as ListE
import QRCode.Matrix as Matrix
import Svg exposing (svg)
import Svg.Attributes exposing (fill, height, shapeRendering, viewBox, width, x, y)


moduleSize : Int
moduleSize =
    5


view : List (Svg.Attribute msg) -> Matrix.Model -> Html msg
view =
    viewBase 4


viewWithoutQuietZone : List (Svg.Attribute msg) -> Matrix.Model -> Html msg
viewWithoutQuietZone =
    viewBase 0


viewBase : Int -> List (Svg.Attribute msg) -> Matrix.Model -> Html msg
viewBase quietZoneSize extraAttrs matrix =
    let
        quietZonePx =
            quietZoneSize * moduleSize

        sizePx =
            String.fromInt (List.length matrix * moduleSize + (2 * quietZonePx))
    in
    matrix
        |> List.map
            (List.foldl toRowLines
                ( { width = 0
                  , space = 0
                  }
                , []
                )
                >> appendLastRect
                >> List.reverse
            )
        |> List.indexedMap (viewRow quietZoneSize)
        |> List.concat
        |> String.concat
        |> Svg.Attributes.d
        |> (\d ->
                [ Svg.path
                    [ d
                    , "translate("
                        ++ String.fromInt quietZonePx
                        ++ ", "
                        ++ String.fromFloat (toFloat quietZonePx + (toFloat moduleSize / 2))
                        ++ ")"
                        |> Svg.Attributes.transform
                    , Svg.Attributes.strokeWidth "5px"
                    ]
                    []
                ]
           )
        |> svg
            ([ viewBox ("0 0 " ++ sizePx ++ " " ++ sizePx)
             , shapeRendering "crispEdges"
             , Svg.Attributes.stroke "#000"
             , Svg.Attributes.strokeWidth (String.fromInt moduleSize ++ "px")
             ]
                ++ extraAttrs
            )


type alias Rect =
    { width : Int
    , space : Int
    }


toRowLines : Bool -> ( Rect, List String ) -> ( Rect, List String )
toRowLines isDark ( lastRect, rowLines ) =
    if isDark then
        if lastRect.space == 0 then
            ( { lastRect | width = lastRect.width + 1 }
            , rowLines
            )

        else
            ( { width = 1
              , space = 0
              }
            , ([ if lastRect.width > 0 then
                    "h" ++ String.fromInt (lastRect.width * moduleSize)

                 else
                    ""
               , "m"
               , String.fromInt (lastRect.space * moduleSize)
               , " 0"
               ]
                |> String.concat
              )
                :: rowLines
            )

    else
        ( { lastRect | space = lastRect.space + 1 }
        , rowLines
        )


appendLastRect : ( Rect, List String ) -> List String
appendLastRect ( lastRect, rowLines ) =
    ("h" ++ String.fromInt (lastRect.width * moduleSize))
        :: rowLines


viewRow : Int -> Int -> List String -> List String
viewRow quietZoneSize rowIndex rowLines =
    "M0 "
        :: String.fromInt (rowIndex * moduleSize)
        :: rowLines

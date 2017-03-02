module QRCode.View exposing (toSvg)


import Array
import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill)
import QRCode.Matrix exposing (Model, Module)



moduleSize : Int
moduleSize = 5


toSvg : Model -> Html msg
toSvg { size, matrix } =
    let
        quietZone = 8 * moduleSize
        sizePx = toString (size * moduleSize + quietZone)


    in matrix
        |> Array.toList
        |> List.indexedMap (moduleView size)
        |> List.filterMap identity
        |> List.map rectView
        |> svg
            [ width sizePx
            , height sizePx
            , viewBox ("0 0 " ++ sizePx ++ " " ++ sizePx)
            ]


type alias ModuleView =
    { row : Int
    , col : Int
    }


indexToModuleView : Int -> Int -> ModuleView
indexToModuleView size index =
    { row = (index // size) + 4 -- quiet zone
    , col = (index % size) + 4 -- quiet zone
    }


moduleView : Int -> Int -> Maybe Module -> Maybe ModuleView
moduleView size index maybeModule =
    case maybeModule of
        Just ( _, True ) ->
            indexToModuleView size index
                |> Just

        _ ->
            Nothing


rectView : ModuleView -> Html msg
rectView { row, col } =
    rect
        [ x (toString (col * moduleSize))
        , y (toString (row * moduleSize))
        , width (toString moduleSize)
        , height (toString moduleSize)
        , fill "black"
        ] []

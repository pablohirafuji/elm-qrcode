module QRCode.Render.Canvas
    exposing
        ( view
        )

import Html exposing (Html)
import Collage exposing (Form, collage, group, filled, move, moveX, moveY, square)
import Element
import Color exposing (black, white)
import QRCode.Matrix as Matrix


view : Matrix.Model -> Html msg
view matrix =
    let
        quietZone =
            8 * canvasModuleSize

        qrCodeSize =
            List.length matrix * canvasModuleSize

        totalSize =
            qrCodeSize + quietZone
    in
        collage
            totalSize
            totalSize
            [ toFloat totalSize
                |> square
                |> filled white
            , matrixToForm matrix
                |> move
                    ( toFloat (qrCodeSize - canvasModuleSize)
                        / 2
                        |> negate
                        |> round
                        |> toFloat
                    , toFloat (qrCodeSize - canvasModuleSize)
                        / 2
                        |> round
                        |> toFloat
                    )
            ]
            |> Element.toHtml


matrixToForm : Matrix.Model -> Form
matrixToForm matrix =
    List.indexedMap
        (\index row ->
            canvasRow row
                |> moveY (toFloat (-index * canvasModuleSize))
        )
        matrix
        |> group


canvasRow : List Bool -> Form
canvasRow row =
    List.indexedMap
        (\index isDark ->
            if isDark then
                canvasModule
                    |> moveX (toFloat (index * canvasModuleSize))
                    |> Just
            else
                Nothing
        )
        row
        |> List.filterMap identity
        |> group


canvasModule : Form
canvasModule =
    square (toFloat canvasModuleSize)
        |> filled black


canvasModuleSize : Int
canvasModuleSize =
    5

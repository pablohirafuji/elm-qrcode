module QRCode.Render.String
    exposing
        ( view
        )

import QRCode.Matrix as Matrix


view : Matrix.Model -> String
view =
    List.map
        (List.map
            (\isDark ->
                if isDark then
                    "■"
                else
                    " "
            )
            >> String.concat
        )
        >> List.intersperse "\n"
        >> String.concat

module Main exposing (..)


import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (type_, selected, title)
import Html.Events exposing (onInput, onSubmit, on, targetValue)
import Html.Lazy exposing (lazy3)
import QRCode
import QRCode.ECLevel as ECLevel exposing (ECLevel)



type alias Model =
    { message      : String
    , ecLevel      : ECLevel
    , renderer     : Renderer
    , finalMessage : String
    }


initModel : Model
initModel =
    { message      = ""
    , ecLevel      = ECLevel.Q
    , renderer     = Svg
    , finalMessage = "Elm QR Code"
    }


type Renderer
    = Svg
    | String_


type Msg
    = UpdateMessage String
    | ChangeRenderer Renderer
    | ChangeECLevel ECLevel
    | Render


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMessage message ->
            ( { model | message = message }
            , Cmd.none
            )

        ChangeRenderer renderer ->
            ( { model | renderer = renderer }
            , Cmd.none
            )

        ChangeECLevel ecLevel ->
            ( { model | ecLevel = ecLevel }
            , Cmd.none
            )

        Render ->
            ( { model | finalMessage = model.message }
            , Cmd.none
            )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( initModel, Cmd.none )
        }


view : Model -> Html Msg
view { ecLevel, renderer, finalMessage } =
    div []
        [ form [ onSubmit Render ]
            [ input [ onInput UpdateMessage ] []
            , select
                [ title "Error Correction Level"
                , targetValue
                    |> Json.map (\str ->
                            case str of
                                "L" -> ECLevel.L
                                "M" -> ECLevel.M
                                "Q" -> ECLevel.Q
                                _   -> ECLevel.H
                        )
                    |> Json.map ChangeECLevel
                    |> on "change"
                ]
                [ option
                    [ selected (ecLevel == ECLevel.L) ]
                    [ text "L" ]
                , option
                    [ selected (ecLevel == ECLevel.M) ]
                    [ text "M" ]
                , option
                    [ selected (ecLevel == ECLevel.Q) ]
                    [ text "Q" ]
                , option
                    [ selected (ecLevel == ECLevel.H) ]
                    [ text "H" ]
                ]
            , select
                [ title "Renderer"
                , targetValue
                    |> Json.map (\str ->
                            if str == "SVG"
                                then Svg
                                else String_
                        )
                    |> Json.map ChangeRenderer
                    |> on "change"
                ]
                [ option
                    [ selected (renderer == Svg) ]
                    [ text "SVG" ]
                , option
                    [ selected (renderer == String_) ]
                    [ text "String" ]
                ]
            , button [ type_ "submit" ] [ text "Render" ]
            ]
        , lazy3 qrCodeView finalMessage ecLevel renderer
        ]


qrCodeView : String -> ECLevel -> Renderer -> Html msg
qrCodeView message ecLevel renderer  =
    case renderer of
        Svg ->
            qrCodeSvgView message ecLevel

        String_ ->
            qrCodeStrView message ecLevel


qrCodeSvgView : String -> ECLevel -> Html msg
qrCodeSvgView message =
    QRCode.encodeWithECLevel message
        >> Result.map QRCode.toSvg
        >> Result.withDefault
            (Html.text "Error while encoding to QR Code.")


qrCodeStrView : String -> ECLevel -> Html msg
qrCodeStrView message =
    QRCode.encodeWithECLevel message
        >> Result.map (QRCode.toString >> toHtml)
        >> Result.withDefault
            (Html.text "Error while encoding to QR Code.")


toHtml : String -> Html msg
toHtml qrCodeStr =
    Html.pre
        [ Html.Attributes.style
            [ ( "line-height", "0.6" )
            , ( "background", "white" )
            , ( "color", "black" )
            , ( "padding", "20px" )
            , ( "letter-spacing", "-0.5px" )
            ]
        ]
        [ Html.code [] [ Html.text qrCodeStr ] ]


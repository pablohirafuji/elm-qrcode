module Main exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (type_, selected, title)
import Html.Events exposing (onInput, onSubmit, on, targetValue)
import Html.Lazy exposing (lazy3)
import Navigation
import QRCode exposing (QRCode)
import QRCode.ECLevel as ECLevel exposing (ECLevel)
import QRCode.Error exposing (Error)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        initStr =
            String.dropLeft 1 location.hash
    in
        ( if String.isEmpty initStr then
            initModel
          else
            { initModel
                | message = initStr
                , finalMessage = initStr
            }
        , Cmd.none
        )



-- MODEL


type alias Model =
    { message : String
    , ecLevel : ECLevel
    , renderer : Renderer
    , finalMessage : String
    }


initModel : Model
initModel =
    { message = ""
    , ecLevel = ECLevel.Q
    , renderer = Canvas
    , finalMessage = "Elm QR Code"
    }


type Renderer
    = Canvas
    | Svg
    | String_



-- UPDATE


type Msg
    = UrlChange Navigation.Location
    | UpdateMessage String
    | ChangeRenderer Renderer
    | ChangeECLevel ECLevel
    | Render


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange _ ->
            ( model
            , Cmd.none
            )

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



-- VIEW


view : Model -> Html Msg
view { ecLevel, renderer, finalMessage } =
    div []
        [ form [ onSubmit Render ]
            [ input
                [ onInput UpdateMessage
                , Html.Attributes.defaultValue finalMessage
                ]
                []
            , select
                [ title "Error Correction Level"
                , targetValue
                    |> Json.map
                        (\str ->
                            case str of
                                "L" ->
                                    ECLevel.L

                                "M" ->
                                    ECLevel.M

                                "Q" ->
                                    ECLevel.Q

                                _ ->
                                    ECLevel.H
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
                    |> Json.map
                        (\str ->
                            if str == "Canvas" then
                                Canvas
                            else if str == "SVG" then
                                Svg
                            else
                                String_
                        )
                    |> Json.map ChangeRenderer
                    |> on "change"
                ]
                [ option
                    [ selected (renderer == Canvas) ]
                    [ text "Canvas" ]
                , option
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
qrCodeView message ecLevel renderer =
    QRCode.encodeWithECLevel message ecLevel
        |> qrCodeRender renderer
        |> Result.withDefault
            (Html.text "Error while encoding to QR Code.")


qrCodeRender : Renderer -> Result Error QRCode -> Result Error (Html msg)
qrCodeRender renderer =
    case renderer of
        Canvas ->
            Result.map QRCode.toCanvas

        Svg ->
            Result.map QRCode.toSvg

        String_ ->
            Result.map (QRCode.toString >> toHtml)


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

module Main exposing (..)

import Browser exposing (Page)
import Browser.Navigation
import Html exposing (..)
import Html.Attributes exposing (selected, title, type_, style)
import Html.Events exposing (on, onInput, onSubmit, targetValue)
import Html.Lazy exposing (lazy3)
import Json.Decode as Decode exposing (Value)
import QRCode exposing (QRCode)
import QRCode.ECLevel as ECLevel exposing (ECLevel)
import QRCode.Error exposing (Error)
import Url.Parser exposing (Url)


main : Program Value Model Msg
main =
    Browser.fullscreen
        { init = init
        , onNavigation = Just onNavigation
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Browser.Env Value -> ( Model, Cmd Msg )
init { url, flags } =
    ( initModel, Cmd.none )


onNavigation : Url -> Msg
onNavigation url =
    UrlChange url



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
    , renderer = Svg
    , finalMessage = "Elm QR Code"
    }


type Renderer
    = Svg
    | String_



-- UPDATE


type Msg
    = UrlChange Url
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


view : Model -> Page Msg
view model =
    Page "QR Code" [ view_ model ]


view_ : Model -> Html Msg
view_ { ecLevel, renderer, finalMessage } =
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
                    |> Decode.map
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
                    |> Decode.map ChangeECLevel
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
                    |> Decode.map
                        (\str ->
                            if str == "SVG" then
                                Svg
                            else
                                String_
                        )
                    |> Decode.map ChangeRenderer
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
qrCodeView message ecLevel renderer =
    QRCode.encodeWithECLevel message ecLevel
        |> qrCodeRender renderer
        |> Result.mapError Debug.toString
        |> \n ->
            case n of
                Ok a ->
                    a

                Err a ->
                    Html.text a


qrCodeRender : Renderer -> Result Error QRCode -> Result Error (Html msg)
qrCodeRender renderer =
    case renderer of
        Svg ->
            Result.map QRCode.toSvg

        String_ ->
            Result.map (QRCode.toString >> toHtml)


toHtml : String -> Html msg
toHtml qrCodeStr =
    Html.pre
        [ style "line-height" "0.6"
        , style "background" "white"
        , style "color" "black"
        , style "padding" "20px"
        , style "letter-spacing" "-0.5px"
        ]
        [ Html.code [] [ Html.text qrCodeStr ] ]

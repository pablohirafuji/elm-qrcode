module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (class, href, selected, src, style, title, type_)
import Html.Events exposing (on, onInput, onSubmit, targetValue)
import Html.Lazy exposing (lazy3)
import Image exposing (Image)
import Json.Decode as JsonD exposing (Value)
import Json.Encode as JsonE
import QRCode exposing (Error(..), ErrorCorrection(..), QRCode)
import Svg.Attributes as SvgA
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = onUrlChange
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( initModel url.fragment
    , Cmd.none
    )


onUrlChange : Url -> Msg
onUrlChange url =
    UrlChange url



-- MODEL


type alias Model =
    { message : String
    , ecLevel : ErrorCorrection
    , renderer : Renderer
    , finalMessage : String
    }


initModel : Maybe String -> Model
initModel mS =
    { message = Maybe.withDefault "Elm QR Code" mS
    , ecLevel = Quartile
    , renderer = Svg
    , finalMessage = Maybe.withDefault "Elm QR Code" mS
    }


type Renderer
    = Svg
    | Png
    | Bmp



-- UPDATE


type Msg
    = NoOp
    | UrlChange Url
    | UpdateMessage String
    | ChangeRenderer Renderer
    | ChangeErrorCorrection ErrorCorrection
    | Render


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

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

        ChangeErrorCorrection ecLevel ->
            ( { model | ecLevel = ecLevel }
            , Cmd.none
            )

        Render ->
            ( { model | finalMessage = model.message }
            , Cmd.none
            )



-- VIEW


view : Model -> Document Msg
view model =
    Document "Elm QR Code" (view_ model)


view_ : Model -> List (Html Msg)
view_ { ecLevel, renderer, finalMessage, message } =
    [ h1 []
        [ text "Elm QR Code "
        , small [] [ text "v4.0.0" ]
        ]
    , p [ class "subheading" ]
        [ a [ href "http://package.elm-lang.org/packages/pablohirafuji/elm-qrcode/latest" ]
            [ text "Package" ]
        , text " / "
        , a [ href "https://github.com/pablohirafuji/elm-qrcode" ]
            [ text "GitHub" ]
        , text " / "
        , a [ href "https://github.com/pablohirafuji/elm-qrcode/blob/master/demo/src/Main.elm" ]
            [ text "Source" ]
        ]
    , form [ onSubmit Render ]
        [ input
            [ onInput UpdateMessage
            , Html.Attributes.value message
            ]
            []
        , select
            [ title "Error Correction Level"
            , targetValue
                |> JsonD.map
                    (\str ->
                        case str of
                            "Low" ->
                                Low

                            "Medium" ->
                                Medium

                            "Quartile" ->
                                Quartile

                            _ ->
                                High
                    )
                |> JsonD.map ChangeErrorCorrection
                |> on "change"
            ]
            [ option
                [ selected (ecLevel == Low) ]
                [ text "Low" ]
            , option
                [ selected (ecLevel == Medium) ]
                [ text "Medium" ]
            , option
                [ selected (ecLevel == Quartile) ]
                [ text "Quartile" ]
            , option
                [ selected (ecLevel == High) ]
                [ text "High" ]
            ]
        , select
            [ title "Renderer"
            , targetValue
                |> JsonD.map
                    (\str ->
                        case str of
                            "SVG" ->
                                Svg

                            "PNG" ->
                                Png

                            "BMP" ->
                                Bmp

                            _ ->
                                Svg
                    )
                |> JsonD.map ChangeRenderer
                |> on "change"
            ]
            [ option
                [ selected (renderer == Svg) ]
                [ text "SVG" ]
            , option
                [ selected (renderer == Png) ]
                [ text "PNG" ]
            , option
                [ selected (renderer == Bmp) ]
                [ text "BMP" ]
            ]
        , button [ type_ "submit" ] [ text "Render" ]
        ]
    , lazy3 qrCodeView finalMessage ecLevel renderer
    ]


qrCodeView : String -> ErrorCorrection -> Renderer -> Html msg
qrCodeView message ecLevel renderer =
    QRCode.fromStringWith ecLevel message
        |> Result.map
            (qrCodeRender renderer
                >> List.singleton
                >> div [ class "qrcode" ]
            )
        |> (\n ->
                case n of
                    Ok a ->
                        a

                    Err e ->
                        case e of
                            InputLengthOverflow ->
                                div []
                                    [ p []
                                        [ text "Too much information! I can't encode that!"
                                        ]
                                    ]

                            _ ->
                                div []
                                    [ p []
                                        [ text "An error occured while encoding to QRCode: "
                                        , b [] [ text (errorToString e) ]
                                        ]
                                    , p []
                                        [ text "Please, report at "
                                        , a [ href "https://github.com/pablohirafuji/elm-qrcode/issues" ] [ text "https://github.com/pablohirafuji/elm-qrcode/issues" ]
                                        , text "."
                                        ]
                                    ]
           )


qrCodeRender : Renderer -> QRCode -> Html msg
qrCodeRender renderer qrCode =
    case renderer of
        Svg ->
            QRCode.toSvg
                [ SvgA.width "300px"
                , SvgA.height "300px"
                ]
                qrCode

        Png ->
            Html.img
                [ QRCode.toImage qrCode
                    |> Image.toPngUrl
                    |> src
                ]
                []

        Bmp ->
            Html.img
                [ QRCode.toImage qrCode
                    |> Image.toBmpUrl
                    |> src
                ]
                []


errorToString : QRCode.Error -> String
errorToString e =
    case e of
        AlignmentPatternNotFound ->
            "AlignmentPatternNotFound"

        InvalidNumericChar ->
            "InvalidNumericChar"

        InvalidAlphanumericChar ->
            "InvalidAlphanumericChar"

        InvalidUTF8Char ->
            "InvalidUTF8Char"

        LogTableException i ->
            "LogTableException " ++ String.fromInt i

        PolynomialMultiplyException ->
            "PolynomialMultiplyException"

        PolynomialModException ->
            "PolynomialModException"

        InputLengthOverflow ->
            "InputLengthOverflow"

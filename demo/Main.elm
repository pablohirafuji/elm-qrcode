module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy)
import QRCode



type alias Model =
    { message : String
    , finalMessage : String
    }


initModel : Model
initModel =
    { message = ""
    , finalMessage = "Elm QR Code"
    }


type Msg
    = UpdateMessage String
    | Render


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMessage message ->
            ( { model | message = message }
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
view { finalMessage } =
    div []
        [ form [ onSubmit Render ]
            [ input [ onInput UpdateMessage ] []
            , button [ type_ "submit" ] [ text "Render" ]
            ]
        , div [] [ lazy render finalMessage ]
        ]


render : String -> Html msg
render message =
    Html.div []
        [ QRCode.toSvg message
            |> \result -> case result of
                Result.Ok view -> view
                Result.Err err -> Html.text (toString err)
        ]


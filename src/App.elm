module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { x : Int
    , y : Int
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { x = 100, y = 0 }, Cmd.none )


type Msg
    = MoveLeft
    | MoveRight
    | MoveDown
    | MoveUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveLeft ->
            ( { model | x = model.x - 10 }, Cmd.none )

        MoveRight ->
            ( { model | x = model.x + 10 }, Cmd.none )

        MoveDown ->
            ( { model | y = model.y + 10 }, Cmd.none )

        MoveUp ->
            ( { model | y = model.y - 10 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Square Adventure" ]
        , drawSquare model
        , controls
        ]


controls : Html Msg
controls =
    div
        [ style
            [ ( "position", "fixed" )
            , ( "top", "0" )
            ]
        ]
        [ button [ onClick MoveLeft ] [ text "<-" ]
        , button [ onClick MoveRight ] [ text "->" ]
        , button [ onClick MoveDown ] [ text "v" ]
        , button [ onClick MoveUp ] [ text "^" ]
        ]


drawSquare : Model -> Html Msg
drawSquare model =
    div
        [ style [ ( "position", "relative" ) ] ]
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "left", (toString model.x) ++ "px" )
                , ( "top", (toString model.y) ++ "px" )
                , ( "background-color", "pink" )
                , ( "width", "100px" )
                , ( "height", "100px" )
                ]
            ]
            []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

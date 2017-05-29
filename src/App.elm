module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Extra exposing (Key(..))
import Time


type alias Model =
    { x : Int
    , y : Int
    , pressedKeys : List Key
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { x = 100
      , y = 0
      , pressedKeys = []
      }
    , Cmd.none
    )


type Msg
    = MoveLeft
    | MoveRight
    | MoveDown
    | MoveUp
    | KeyboardMsg Keyboard.Extra.Msg
    | Tick Time.Time


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

        KeyboardMsg keyMsg ->
            ( { model
                | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
              }
            , Cmd.none
            )

        Tick _ ->
            ( handleKeys model
            , Cmd.none
            )


handleKeys : Model -> Model
handleKeys model =
    List.foldl handleKey model model.pressedKeys


handleKey : Key -> Model -> Model
handleKey key model =
    case key of
        ArrowLeft ->
            { model | x = model.x - 1 }

        ArrowRight ->
            { model | x = model.x + 1 }

        ArrowDown ->
            { model | y = model.y + 1 }

        ArrowUp ->
            { model | y = model.y - 1 }

        _ ->
            model


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Square Adventure" ]
        , drawSquare model
        , controls
        , div [] [ text <| toString model.pressedKeys ]
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
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (Time.second / 100) Tick
        ]

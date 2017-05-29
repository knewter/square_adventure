module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Extra exposing (Key(..))
import Time
import Array exposing (Array)


type alias Model =
    { x : Int
    , y : Int
    , color : Int
    , pressedKeys : List Key
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { x = 100
      , y = 0
      , color = 0
      , pressedKeys = []
      }
    , Cmd.none
    )


colors : Array String
colors =
    Array.fromList [ "pink", "red", "blue", "green", "purple" ]


type Msg
    = MoveLeft
    | MoveRight
    | MoveDown
    | MoveUp
    | NextColor
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

        NextColor ->
            let
                colorsCount =
                    Array.length colors

                nextColor =
                    if (model.color + 1) == colorsCount then
                        0
                    else
                        model.color + 1
            in
                ( { model | color = nextColor }
                , Cmd.none
                )

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
        , button [ onClick NextColor ] [ text "Next Color" ]
        ]


drawSquare : Model -> Html Msg
drawSquare model =
    let
        color =
            colors
                |> Array.get model.color
                |> Maybe.withDefault "yellow"
    in
        div
            [ style [ ( "position", "relative" ) ] ]
            [ div
                [ style
                    [ ( "position", "absolute" )
                    , ( "left", (toString model.x) ++ "px" )
                    , ( "top", (toString model.y) ++ "px" )
                    , ( "background-color", color )
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

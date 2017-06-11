module App exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Extra exposing (Key(..))
import Time


type alias Enemy =
    { x : Int
    , y : Int
    , color : Int
    }


type GameState
    = Running
    | Dead


type alias Model =
    { x : Int
    , y : Int
    , color : Int
    , pressedKeys : List Key
    , enemies : Dict Int Enemy
    , gameState : GameState
    }


playerSize : Int
playerSize =
    100


enemySize : Int
enemySize =
    50


init : String -> ( Model, Cmd Msg )
init path =
    ( { x = 100
      , y = 0
      , color = 0
      , pressedKeys = []
      , enemies = initialEnemies
      , gameState = Running
      }
    , Cmd.none
    )


initialEnemies : Dict Int Enemy
initialEnemies =
    Dict.empty
        |> Dict.insert 0 { x = 10, y = 10, color = 0 }
        |> Dict.insert 1 { x = 10, y = 110, color = 1 }
        |> Dict.insert 2 { x = 10, y = 210, color = 2 }
        |> Dict.insert 3 { x = 10, y = 310, color = 3 }


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
            case model.gameState of
                Running ->
                    ( model
                        |> handleKeys
                        |> moveEnemies
                        |> handleCollisions
                    , Cmd.none
                    )

                Dead ->
                    ( model
                    , Cmd.none
                    )


moveEnemies : Model -> Model
moveEnemies model =
    let
        nextEnemies =
            Dict.map
                (\key enemy -> { enemy | x = enemy.x + 1 })
                model.enemies
    in
    { model | enemies = nextEnemies }


handleCollisions : Model -> Model
handleCollisions model =
    let
        minX =
            model.x

        maxX =
            model.x + playerSize

        minY =
            model.y

        maxY =
            model.y + playerSize

        calculateEnemyCoordinates : Enemy -> { minX : Int, maxX : Int, minY : Int, maxY : Int }
        calculateEnemyCoordinates enemy =
            { minX = enemy.x
            , maxX = enemy.x + enemySize
            , minY = enemy.y
            , maxY = enemy.y + enemySize
            }

        hasCollision : Enemy -> Bool
        hasCollision enemy =
            let
                coordinates =
                    calculateEnemyCoordinates enemy

                topLeftCollision =
                    (minX <= coordinates.minX && coordinates.minX <= maxX)
                        && (minY <= coordinates.minY && coordinates.minY <= maxY)

                topRightCollision =
                    (minX <= coordinates.maxX && coordinates.maxX <= maxX)
                        && (minY <= coordinates.minY && coordinates.minY <= maxY)

                bottomLeftCollision =
                    (minX <= coordinates.minX && coordinates.minX <= maxX)
                        && (minY <= coordinates.maxY && coordinates.maxY <= maxY)

                bottomRightCollision =
                    (minX <= coordinates.maxX && coordinates.maxX <= maxX)
                        && (minY <= coordinates.maxY && coordinates.maxY <= maxY)
            in
            topLeftCollision
                || topRightCollision
                || bottomLeftCollision
                || bottomRightCollision

        hasAnyCollision : Bool
        hasAnyCollision =
            model.enemies
                |> Dict.values
                |> List.map hasCollision
                |> List.member True
    in
    if hasAnyCollision then
        { model | gameState = Dead }
    else
        model


handleKeys : Model -> Model
handleKeys model =
    List.foldl handleKey model model.pressedKeys


handleKey : Key -> Model -> Model
handleKey key model =
    case key of
        ArrowLeft ->
            { model | x = model.x - 2 }

        ArrowRight ->
            { model | x = model.x + 2 }

        ArrowDown ->
            { model | y = model.y + 2 }

        ArrowUp ->
            { model | y = model.y - 2 }

        _ ->
            model


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Square Adventure" ]
        , drawGameState model.gameState
        , drawPlayer model
        , drawEnemies model.enemies
        , controls
        , div [] [ text <| toString model.pressedKeys ]
        ]


drawGameState : GameState -> Html Msg
drawGameState gameState =
    case gameState of
        Running ->
            h3 [] [ text "Running" ]

        Dead ->
            h3 [] [ text "Dead" ]


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


drawPlayer : Model -> Html Msg
drawPlayer model =
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
                , ( "left", toString model.x ++ "px" )
                , ( "top", toString model.y ++ "px" )
                , ( "background-color", color )
                , ( "width", toString playerSize ++ "px" )
                , ( "height", toString playerSize ++ "px" )
                ]
            ]
            []
        ]


drawEnemies : Dict Int Enemy -> Html Msg
drawEnemies enemies =
    div []
        (List.map drawEnemy (Dict.values enemies))


drawEnemy : Enemy -> Html Msg
drawEnemy enemy =
    let
        color =
            colors
                |> Array.get enemy.color
                |> Maybe.withDefault "yellow"
    in
    div
        [ style [ ( "position", "relative" ) ] ]
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "left", toString enemy.x ++ "px" )
                , ( "top", toString enemy.y ++ "px" )
                , ( "background-color", color )
                , ( "width", toString enemySize ++ "px" )
                , ( "height", toString enemySize ++ "px" )
                ]
            ]
            []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (Time.second / 50) Tick
        ]

module App exposing (..)
import Html exposing (Html, div, text, program)
import Set as S
import Keyboard exposing (..)
import Time exposing (Time, second, millisecond)
import Char exposing (fromCode)

type alias Part =
  { x: Int
  , y: Int
  }

type Direction = N | S | E | W

type alias Model =
  { msg: String
  , keys: S.Set Char
  , time: Int
  , direction: Direction
  , hd: Part
  }

init : (Model, Cmd Msg)
init =
  ( {msg = "Hello", keys = S.empty, time = 0, direction = S, hd = {x = 10, y = 10}}, Cmd.none )

type Msg
  = NoOp
    | Downs Char
    | Ups Char
    | Tick

viewPart : Part -> Html Msg
viewPart s = div [] [text (toString s)]
viewParts : List Part -> Html Msg
viewParts s = div [] (List.map viewPart s)

view : Model -> Html Msg
view model =
  div []
      [text (toString model), viewPart model.hd]

updateDirection : Model -> Model
updateDirection model = 
  let
    left = S.member 'A' model.keys
    right = S.member 'D' model.keys
    up = S.member 'W' model.keys
    down = S.member 'S' model.keys
  in
    { model |
      direction =
        case model.direction of
          S -> if left then W else if right then E else S
          N -> if left then W else if right then E else N
          E -> if up then N else if down then S else E
          W -> if up then N else if down then S else W
    }

stepSnake : Model -> Model
stepSnake model =
  let
    hd =
      case model.direction of
        S -> { x = model.hd.x, y = model.hd.y + 1 }
        N -> { x = model.hd.x, y = model.hd.y - 1 }
        E -> { x = model.hd.x + 1, y = model.hd.y }
        W -> { x = model.hd.x - 1, y = model.hd.y }
  in
    { model |
      time = model.time + 1
    , hd = hd
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    Downs code ->
      (updateDirection {model | keys = S.insert code model.keys}, Cmd.none )
    Ups code ->
      (updateDirection {model | keys = S.remove code model.keys}, Cmd.none )
    Tick ->
      (stepSnake model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs (\code -> Downs (fromCode code))
    , Keyboard.ups (\code -> Ups (fromCode code))
    , Time.every (millisecond * 500) (\_-> Tick)
    ]

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

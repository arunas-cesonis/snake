module App exposing (..)
import Html exposing (Html, div, text, program)
import Html.Attributes exposing (style)
import Set as S
import Keyboard exposing (..)
import Time exposing (Time, second, millisecond)
import Char exposing (fromCode)

type alias Part =
  { x: Int
  , y: Int
  }

type alias Stage =
  { w: Int
  , h: Int
  }

type Direction = N | S | E | W

type alias Model =
  { keys: S.Set Char
  , direction: Direction
  , parts: List Part
  , length: Int
  , stage: Stage
  }

init : (Model, Cmd Msg)
init =
  ( {keys = S.empty, direction = S, length = 5, stage = {w=20, h=20}, parts = []}, Cmd.none )

type Msg
  = NoOp
    | Downs Char
    | Ups Char
    | Tick


px : Int -> String
px n = toString n ++ "px"

rect : String -> Int -> Int -> Int -> Int -> Html Msg
rect color x y w h =
  let
    s = style
      [ ("backgroundColor", color)
      , ("position", "absolute")
      , ("width", px w)
      , ("height", px h)
      , ("left", px x)
      , ("top", px y)
      ]
  in
    div [s] []

viewPart : Part -> Html Msg
viewPart p = rect "black" (p.x * 10) (p.y * 10) 10 10

viewParts : List Part -> Html Msg
viewParts s = div [] (List.map viewPart s)

viewStage : Stage -> Html Msg
viewStage s = rect "#666" 0 0 (s.w * 10) (s.h * 10)

view : Model -> Html Msg
view model =
  div []
      [ viewStage model.stage
      , viewParts model.parts
      , text (toString model)
      ]

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

stepPart : Direction -> Part -> Part
stepPart direction part =
  case direction of
    S -> { x = part.x, y = part.y + 1 }
    N -> { x = part.x, y = part.y - 1 }
    E -> { x = part.x + 1, y = part.y }
    W -> { x = part.x - 1, y = part.y }

stepSnake : Model -> Model
stepSnake model =
  let
    currentHead =
      case List.head model.parts of
        Just hd -> hd
        Nothing -> { x = model.stage.w // 2, y = model.stage.h // 2 }
    newHead = stepPart model.direction currentHead
    parts = newHead :: List.take model.length model.parts
  in
    { model | parts = parts }

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

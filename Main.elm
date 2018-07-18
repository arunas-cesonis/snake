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
  , hd: Part
  , tl: List Part
  , length: Int
  , stage: Stage
  }

initialModel : Model
initialModel =
  {keys = S.empty, direction = S, length = 5, stage = {w=20, h=20}, hd = {x=10,y=10}, tl = []}

init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none )

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
      , viewParts (model.hd :: model.tl)
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

isWithinStage : Stage -> Part -> Bool
isWithinStage {w, h} {x, y} = x > 0 && y > 0 && x < w && y < h

part2part : Part -> Part -> Bool
part2part p1 p2 =
  p1.x == p2.x && p1.y == p2.y

stepCollision : Model -> Model
stepCollision model =
  let
    collidesWithStage = not (isWithinStage model.stage model.hd)
    collidesWithSelf = List.any (part2part model.hd) model.tl
  in
    if collidesWithStage || collidesWithSelf then initialModel else model

stepSnake : Model -> Model
stepSnake model =
  let
    hd = stepPart model.direction model.hd
    tl = List.take model.length (model.hd :: model.tl)
  in
    { model | hd = hd, tl = tl }

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
      (stepCollision (stepSnake model), Cmd.none )

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

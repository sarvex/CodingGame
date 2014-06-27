module Game where 

import Keyboard
import Window
import Mouse
import Json
import Dict
import Array

-- incoming code source for player control

port code_port : Signal Json.Value



type Level = {groundx: [Float], groundy: [Float]}
type Levels = Array.Array Level

levels: Levels
levels = Array.fromList [ 
{- each level is a record with following elements:
   1) list of ints representing the ground by segments:
      first int - y of the horizontal segment that starts at x=0
      second int - x of the vertical segment
      third int - y of the next horizontal segment
      an so on
      the last segment is horizontal ending at x = w
   2) list of floating polygons represented by list of n points (x, y), where n>=3
-}
   {groundx = [800, 850], groundy = [50, 20, 100]} 
   ]


-- Game state

data State = Before | Playing | Finished

data Direction = Right | Left

-- Game world description

data Action = Forward | Backward | Jump | None


type Hero = {x: Float, y: Float, vx: Float, vy: Float, dir: Direction}

hero = { x=0, y=0, vx=0, vy=0, dir=Right }


type Game = {state: State, level_num: Int, hero: Hero, levels: Levels}


defaultGame: Game
defaultGame = {state = Before, level_num=0, hero=hero, levels = levels}


actionToArrows: Action->(Int, Int)
actionToArrows action = 
     case action of
        Forward -> (1, 0)
        Backward -> (-1, 0)
        Jump -> (0, 1)
        None -> (0, 0) -- Don't move 


defaultHero: Level->Hero
defaultHero level = 
  {x = 0, y = head level.groundy, vx=0, vy=0, dir=Right}


dead: Float->Float->Float->Float->Level->Bool
dead x y w h level = 
  if y<0 then True else False


win: Float->Float->Float->Float->Level->Bool
win x y w h level = 
   if x>w then True else False

get_level: Int -> Array.Array Level -> Level 
get_level level_num levels = 
  Array.getOrFail level_num levels --todo handle maybe


stepGame : Input -> Game -> Game
stepGame input ({state, level_num, hero, levels} as game) =
  let level = get_level level_num levels
      w = toFloat input.w
      h = toFloat input.h
      action = input.action
      delta = input.delta
      
  in  {game| state   <- if | state == Before && action /= None -> Playing
                           | state == Playing && level_num > (Array.length levels) -> Finished
                           | otherwise        -> state
           , hero    <- if state == Playing then 
                           if dead hero.x hero.y w h level then defaultHero level 
                           else step delta (actionToArrows action) hero 
                        else hero
           , level_num   <- if state == Playing && (win hero.x hero.y w h level) then level_num + 1 else level_num
      }

gameState = foldp stepGame defaultGame input

-- Physics -- ("m" is for me)

jump: (Int, Int)->Hero->Hero
jump (x, y) m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity: Float->Hero->Hero
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m
physics: Float->Hero->Hero
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk: (Int, Int)->Hero->Hero
walk (x, y) m = { m | vx <- toFloat x
                 , dir <- if | x < 0     -> Left
                             | x > 0     -> Right
                             | otherwise -> m.dir }

step: Float -> (Int, Int) -> Hero -> Hero
step t dir = physics t . walk dir . gravity t . jump dir



drawGround: Float -> Float -> Level -> [Form]
drawGround w h level = map (\(x1, (x2, y2)) -> rect (x2-x1) y2
                            |> filled (rgb 74 163 41) 
                            |> move (x1 + (x2-x1)/2 - w/2, y2/2 - h/2) 
                            )
                         (zip (0 :: level.groundx) (zip (level.groundx ++ [w] ) level.groundy )) 


-- DISPLAY
render: (Int, Int)->Game->Element
render (w',h') game =
  let hero = game.hero
      level = get_level game.level_num game.levels
      (w,h) = (toFloat w', toFloat h')
      verb = if | hero.y  >  0 -> "jump"
                | hero.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src  = "imgs/mario/" ++ verb ++ "/" ++ (if hero.dir == Right then "right" else "left") ++ ".gif"
  in collage w' h'
      ([ rect w h  |> filled (rgb 174 238 238)]
      ++ (drawGround w h level) 
      --, rect w 50 |> filled (rgb 74 163 41)
                  --|> move (0, 24 - h/2)
      ++ [toForm (image 35 35 src) |> move (hero.x, hero.y + 62 - h/2)]
      )

encodeArrows {x, y} = if | x >0 -> Forward
                         | x <0 -> Backward
                         | y >0 -> Jump
                         | otherwise -> None
 

type Input = {delta: Float, action: Action, w: Int, h: Int}

newinput:Float->Action->(Int, Int)->Input
newinput delta action (w,h) = {delta = delta, action =  action, w = w, h = h}

-- MARIO

input: Signal Input
input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift3 newinput delta (lift encodeArrows Keyboard.arrows) Window.dimensions)

main  = lift2 render Window.dimensions gameState


json_processor: (Dict.Dict String Json.Value) -> String
json_processor d = concat[
    "I will ",
    (Json.toString "" (Dict.getOrFail "action" d)),
    " to ",
    (Json.toString "" (Dict.getOrFail "direction" d))]

smart_text_obtainer: Json.Value -> String
smart_text_obtainer json_value =
        case json_value of
            Json.String s -> s
            Json.Object d ->  json_processor d



--main = lift (\json->asText (smart_text_obtainer json)) code_port
port messageOut : Signal Int
port messageOut = Mouse.x

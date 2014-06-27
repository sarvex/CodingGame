module Game where 

import Keyboard
import Window
import Mouse
import Json
import Dict
import Array

-- incoming code source for player control

port code_port : Signal Json.Value


eps = 0.00001


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


-- Game world description

data Action = Forward | Backward | Jump | None

data Direction = Right | Left


type Hero = {x: Float, y: Float, vx: Float, vy: Float, dir: Direction}

hero = { x=0, y=0, vx=0, vy=0, dir=Right }


type Game = {state: State, level_num: Int, hero: Hero, levels: Levels, w: Float, h:Float}


defaultGame: Game
defaultGame = {state = Before, level_num=0, hero=hero, levels = levels, w = 0, h = 0}


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

cur_level: Game->Level
cur_level g = 
    get_level g.level_num g.levels


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
           , hero    <- if | state == Before -> defaultHero level
                           | state == Playing -> 
                           if dead hero.x hero.y w h level then defaultHero level 
                           else step delta (actionToArrows action) game hero 
                           |otherwise -> hero
           , level_num   <- if state == Playing && (win hero.x hero.y w h level) then level_num + 1 else level_num
           , w <- w
           , h <- h
      }

gameState = foldp stepGame defaultGame input

-- Physics --

groundBlocks: Float->Level -> [(Float, (Float, Float))]
groundBlocks w level = 
      (zip (level.groundy) (zip (level.groundx ++ [w] ) (0 :: level.groundx) )) 


intersectBlocks: (Float, Float) -> [(Float, (Float, Float))] -> Bool
intersectBlocks (x, y) blocks =
       not (isEmpty (filter (\(y1, (x1, x2)) -> if (x>x1-eps) && (x<x1+eps) && (y<y1+eps) then True else False ) blocks))   --TODO



jump: (Int, Int)->Game->Hero->Hero
jump (x, y) g hero =
  if (y > 0) && (hero.y == 0) then { hero | vy <- 5 } else hero

gravity: Float->Game->Hero->Hero
gravity t g hero=   
  if not (intersectBlocks (hero.x, hero.y) (groundBlocks g.w (cur_level g))) then { hero | vy <- hero.vy - t/4 } else hero

physics: Float->Game->Hero->Hero
physics t g hero =   
  { hero | x <- hero.x + t*hero.vx , y <- max 0 (hero.y + t*hero.vy) }

walk: (Int, Int)->Game->Hero->Hero
walk (x, y) g hero =
  { hero | vx <- toFloat x
                 , dir <- if | x < 0     -> Left
                             | x > 0     -> Right
                             | otherwise -> hero.dir }

step: Float -> (Int, Int) -> Game -> Hero-> Hero
step t dir g = physics t g . walk dir g . gravity t g . jump dir g



drawGround: Float -> Float -> Level -> [Form]
drawGround w h level = map (\(y, (x1, x2)) -> rect (x2-x1) y
                            |> filled (rgb 74 163 41) 
                            |> move (x1 + (x2-x1)/2 - w/2, y/2 - h/2) 
                            )
                         (groundBlocks w level) 


-- DISPLAY
render: (Int, Int)->Game->Element
render (w',h') game =
  let hero = game.hero
      level = cur_level game
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
      ++ [ (if game.state == Before then toForm (plainText "Press Run to Start") else (toForm (image 35 35 src) |> move (hero.x, hero.y + 17 - h/2)))]
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

module Game where 

import Keyboard
import Window
import Mouse
import Json
import Dict
import Array

import Levels (..)

-- incoming code source for player control

port code_port : Signal Json.Value




-- Game state

data State = Before | Playing | Finished


-- Game world description

data Action = Forward | Backward | Jump | None

data Direction = Right | Left


type Hero = {x: Float, y: Float, vx: Float, vy: Float, dir: Direction, size: Float}

hero = { x=0, y=0, vx=0, vy=0, dir=Right, size = 35}


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
  {x = 0, y = 50 + (head level.groundy), vx=0, vy=0, dir=Right, size=35}


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

corpus hero v = 
    if v <0 then -hero.size/2 else hero.size 

move_hor: Float->Float->Maybe Segment->Float
move_hor x dx s = case s of
                     Nothing -> x + dx
                     Just (y, (x1, x2)) -> if | dx<0 -> x2
                                             | dx>0 -> x1
                                             | otherwise -> x + dx 
move_vert: Float->Float->Maybe Segment->Float
move_vert y dy s = case s of
                     Nothing -> y + dy
                     Just (y', (x1, x2)) -> if  dy<0 then y' else y + dy
                                             


physics: Float->(Int, Int)->Game->Hero->Hero
physics t (dir_x, dir_y) g hero =  
  let m_v = { hero | y <- hero.y + t*hero.vy }
      m_h  = { hero | x <- hero.x + t*hero.vx }
      b = groundBlocks g.w (cur_level g)
      vert_int = intersectBlocks m_v.x (m_v.y + (corpus m_v m_v.y))  b
      hor_int  = intersectBlocks (m_h.x + (corpus m_h m_h.x)) m_h.y b
  in { hero | x <- move_hor hero.x (t*hero.vx) hor_int,
              y <- move_vert hero.y (t*hero.vy) vert_int,
              vy <- if isNothing vert_int then hero.vy - t/4 -- gravity
                    else if dir_y>0 then 5 -- jump
                    else 0, -- stand
              vx <- toFloat dir_x,    -- walking speed
              dir <- if | dir_x < 0     -> Left
                        | dir_x > 0     -> Right
                        | otherwise   -> hero.dir
     } 

step: Float -> (Int, Int) -> Game -> Hero-> Hero
step t dir g = physics t dir g



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
      ++ [ (if game.state == Before then toForm (plainText "Press Run to Start") 
                                    else (toForm (image (round hero.size) (round hero.size) src) |> move (hero.x, hero.y + hero.size/2 - h/2)))]
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

--main  = lift2 render Window.dimensions gameState


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



main = lift (\json->asText (smart_text_obtainer json)) code_port


-- Send JSon to JS as dict with hero_x, hero_y
port messageOut : Signal Json.Value
port messageOut =  lift (\(x,y)->(Json.Object (Dict.fromList[
    ("hero_x", Json.Number (toFloat x)),
    ("hero_y", Json.Number (toFloat y))
  ])))  Mouse.position

module Game where 

import Keyboard
import Window
import Mouse
import Json
import Dict
import Array


import Debug


big_eps = 7
eps = 0.0001


type Level = {groundx: [Float], groundy: [Float]}
type Levels = Array.Array Level

type Segment = (Float, (Float, Float))

first = {groundx = [400, 500, 800, 850], groundy = [50, 80, 50, 20, 100]}
--first = {groundx=[], groundy=[50]}

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
   first 
   ]


get_level: Int->Array.Array Level->Level 
get_level level_num levels = 
  Array.getOrElse first level_num levels --todo handle maybe
  

groundBlocks: Float->Level->[Segment]
groundBlocks w level = 
      (zip (level.groundy) (zip (0 :: level.groundx) (level.groundx ++ [w])  )) 


intersectsHor: Float->Float->Float->Float->Segment->Bool
intersectsHor x y w h (y', (x1, x2)) = (y - h + big_eps<y') && (((x+w>x1 - eps) && (x+ w<x2+eps)) || ((x-w>x1-eps) && (x-w<x2+eps)))

intersectsVer: Float->Float->Float->Float->Segment->Bool
intersectsVer x y w h (y', (x1, x2)) = (y - h<y'+eps) && (((x+w>x1+big_eps) && (x+w<x2-big_eps)) || ((x-w>x1+big_eps) && (x-w<x2-big_eps)))  


intersectBlocksHor: Float->Float->Float->Float->[Segment]->Maybe Segment
intersectBlocksHor x y w h blocks = 
 let l = filter (intersectsHor x y w h) blocks
 in if isEmpty l then Nothing else Just (head l)


intersectBlocksVer: Float->Float->Float->Float->[Segment]->Maybe Segment
intersectBlocksVer x y w h blocks = 
 let l = filter (intersectsVer x y w h) blocks
 in if isEmpty l then Nothing else Just (head l)




-- Game state

data State = Before | Playing | Finished


-- Game world description

data Action = Forward | Backward | Jump | None

data Direction = Right | Left


type Hero = {x: Float, y: Float, vx: Float, vy: Float, dir: Direction, w: Float, h: Float}

hero = { x=0, y=0, vx=0, vy=0, dir=Right, w = 20, h = 35}


type Game = {state: State, level_num: Int, hero: Hero, levels: Levels, w: Float, h:Float}


defaultGame: Game
defaultGame = {state = Before, level_num=0, hero=hero, levels = levels, w = 0, h = 0}


actionToArrows: Action->(Int, Int)
actionToArrows action = 
     case action of
        Forward -> (1, 0)
        Backward -> (-1, 0)
        Jump -> (1, 1)
        None -> (0, 0) -- Don't move 


defaultHero: Level->Hero
defaultHero level = 
  {x = 100, y = 50 + (head level.groundy), vx=0, vy=0, dir=Right, w = 20, h=35}


dead: Float->Float->Float->Float->Level->Bool
dead x y w h level = 
  if y<0 then True else False


win: Float->Float->Float->Float->Level->Bool
win x y w h level = 
   if x>w then True else False


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


move_hor: Float->Float->Float->Float->Maybe Segment->Float
move_hor x dx w h seg = case seg of
                     Nothing -> x + dx
                     Just (y, (x1, x2)) -> if | dx<0 && (x-w<x2-eps) -> max (x2+w) (x + dx)
                                              | dx>0 && (x+w>x1+eps) -> min (x1-w) (x + dx)
                                              | otherwise -> x + dx 
move_vert: Float->Float->Float->Float->Maybe Segment->Float
move_vert y dy w h seg = case seg of
                     Nothing -> y + dy
                     Just (y', (x1, x2)) -> max (y'+h) (y + dy)
                                             


physics: Float->(Int, Int)->Game->Hero->Hero
physics t (dir_x, dir_y) g hero =  
  let 
      w = hero.w/2
      h = hero.h/2
      b = groundBlocks g.w (cur_level g)
      vert_int = intersectBlocksVer hero.x (hero.y + t*hero.vy) w h b
      hor_int  = intersectBlocksHor (hero.x + t*hero.vx) hero.y w h b
  in { hero | x <- move_hor hero.x (t*hero.vx) w h hor_int,
              y <- move_vert hero.y (t*hero.vy) w h vert_int,
              vy <- if isNothing vert_int then hero.vy - t/4 -- gravity
                     else if dir_y>0 then 5
                     else 0 -- stand 
                    ,     
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
      -- src  = "imgs/man/" ++ verb ++ "/" ++ (if hero.dir == Right then "right" else "left") ++ ".gif"
      src  = "imgs/man/walk/" ++ (if hero.dir == Right then "right" else "left") ++ ".gif"
  in collage w' h'
      ([ rect w h  |> filled (rgb 174 238 238)]
      ++ (drawGround w h level) 
      --, rect w 50 |> filled (rgb 74 163 41)
                  --|> move (0, 24 - h/2)
      ++ [ (if game.state == Before then toForm (plainText "Press Run to Start") 
                                    else (toForm (image (round hero.h) (round hero.h) src) |> move (hero.x - w/2, hero.y - h/2 - 2)))]
      )

encodeArrows {x, y} = if | x >0 -> Forward
                         | x <0 -> Backward
                         | y >0 -> Jump
                         | otherwise -> None
 

type Input = {delta: Float, action: Action, w: Int, h: Int}

newinput:Float->Action->(Int, Int)->Input
newinput delta action (w,h) = {delta = delta, action =  action, w = w, h = h}

-- Input 

input: Signal Input
input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift3 newinput delta (lift record_to_action code_port) Window.dimensions)
        --in sampleOn delta (lift3 newinput delta (lift encodeArrows Keyboard.arrows) Window.dimensions)


--- Main --- 
main  = lift2 render Window.dimensions gameState

record_to_action: ({action:String, direction:String})->Action
record_to_action rec = 
  if | rec.action == "forward" -> Forward
     | rec.action == "jump" -> Jump
     | otherwise -> None

-- incoming code source for player control

port code_port : Signal ({action:String, direction:String})


-- This function is exported to python as _game.summarize (see its usages in game.py)
port summarize: Int -> Int -> Int
port summarize = (\x y -> x + y * 3) -- 3 here is to show it works in elm :)


obstacle_front g = 
   let s = g.hero.h/2
       b = groundBlocks g.w (cur_level g)
       hor_int  = intersectBlocksHor hero.x hero.y (hero.w/2) (hero.h/2)  b
   in not (isNothing hor_int)

-- Send Record
port messageOut : Signal ({ hero_x:Int, hero_y:Int, obstacle_front:Bool, obstacle_back:Bool, obstacle_top:Bool })
port messageOut =  lift (\g -> Debug.log "Data" {hero_x = round g.hero.x,  hero_y = round g.hero.y, obstacle_front = obstacle_front g, obstacle_back = False, obstacle_top = False}) gameState



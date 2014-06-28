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


type Level = {image: {x: Int, y: Int, src: String}, playerx : Float, playery: Float, groundx: [Float], groundy: [Float], water: [Segment]}
type Levels = Array.Array Level

type Segment = (Float, (Float, Float))

first_level = {image = {x = 751, y = 302, src="imgs/levels/1.jpg"}, playerx = 100, playery = 310, groundx = [200, 270, 450, 500], groundy = [100, 150, 100, 0, 100], water = [(90, (450, 499))]}
end_level = {image = {x = 751, y = 302, src="imgs/end.jpg"}, playerx = 100, playery = 300, groundx = [], groundy = [], water = []}

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
   first_level, 
   end_level 
   ]


get_level: Int->Array.Array Level->Level 
get_level level_num levels = 
  Array.getOrElse end_level level_num levels --todo handle maybe
  

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

data Action = Start | Forward | Backward | Jump | None

data Direction = Right | Left


type Hero = {x: Float, y: Float, vx: Float, vy: Float, dir: Direction, w: Float, h: Float, falling: Bool, action: Action}

defHero = { x=0, y=0, vx=0, vy=0, dir=Right, w = 30, h = 46, falling = False, action = None}


type Game = {state: State, level_num: Int, hero: Hero, levels: Levels, w: Float, h:Float, time: Float}


defaultGame: Game
defaultGame = {state = Before, level_num=0, hero=defHero, levels = levels, w = 0, h = 0, time = 0}


actionToArrows: Action->(Int, Int)
actionToArrows action = 
     case action of
        Forward -> (1, 0)
        Backward -> (-1, 0)
        Jump -> (1, 1)
        None -> (0, 0) -- Don't move 


defaultHero: Level->Hero
defaultHero level = 
  {x = level.playerx, y = level.playery, vx=0, vy=0, dir=Right, w = 30, h=46, falling=False, action = None}


dead: Float->Float->Float->Float->Level->Bool
dead x y w h level = 
  if y<0 then True else False


win: Float->Float->Float->Float->Level->Bool
win x y w h level = 
   if x>(w-20) then True else False


cur_level: Game->Level
cur_level g = 
    get_level g.level_num g.levels

isEndLevel g = g.state == Playing && g.level_num == (Array.length g.levels) - 1


stepGame : Input -> Game -> Game
stepGame input ({state, level_num, hero, levels} as game) =
  let level = get_level level_num levels
      w = toFloat input.w
      h = toFloat input.h
      action = input.action
      delta = input.delta
      
  in  {game| state   <- if | state == Before && action /= None -> Playing
                           | isEndLevel game -> Finished
                           | otherwise        -> state
           , hero    <- if | state == Before -> defaultHero level
                           | state == Playing && action == Start -> defaultHero level
                           | isEndLevel game -> defaultHero level
                           | state == Playing -> 
                                  if dead hero.x hero.y w h level then defaultHero level 
                                  else step delta action game hero 
                           | state == Finished -> 
                                  if dead hero.x hero.y w h level then defaultHero level 
                                  else step delta None game hero
                           | otherwise -> hero
           , level_num   <- if state == Playing && (win hero.x hero.y w h level) then level_num + 1 else level_num
           , w <- w
           , h <- h
           , time <- game.time + input.delta
      }

gameState = foldp stepGame defaultGame input


-- Physics --


move_hor: Float->Float->Float->Float->Maybe Segment->Float
move_hor x dx w h seg = case seg of
                     Nothing -> x + dx
                     Just (y, (x1, x2)) -> if | dx<0 && (x-w<x2-eps) && (x2-x<w) -> max (x2+w) (x + dx)
                                              | dx>0 && (x+w>x1+eps) && (x-x1<w)-> min (x1-w) (x + dx)
                                              | otherwise -> x + dx 
move_vert: Float->Float->Float->Float->Maybe Segment->Float
move_vert y dy w h seg = case seg of
                     Nothing -> y + dy
                     Just (y', (x1, x2)) -> max (y'+h) (y + dy)
                                             


physics: Float->Action->Game->Hero->Hero
physics t a g hero =  
  let 
      (dir_x, dir_y) = actionToArrows a
      w = hero.w/2
      h = hero.h/2
      b = groundBlocks g.w (cur_level g)
      vert_int = intersectBlocksVer hero.x (hero.y + t*hero.vy) w h b -- barrier for vertical move
      hor_int  = intersectBlocksHor (hero.x + t*hero.vx) hero.y w h b -- barrier for horizontal move
  in { hero |  x <- if isNothing vert_int && not (hero.action == Jump) then hero.x else 
                   (move_hor hero.x (t*hero.vx) w h hor_int)
              ,y <- move_vert hero.y (t*hero.vy) w h vert_int
              ,vy <- if isNothing vert_int then hero.vy - t/4 -- gravity
                     else if dir_y>0 then 5
                     else 0           -- stand      
              ,vx <- (toFloat dir_x)*1.5    -- walking speed
              ,dir <- if | dir_x < 0     -> Left
                         | dir_x > 0     -> Right
                         | otherwise   -> hero.dir
              ,falling <- isNothing vert_int && (hero.vy < -1.5)
              ,action  <- if not (isNothing vert_int) then a else hero.action 
     } 

step: Float -> Action -> Game -> Hero-> Hero
step t a g = physics t a g


drawSegments: Float -> Float -> Color -> [Segment] -> [Form]
drawSegments w h c s= map (\(y, (x1, x2)) -> rect (x2-x1) y
                            |> filled c
                            |> move (x1 + (x2-x1)/2 - w/2, y/2 - h/2)
                            )
                          s

drawImage: String ->Int->Int->Shape -> Form
drawImage src w' h' shp = toForm(image w' h' src)

--TODO: Copy/paste
drawSegmentsTexture: Float -> Float -> String -> (Int, Int)-> [Segment] -> [Form]
drawSegmentsTexture w h str (w', h') s= map (\(y, (x1, x2)) -> rect (x2-x1) y
                            |> drawImage str w' h'
                            |> move (x1 + (x2-x1)/2 - w/2, y/2 - h/2)
                            )
                          s

--drawGround: Float->Float->Level -> [Form]
--drawGround w h l = drawSegments w h (rgb 74 163 41) (groundBlocks w l)


drawWater: Float->Float->[Segment]->Bool -> [Form]
drawWater w h s rnd = map (alpha 0.4) (drawSegmentsTexture w h (if rnd then "imgs/water.png" else "imgs/water2.png" ) (54,90) s)


displayText: Float->Float->String -> Form
displayText w h s = toText s |> Text.color (rgb 160 200 160) |> monospace |> (Text.height 40) |> leftAligned |> toForm |> move (0,  h/4) 

-- DISPLAY
render: (Int, Int)->Game->Element
render (w',h') game =
  let hero = game.hero
      level = cur_level game
      (w,h) = (toFloat w', toFloat h')
      verb = if | (hero.falling) -> "jump"
                | hero.vx /= 0  -> "walk"
                | otherwise     -> "stand"
      src  = "imgs/man/" ++ verb ++ "/" ++ (if hero.dir == Right then "right" else "left") ++ ".gif"
  in collage w' h'
      ([(toForm (image level.image.x level.image.y level.image.src))]

      --, rect w 50 |> filled (rgb 74 163 41)
                  --|> move (0, 24 - h/2)
      ++ [ (if game.state == Before then displayText w h "Press Start"
                                    else (toForm (image (round hero.w) (round hero.h) src) |> move (hero.x - w/2, hero.y - h/2 - 2)))]
      ++ drawWater w h level.water ((rem (round game.time) 2) == 0)
      ++ if game.state == Finished then [displayText w h "You Win"] else []
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
input = let delta = lift (\t -> (min (40 + (toFloat (rem (round t) 2))) t)/20) (fps 25)
        in sampleOn delta (lift3 newinput delta (lift record_to_action code_port) Window.dimensions)
        --in sampleOn delta (lift3 newinput delta (lift encodeArrows Keyboard.arrows) Window.dimensions)


--- Main --- 
main  = lift2 render Window.dimensions gameState

record_to_action: ({action:String, direction:String})->Action
record_to_action rec = 
  if | rec.action == "forward" -> Forward
     | rec.action == "jump" -> Jump
     | rec.action == "start" -> Start
     | otherwise -> None

-- incoming code source for player control

port code_port : Signal ({action:String, direction:String})


-- This function is exported to python as _game.summarize (see its usages in game.py)
port summarize: Int -> Int -> Int
port summarize = (\x y -> x + y * 3) -- 3 here is to show it works in elm :)

-- This function is exported to python
--port pick_material: Signal (Int -> Int -> String)
--port pick_material = lift (\g x y -> (if not  (isNothing (intersectBlocksHor (toFloat x) (toFloat y) big_eps big_eps (cur_level g).water))  then "WATER" else "STONE")) gameState


blockAt g (x, y) blocks = 
   let 
       hero = g.hero
       seg  = intersectBlocksHor (hero.x + hero.w*x)  (hero.y + hero.h*y) big_eps big_eps blocks
   in not (isNothing seg)

groundAt g (x, y) = blockAt g (x, y) (groundBlocks g.w (cur_level g))

waterAt g (x, y)= blockAt g (x, y) (cur_level g).water
   

material g = 
  map (\(x, y) -> if groundAt g (x, y) then "GROUND" else if waterAt g (x, y) then "WATER" else "NOTHING") 
      [(1, 1), (1, 0), (1, -1)]

-- Send Record
port messageOut : Signal ({ hero_x:Int, hero_y:Int, material: [String] })
port messageOut =  lift (\g -> {hero_x = round g.hero.x,  hero_y = round g.hero.y, material = material g}) gameState



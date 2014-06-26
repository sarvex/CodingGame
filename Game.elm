module Game where 

import Keyboard
import Window
import Mouse
import Json
import Dict

-- incoming code source for player control

port code_port : Signal Json.Value

-- MODEL
mario = { x=0, y=0, vx=0, vy=0, dir="right" }


-- UPDATE -- ("m" is for Mario)
jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

step (t,dir) = physics t . walk dir . gravity t . jump dir


-- DISPLAY
render (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | mario.y  >  0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src  = "imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in collage w' h'
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
      , toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)
      ]

-- MARIO
input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

--main  = lift2 render Window.dimensions (foldp step mario input)


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
port messageOut : Signal Int
port messageOut = Mouse.x

import game

def hero_action(game_info):
   if game_info.obstacle_front:
        return game.Jump()
    else:
        return game.Forward()

import game
def hero_action(game_info):
    """

    :type game_info game.GameInfo
    """
    return game.Jump("left") if game_info.hero_x > 200 else game.Left()
import game


def hero_action(game_info):
    """

    :type game_info game.GameInfo
    """
    if game_info.obstacle_front:
        return game.Jump()
    else:
        return game.Forward()

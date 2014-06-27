import game


def hero_action(game_info):
    """

    :type game_info game.GameInfo
    """
    if not game_info.obstacle_top:
        return game.Jump()
    if not game_info.obstacle_front:
        return game.Jump(game.LEFT)
    return game.Right()

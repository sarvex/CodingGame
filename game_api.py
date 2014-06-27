import _game

LEFT = "left"
TOP = "top"
RIGHT = "right"


class Command(object):
    command_name = None
    direction = None

    def _get_dict(self):
        clz = self.__class__
        return {"action": clz.command_name, "direction": clz.direction}


class Forward(Command):
    command_name = "forward"
    direction = "forward"


class Jump(Command):
    def __init__(self, direction=TOP):
        self.direction = direction

    def _get_dict(self):
        return {"action": "jump", "direction": self.direction}


# class Left(Command):
#     command_name = "turn"
#     direction = LEFT
#
#
# class Right(Command):
#     command_name = "turn"
#     direction = RIGHT


class Gps(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y


class GameInfo(object):
    def __init__(self, params_dic):
        self.gps = Gps(params_dic['hero_x'], params_dic['hero_y'])
        self.obstacle_front = params_dic['obstacle_front']
        self.obstacle_back = params_dic['obstacle_back']
        self.obstacle_top = params_dic['obstacle_top']

    def get_coords_sum(self):
        return _game.summarize(self.hero_x, self.hero_y)
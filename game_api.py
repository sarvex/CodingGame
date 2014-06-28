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
# command_name = "turn"
#     direction = LEFT
#
#
# class Right(Command):
#     command_name = "turn"
#     direction = RIGHT


class Gps(object):
    def __init__(self, params_dic):
        self.x = params_dic['hero_x']
        self.y = params_dic['hero_y']


class Sensors(object):
    def __init__(self, params_dic):
        self.obstacle_front = params_dic['obstacle_front']

    def is_barrier_near(self):
        return self.obstacle_front


class GameInfo(object):
    def __init__(self, params_dic):
        self.gps = Gps(params_dic)
        self.sensors = Sensors(params_dic)

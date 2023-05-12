import _elm

LEFT = "left"
TOP = "top"
RIGHT = "right"


class Command(object):
    command_name = None
    direction = None

    def _get_dict(self):
        clz = self.__class__
        return {"action": clz.command_name, "direction": clz.direction}


class Run(Command):
    command_name = "forward"
    direction = "forward"

class Stop(Command):
    command_name = "stop"
    direction = "stop"

class Jump(Command):
    def __init__(self, direction=TOP):
        self.direction = direction

    def _get_dict(self):
        return {"action": "jump", "direction": self.direction}


# class Left(Command):
# command_name = "turn"
# direction = LEFT
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
        # (1, 1), (1, 0), (1, -1)
        self.material = params_dic['material']

    def is_blocked(self):
        return self.pick_material(1, 0) == 'GROUND'

    def pick_material(self, x, y):
        if x == 1:
            if y == 1:
                return self.material[0]
            if y == 0:
                return self.material[1]
            if y == -1:
                return self.material[2]
        return 'UNKNOWN'


class Laser(object):
    def __init__(self, params_dic):
        pass

    def fire(self):
        raise AttributeError("Laser is broken")


class Robo(object):
    def __init__(self, params_dic):
        self.gps = Gps(params_dic)
        self.sensors = Sensors(params_dic)
        self.laser = Laser(params_dic)

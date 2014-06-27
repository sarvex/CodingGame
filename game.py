class Command(object):
    command_name = None
    direction = None

    def _get_dict(self):
        clz = self.__class__
        return {"action": clz.command_name, "direction": clz.direction}


class Jump(Command):
    def __init__(self, direction):
        self.direction = direction

    def _get_dict(self):
        return {"action": "jump", "direction": self.direction}


class Left(Command):
    command_name = "turn"
    direction = "left"


class Right(Command):
    command_name = "turn"
    direction = "right"

class Command(object):
    command_name = None
    direction = None

    def _get_dict(self):
        clz = self.__class__
        return {"action": clz.command_name, "direction": clz.direction}


class Jump(Command):
    command_name = "jump"
    direction = "up"


class Left(Command):
    command_name = "turn"
    direction = "left"


class Right(Command):
    command_name = "turn"
    direction = "right"
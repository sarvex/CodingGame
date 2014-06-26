class Command(object):
    command_name = None
    direction = None


class Jump(Command):
    command_name = "jump"
    direction = "up"


class Left(Command):
    command_name = "turn"
    direction = "left"


class Right(Command):
    command_name = "turn"
    direction = "right"


class Logic(object):
    def get_command(self, mouse_x):
        pass


    def _get_dict(self, mouse_x):
        commmand = self.get_command(mouse_x)
        return {"action": commmand.command_name, "direction": commmand.direction}

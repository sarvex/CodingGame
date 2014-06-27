from game_api import *


def robot_action(robot):
    print robot.gps.x
    if robot.gps.x > 100:
        return Jump()
    else:
        return Forward()

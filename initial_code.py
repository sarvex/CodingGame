from game_api import *


def robot_action(robot):
    print robot.gps.x
    if robot.sensors.is_barrier_near() or robot.gps.x > 446:
        return Jump()
    else:
        return Forward()

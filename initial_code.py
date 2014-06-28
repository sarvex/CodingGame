from game_api import *


def robot_action(robot):
    # print robot.gps.x

    if robot.sensors.is_barrier_near() or robot.sensors.pick_material(1, -1) == 'WATER':
        return Jump()
    else:
        return Forward()

import numpy as np
import janus_swi as janus

class RoutePlanner():
    # variables
    # map -> numpy array that displays the current map

    def __init__(self, map):
        janus.consult("./resources/route_reasoner.pl")
        self.map = map

    def set_current_pos(self, pos):
        janus.query_once("retractall(robot(_, _))", {}) #todo
        res = janus.query_once("assertz(robot())", {}) #todo
        return res["truths"]

    def get_route(self, goal):
        return nil

    def get_route_for_load(self, goal, type):
        return nil

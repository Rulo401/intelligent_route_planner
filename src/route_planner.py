import numpy as np
import janus_swi as janus

def get_floor_type(cell):
    if cell == 1:
        return "smooth"
    elif cell == 2:
        return "uneven"
    elif cell == 3:
        return "mesh"
    elif cell == 4:
        return "carpet"

class RoutePlanner():
    def __init__(self, map):
        janus.consult("../resources/route_reasoner.pl")
        
        x, y = 0, 0
        for row in map:
            for cell in row:
                if cell != 0:
                    janus.query_once("assertz(cell(X,Y))", { "X" : x, "Y": y})
                    janus.query_once("assertz(floor_type(cell(X,Y), Type))", { "X": x, "Y": y, "Type": get_floor_type(cell)})
                x += 1

            x = 0
            y += 1

    def set_current_pos(self, x, y):
        janus.query_once("retractall(located_at(robot, _))", {})
        res = janus.query_once("assertz(located_at(robot, cell(X,Y)))", { "X": x, "Y": y })
        return res["truth"]

    def get_route(self, goal):
        return nil

    def get_route_for_load(self, goal, type):
        return nil

if __name__ == "__main__":
    map = [
        [0, 1, 1, 4],
        [1, 1, 0, 1],
        [1, 0, 2, 2],
        [3, 1, 2, 0]
    ]

    planner = RoutePlanner(map)
    planner.set_current_pos(0,3)
    print("--- SMOOTH CELLS ---")
    q = janus.query("floor_type(cell(X,Y),smooth)")
    while ( s := q.next() ):
        print(s['X'], s['Y'])
    q.close()

    print("--- UNEVEN CELLS ---")
    q = janus.query("floor_type(cell(X,Y),uneven)")
    while ( s := q.next() ):
        print(s['X'], s['Y'])
    q.close()

    print("--- MESH CELLS ---")
    q = janus.query("floor_type(cell(X,Y),mesh)")
    while ( s := q.next() ):
        print(s['X'], s['Y'])
    q.close()

    print("--- CARPET CELLS ---")
    q = janus.query("floor_type(cell(X,Y),carpet)")
    while ( s := q.next() ):
        print(s['X'], s['Y'])
    q.close()

    print("--- ROBOT LOCATION ---")
    q = janus.query("located_at(robot,cell(X,Y))")
    while ( s := q.next() ):
        print(s['X'], s['Y'])
    q.close()

    # goal = (3,0)
    # planner.get_route(goal)

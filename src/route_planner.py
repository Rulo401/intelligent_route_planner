import numpy as np
import janus_swi as janus

import matplotlib
matplotlib.use('TkAgg')

import matplotlib.pyplot as plt
from matplotlib import colors
from matplotlib.patches import Patch
from matplotlib.lines import Line2D

import itertools

def get_floor_type(cell):
    if cell == 1:
        return "smooth"
    elif cell == 2: 
        return "uneven"
    elif cell == 3:
        return "mesh"
    elif cell == 4:
        return "carpet"

def code_floor_type(cell):
    if cell == "smooth":
        return 1
    elif cell == "uneven":
        return 2
    elif cell == "mesh":
        return 3
    elif cell == "carpet":
        return 4
    
def get_zone_type(cell):
    if cell == 1:
        return "common"
    elif cell == 2: 
        return "vehicles_only"
    elif cell == 3:
        return "robots_only"
    
def code_zone_type(cell):
    if cell == "common":
        return 1
    elif cell == "vehicles_only":
        return 2
    elif cell == "robots_only":
        return 3

class RoutePlanner():
    # Variables
    #   max_x: map x dimension
    #   max_y: map y dimension
    # Plot variables:
    #   fig: plot figure
    #   ax: plot artist
    #   cmap: color map
    #   norm: color normalization
    def __init__(self, map, doors=[], barriers=[], items=[], zones=[], conveyors=[]):
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

        x, y = 0, 0
        for row in zones:
            for cell in row:
                if cell != 0:
                    janus.query_once("assertz(zone_type(cell(X,Y), Type))", { "X": x, "Y": y, "Type": get_zone_type(cell)})
                x += 1

            x = 0
            y += 1

        self.max_y, self.max_x = map.shape

        for door, start, end in doors:
            if start == end:
                raise Exception(f"Door {door} must be at least 2 cells long")
            elif start[1] == end[1]:
                limits = sorted([start[0], end[0]])
                for x in range(limits[0], limits[1]+1):
                    janus.query_once("assertz(located_at(door(Name), cell(X,Y)))", { "Name" : door, "X" : x, "Y": start[1]})
            elif start[0] == end[0]:
                limits = sorted([start[1], end[1]])
                for y in range(limits[0], limits[1]+1):
                    janus.query_once("assertz(located_at(door(Name), cell(X,Y)))", { "Name" : door, "X" : start[0], "Y": y})
            else:
                raise Exception(f"Door {door} is not vertical or horizontal")
            
        for barrier, start, end in barriers:
            if start == end:
                raise Exception(f"Barrier {barrier} must be at least 2 cells long")
            elif start[1] == end[1]:
                limits = sorted([start[0], end[0]])
                for x in range(limits[0], limits[1]+1):
                    janus.query_once("assertz(located_at(barrier(Name), cell(X,Y)))", { "Name" : barrier, "X" : x, "Y": start[1]})
            elif start[0] == end[0]:
                limits = sorted([start[1], end[1]])
                for y in range(limits[0], limits[1]+1):
                    janus.query_once("assertz(located_at(barrier(Name), cell(X,Y)))", { "Name" : barrier, "X" : start[0], "Y": y})
            else:
                raise Exception(f"Barrier {barrier} is not vertical or horizontal")

        for item_type, name, x, y in items:
            if item_type == "key":
                janus.query_once("assertz(located_at(key(Name), cell(X,Y)))", { "Name" : name, "X" : x, "Y": y})
            elif item_type == "switch":
                janus.query_once("assertz(located_at(switch(Name), cell(X,Y)))", { "Name" : name, "X" : x, "Y": y})

        for name, length, (x1, y1), (x2, y2) in conveyors:
            janus.query_once("assertz(conveyor(Name,Length))", { "Name" : name, "Length" : length })
            janus.query_once("assertz(entry(Name,cell(X,Y)))", { "Name" : name, "X" : x1, "Y": y1 })
            janus.query_once("assertz(exit(Name,cell(X,Y)))", { "Name" : name, "X" : x2, "Y": y2 })

        # Setup plot
        plt.ion()
        self.fig, self.ax = plt.subplots(figsize=(6, 6))
    
        # Personalized colormap (Wall=Black, Smooth=White, Uneven=Wheat, Mesh=Gray, Carpet=IndianRed)
        self.cmap = colors.ListedColormap(['black', 'white', 'wheat', 'gray', 'indianred'])
        bounds = [0, 1, 2, 3, 4, 5]
        self.norm = colors.BoundaryNorm(bounds, self.cmap.N)
    
    def get_current_map(self):
        q = janus.query("floor_type(cell(X,Y),Type)")
        data = np.zeros((self.max_y, self.max_x))

        while ( s := q.next() ):
            data[s['Y'], s['X']]=code_floor_type(s['Type'])
        q.close()

        return data
    
    def get_current_zones(self):
        q = janus.query("zone_type(cell(X,Y),Type)")
        data = {}

        while ( s := q.next() ):
            data[(s['X'], s['Y'])]=s['Type']
        q.close()

        return data
    
    def get_doors(self):
        q = janus.query("located_at(door(D), cell(X,Y))")
        door_cells = []
        while ( s := q.next() ):
           door_cells.append((s["D"], (s["X"], s["Y"])))
        q.close()

        doors = []
        
        for door, cells in itertools.groupby(door_cells, lambda x: x[0]):
            cells = [cell for d, cell in cells]
            cells = sorted(cells)
            doors.append((door, cells[0], cells[-1]))

        return doors
    
    def get_barriers(self):
        q = janus.query("located_at(barrier(B), cell(X,Y))")
        barrier_cells = []
        while ( s := q.next() ):
           barrier_cells.append((s["B"], (s["X"], s["Y"])))
        q.close()

        barriers = []
        
        for barrier, cells in itertools.groupby(barrier_cells, lambda x: x[0]):
            cells = [cell for d, cell in cells]
            cells = sorted(cells)
            barriers.append((barrier, cells[0], cells[-1]))

        return barriers
    
    def get_current_items_location(self):
        keys = self._get_keys()
        switches = self._get_switches()

        return { "keys" : keys, "switches" : switches }
    
    def _get_keys(self):
        q = janus.query("located_at(key(K), cell(X,Y))")
        keys = []
        while ( s := q.next() ):
           keys.append((s["K"], (s["X"], s["Y"])))
        q.close()

        return keys
    
    def _get_switches(self):
        q = janus.query("located_at(switch(S), cell(X,Y))")
        switches = []
        while ( s := q.next() ):
           switches.append((s["S"], (s["X"], s["Y"])))
        q.close()

        return switches
    
    def get_conveyors_locations(self):
        conveyors = []
        q = janus.query("conveyor(Name,L), entry(Name,cell(EntryX,EntryY)), exit(Name,cell(ExitX,ExitY))")
        while (s := q.next()):
            conveyors.append((s["Name"], s["L"], (s["EntryX"],s["EntryY"]), (s["ExitX"],s["ExitY"])))
        q.close()

        return conveyors

    def get_current_pos(self):
        res = janus.query_once("located_at(robot, cell(X,Y))")
        if res["truth"]:
            return res["X"], res["Y"]

        return None
    
    def get_current_goal(self):
        res = janus.query_once("located_at(goal, cell(X,Y))")
        if res["truth"]:
            return res["X"], res["Y"]

        return None

    def set_current_pos(self, x, y):
        janus.query_once("retractall(located_at(robot, _))", {})
        res = janus.query_once("assertz(located_at(robot, cell(X,Y)))", { "X": x, "Y": y })
        return res["truth"]
    
    def set_goal(self, x, y):
        janus.query_once("retractall(located_at(goal, _))", {})
        res = janus.query_once("assertz(located_at(goal, cell(X,Y)))", { "X": x, "Y": y })
        return res["truth"]

    def get_route_for_load(self, type="standard"):
        self.current_load = type   
        q = janus.query_once("route(LoadType, Path)", { "LoadType" : type })
        return q["Path"]

    def print_current_map(self, route=None):
        # Load map
        map = self.get_current_map()

        # Show base grid
        self.ax.clear()
        self.ax.imshow(map, cmap=self.cmap, norm=self.norm, origin='upper')

        # Adding visual mesh between cells
        self.ax.grid(which='major', axis='both', linestyle='-', color='k', linewidth=1, alpha=0.3)
        self.ax.set_xticks(np.arange(-.5, self.max_x, 1)); self.ax.set_yticks(np.arange(-.5, self.max_y, 1))
        self.ax.set_xticklabels([]); self.ax.set_yticklabels([]) #hide numbers

        # Show zones
        for (x, y), zone_type in self.get_current_zones().items():
            if zone_type == 'common':
                rect = matplotlib.patches.Rectangle(
                    (x - 0.5, y - 0.5), 1, 1, 
                    linewidth=1, edgecolor='black', facecolor='none', 
                    hatch='..', alpha=0.3, zorder=5)
                self.ax.add_patch(rect)

            elif zone_type == 'vehicles_only':
                rect = matplotlib.patches.Rectangle(
                    (x - 0.5, y - 0.5), 1, 1, 
                    linewidth=1, edgecolor='black', facecolor='none', 
                    hatch='///', alpha=0.3, zorder=5)
                self.ax.add_patch(rect)
                
            elif zone_type == 'robots_only':
                rect = matplotlib.patches.Rectangle(
                    (x - 0.5, y - 0.5), 1, 1, 
                    linewidth=1, edgecolor='black', facecolor='none', 
                    hatch='OO', alpha=0.3, zorder=5)
                self.ax.add_patch(rect)

        # Show route
        if route:
            path_x = []
            path_y = []
            for x,y in route:
                path_x.append(x)
                path_y.append(y)
            
            if path_x and path_y:
                self.ax.plot(path_x, path_y,
                             color='blue',
                             linewidth=4,
                             linestyle='-',
                             zorder=9
                             )
                
        # Show robot
        robot_pos = self.get_current_pos()
        if robot_pos:
            self.ax.plot(robot_pos[0], robot_pos[1], 
                marker='o',
                markersize=20,
                color='cyan',
                markeredgecolor='k',
                markeredgewidth=2,  
                zorder=10
            )

        # Show goal
        goal_pos = self.get_current_goal()
        if goal_pos:
            self.ax.plot(goal_pos[0], goal_pos[1], 
                marker='*',
                markersize=20,
                color='gold',
                markeredgecolor='k',
                markeredgewidth=2,  
                zorder=10
            )

        # Show items
        items = self.get_current_items_location()
        for key, pos in items["keys"]:
            self.ax.plot(pos[0], pos[1], 
                marker='d',
                markersize=20,
                color='gold',
                markeredgecolor='k',
                markeredgewidth=2,  
                zorder=10
            )

            self.ax.text(pos[0], pos[1], key[:1], 
                    ha='center', va='center', 
                    color='black', fontweight='bold',
                    zorder=11)
            
        for switch, pos in items["switches"]:
            self.ax.plot(pos[0], pos[1], 
                marker='s',
                markersize=20,
                color='grey',
                markeredgecolor='k',
                markeredgewidth=2,  
                zorder=10
            )

            self.ax.text(pos[0], pos[1], switch[:1], 
                    ha='center', va='center', 
                    color='black', fontweight='bold',
                    zorder=11)
        
        # Show conveyors 
        for name, length, (ex, ey), (sx, sy) in self.get_conveyors_locations():
            self.ax.plot(ex, ey, marker='h', markersize=18, color="darkviolet", markeredgecolor='black', markeredgewidth=2, zorder=8)
            self.ax.text(ex, ey, "IN", ha='center', va='center', color='white', fontsize=7, fontweight='bold', zorder=9)
            
            self.ax.plot(sx, sy, marker='H', markersize=18, color="darkviolet", markeredgecolor='black', markeredgewidth=2, zorder=8)
            self.ax.text(sx, sy, "OUT", ha='center', va='center', color='white', fontsize=6, fontweight='bold', zorder=9)

            self.ax.annotate("", 
                             xy=(sx, sy), xycoords='data',
                             xytext=(ex, ey), textcoords='data',
                             arrowprops=dict(arrowstyle="->", color="darkviolet", lw=2, linestyle='--', shrinkA=10, shrinkB=10),
                             zorder=7)
            
            mid_x, mid_y = (ex + sx) / 2, (ey + sy) / 2
            self.ax.text(mid_x, mid_y, f"{name[:1]}: {length}", color="darkviolet", fontsize=8, fontweight='bold', ha='center', va='center', zorder=9, bbox=dict(facecolor='white', alpha=0.7, edgecolor='none', pad=1))

        # Show doors
        for door, start, end in self.get_doors():
            c1, r1 = start
            c2, r2 = end
            
            # door center
            x_prom = (c1 + c2) / 2
            y_prom = (r1 + r2) / 2
            
            if c1 == c2: # Vertical
                self.ax.plot([x_prom, x_prom], [r1 - 0.4, r2 + 0.4], 
                        color='gold', linewidth=5, solid_capstyle='round', zorder=10)
                
                self.ax.text(x_prom + 0.3, y_prom, door, 
                    ha='center', va='center', 
                    color='black', fontweight='bold',
                    zorder=11)
            else: # Horizontal
                self.ax.plot([c1 - 0.4, c2 + 0.4], [y_prom, y_prom], 
                        color='gold', linewidth=5, solid_capstyle='round', zorder=10)
                
                self.ax.text(x_prom, y_prom + 0.3, door, 
                    ha='center', va='center', 
                    color='black', fontweight='bold',
                    zorder=11)
                
        # Show barriers
        for barrier, start, end in self.get_barriers():
            c1, r1 = start
            c2, r2 = end
            
            # barrier center
            x_prom = (c1 + c2) / 2
            y_prom = (r1 + r2) / 2
            
            if c1 == c2: # Vertical
                self.ax.plot([x_prom, x_prom], [r1 - 0.4, r2 + 0.4], 
                        color='darkorange', linewidth=5, solid_capstyle='round', zorder=10)
                
                self.ax.text(x_prom + 0.3, y_prom, barrier, 
                    ha='center', va='center', 
                    color='black', fontweight='bold',
                    zorder=11)
            else: # Horizontal
                self.ax.plot([c1 - 0.4, c2 + 0.4], [y_prom, y_prom], 
                        color='darkorange', linewidth=5, solid_capstyle='round', zorder=10)
                
                self.ax.text(x_prom, y_prom + 0.3, barrier, 
                    ha='center', va='center', 
                    color='black', fontweight='bold',
                    zorder=11)
                
        legend_elements = [
            # Floor types
            Patch(facecolor='white', edgecolor='black', label='Smooth'),
            Patch(facecolor='wheat', edgecolor='black', label='Uneven'),
            Patch(facecolor='gray', edgecolor='black', label='Mesh'),
            Patch(facecolor='indianred', edgecolor='black', label='Carpet'),
            Patch(facecolor='black', edgecolor='gray', label='Wall'),

            # Zones
            Patch(facecolor='none', edgecolor='black', label='Maneuver'),
            Patch(facecolor='none', edgecolor='black', hatch='..', label='Common'),
            Patch(facecolor='none', edgecolor='black', hatch='///', label='Vehicles Only'),
            Patch(facecolor='none', edgecolor='black', hatch='OO', label='Robots Only'),
            
            # Doors
            Line2D([0], [0], color='gold', lw=4, label='Door'),
            Line2D([0], [0], color='darkorange', lw=4, label='Barrier'),

            # Items
            Line2D([0], [0], marker='o', color='w', label='Robot',
                   markerfacecolor='cyan', markeredgecolor='k', markersize=10),
            Line2D([0], [0], marker='*', color='w', label='Goal',
                   markerfacecolor='gold', markeredgecolor='k', markersize=12),
            Line2D([0], [0], marker='d', color='w', label='Key',
                   markerfacecolor='gold', markeredgecolor='k', markersize=10),
            Line2D([0], [0], marker='s', color='w', label='Switch',
                   markerfacecolor='grey', markeredgecolor='k', markersize=10),

            # Conveyors
            Line2D([0], [0], marker='h', color='w', label='Conveyor IN', markerfacecolor='darkviolet', markeredgecolor='w', markersize=10),
            Line2D([0], [0], marker='H', color='w', label='Conveyor OUT', markerfacecolor='darkviolet', markeredgecolor='w', markersize=10),
            
            # Route
            Line2D([0], [0], color='blue', lw=2, label='Route')
        ]

        self.ax.legend(handles=legend_elements, loc='upper left', bbox_to_anchor=(1.05, 1), borderaxespad=0.)
        # --- Show load type below legend ---
        load_type = getattr(self, "current_load", "standard")

        load_style = {
            "standard":    {"color": "black",     "icon": "📦"},
            "fragile":     {"color": "goldenrod", "icon": "⚠"},
            "biochemical": {"color": "purple",    "icon": "☣"},
            "dangerous":   {"color": "red",       "icon": "☠"},
        }.get(load_type, {"color": "black", "icon": ""})

        self.ax.text(
            1.05, 0.55,                               # ↓ debajo de la leyenda
            f"{load_style['icon']} Load: {load_type.upper()}",
            transform=self.ax.transAxes,
            ha='left', va='bottom',
            fontsize=11,
            fontweight='bold',
            color=load_style["color"],
            bbox=dict(
                facecolor='white',
                edgecolor=load_style["color"],
                boxstyle='round,pad=0.35',
                alpha=0.95
            ),
            zorder=50
        )

        self.fig.tight_layout()

        plt.title("World map")
        plt.draw()
        plt.pause(0.5)

if __name__ == "__main__":
    map = [
        [0, 4, 4, 4, 4, 4, 4, 0],
        [0, 4, 4, 4, 4, 4, 4, 0],
        [1, 1, 1, 1, 0, 1, 1, 0],
        [1, 3, 1, 0, 0, 2, 2, 0],
        [1, 3, 1, 0, 0, 2, 2, 0],
        [1, 3, 1, 0, 0, 2, 2, 2],
        [1, 3, 1, 0, 0, 2, 2, 0],
        [1, 3, 1, 0, 2, 2, 2, 0],
        [1, 3, 1, 1, 2, 0, 0, 0],
        [1, 3, 1, 1, 2, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0]
    ]

    items = [
        ("key", "a", 4, 0),
        ("switch", "s", 1, 11)
    ]

    doors = [
        ("a", (5,2), (6,2))
    ]

    barriers = [
        ("s", (3,8), (3,9))
    ]

    zones = [
        [0, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 0],
        [2, 2, 1, 1, 0, 1, 1, 0],
        [2, 2, 3, 0, 0, 1, 1, 0],
        [2, 2, 3, 0, 0, 1, 1, 0],
        [2, 2, 3, 0, 0, 3, 3, 0],
        [2, 2, 3, 0, 0, 3, 3, 0],
        [2, 2, 3, 0, 3, 3, 3, 0],
        [3, 3, 3, 3, 3, 0, 0, 0],
        [3, 3, 3, 3, 3, 0, 0, 0],
        [3, 3, 3, 0, 0, 0, 0, 0],
        [3, 3, 3, 0, 0, 0, 0, 0]
    ]

    conveyors =[
        ("c", 15, (0,3), (7,5))
    ]
    current_load = "standard"

    planner = RoutePlanner(np.asarray(map, dtype=np.int32), doors, barriers, items, zones, conveyors)
    planner.set_current_pos(0,4)
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

    print("--- DOORS ---")
    q = janus.query("located_at(door(D),cell(X,Y))")
    while ( s := q.next() ):
        print(s['X'], s['Y'], s['D'])
    q.close()

    planner.set_goal(6,5)
    print("--- GOAL LOCATION ---")
    q = janus.query("located_at(goal,cell(X,Y))")
    while ( s := q.next() ):
        print(s['X'], s['Y'])
    q.close()

    q = janus.query_once("can_enter(cell(5,1), [])")
    print(f"Can enter door 5 1? {q}")
    q = janus.query_once("can_enter(cell(3,7), [])")
    print(f"Can enter door 3 7? {q}")

    route = planner.get_route_for_load("standard")
    print("--- ROUTE ---")
    print(route)

    planner.print_current_map(route)
    plt.ioff()
    plt.show()
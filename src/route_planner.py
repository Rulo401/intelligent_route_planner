import numpy as np
import janus_swi as janus

import matplotlib
matplotlib.use('TkAgg')

import matplotlib.pyplot as plt
from matplotlib import colors

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

class RoutePlanner():
    # Variables
    #   max_x: map x dimension
    #   max_y: map y dimension
    # Plot variables:
    #   fig: plot figure
    #   ax: plot artist
    #   cmap: color map
    #   norm: color normalization
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

        self.max_y, self.max_x = map.shape

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

    def get_current_pos(self):
        res = janus.query_once("located_at(robot, cell(X,Y))")
        if res["truth"]:
            return res["X"], res["Y"]

        return None

    def set_current_pos(self, x, y):
        janus.query_once("retractall(located_at(robot, _))", {})
        res = janus.query_once("assertz(located_at(robot, cell(X,Y)))", { "X": x, "Y": y })
        return res["truth"]

    def get_route(self, goal):
        return nil

    def get_route_for_load(self, goal, type):
        return nil

    def print_current_map(self):
        # 1. Load map
        map = self.get_current_map()

        # 2. Show base grid
        self.ax.clear()
        self.ax.imshow(map, cmap=self.cmap, norm=self.norm, origin='upper')

        # Adding visual mesh between cells
        self.ax.grid(which='major', axis='both', linestyle='-', color='k', linewidth=1, alpha=0.3)
        self.ax.set_xticks(np.arange(-.5, self.max_x, 1)); self.ax.set_yticks(np.arange(-.5, self.max_y, 1))
        self.ax.set_xticklabels([]); self.ax.set_yticklabels([]) #hide numbers

        # 3. Show robot
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

            # Opcional: Añadir texto o etiqueta al lado
            # self.ax.text(robot_pos[0], robot_pos[1], 'R', 
            #         ha='center', va='center', 
            #         color='black', fontweight='bold')

        # # 3. DIBUJAR LAS PUERTAS (ICONOS ENTRE CELDAS)
        # for (p1, p2) in puertas_logicas:
        #     r1, c1 = p1
        #     r2, c2 = p2
            
        #     # Calcular centro de la "puerta" (promedio de coordenadas)
        #     # Nota: en matplotlib imshow, X es columnas (c), Y es filas (r)
        #     x_prom = (c1 + c2) / 2
        #     y_prom = (r1 + r2) / 2
            
        #     # Determinar orientación para dibujar una línea gruesa (la puerta)
        #     if r1 == r2: # Puerta Vertical (entre columnas)
        #         # Dibujamos línea vertical
        #         self.ax.plot([x_prom, x_prom], [r1 - 0.4, r1 + 0.4], 
        #                 color='gold', linewidth=5, solid_capstyle='round')
        #     else: # Puerta Horizontal (entre filas)
        #         # Dibujamos línea horizontal
        #         self.ax.plot([c1 - 0.4, c1 + 0.4], [y_prom, y_prom], 
        #                 color='gold', linewidth=5, solid_capstyle='round')

        plt.title("World map")
        plt.draw()      # Fuerza el dibujado
        plt.pause(0.5)  # Pausa 0.5 segundos para que el ojo humano lo vea

if __name__ == "__main__":
    map = [
        [0, 1, 1, 4],
        [1, 1, 0, 1],
        [1, 0, 2, 2],
        [1, 0, 2, 2],
        [1, 0, 2, 2],
        [1, 0, 2, 2],
        [1, 0, 2, 2],
        [3, 1, 2, 0]
    ]

    planner = RoutePlanner(np.asarray(map, dtype=np.int32))
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

    planner.print_current_map()
    plt.ioff() # Desactivar modo interactivo
    plt.show() # Mantener la ventana abierta al final para ver el resultado
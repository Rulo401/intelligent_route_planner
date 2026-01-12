:- use_module(library(clpfd)).
% cell(X, Y) available cells
:- dynamic cell/2.
% door(D) existing doors
:- dynamic door/1.
% key(K) keys for interactive elements
:- dynamic key/1.
% barrier(B) existing barrier
:- dynamic barrier/1.
% switch(S) switch for barrier elements
:- dynamic switch/1.
% conveyor(C, length) conveyor belts
:- dynamic conveyor/2.

% floor_type(cell, type) -> type ∈ {smooth, uneven, mesh, carpet}
:- dynamic floor_type/2.
% located_at(element, cell) -> element ∈ {key, goal}
:- dynamic located_at/2.
% entry(conveyor, cell)
:- dynamic entry/2.
% exit(conveyor, cell)
:- dynamic exit/2.
% controls(switch, barrier)
:- dynamic controls/2.

% zone_type(cell, zone) -> zone ∈ {common, vehicles_only, robots_only}
:- dynamic zone_type/2.


adjacent(cell(X,Y1), cell(X,Y2)) :- cell(X,Y1), cell(X,Y2), Y1 #= Y2-1.
adjacent(cell(X,Y1), cell(X,Y2)) :- cell(X,Y1), cell(X,Y2), Y1 #= Y2+1.
adjacent(cell(X1,Y), cell(X2,Y)) :- cell(X1,Y), cell(X2,Y), X1 #= X2+1.
adjacent(cell(X1,Y), cell(X2,Y)) :- cell(X1,Y), cell(X2,Y), X1 #= X2-1.

connected(A, B) :- adjacent(A, B).
connected(A, B) :- conveyor(C, _), entry(C, A), exit(C, B).

% compatible_floor(LoadType, FloorType)
%   -> LoadType ∈ {standard, fragile, biochemical, dangerous}
%   -> FloorType ∈ {smooth, uneven, mesh, carpet}
compatible_floor(standard, smooth).
compatible_floor(standard, uneven).
compatible_floor(standard, mesh).
compatible_floor(standard, carpet).

compatible_floor(fragile, smooth).
compatible_floor(fragile, mesh).
compatible_floor(fragile, carpet).

compatible_floor(biochemical, smooth).
compatible_floor(biochemical, uneven).

compatible_floor(dangerous, smooth).
compatible_floor(dangerous, mesh).

% compatible_zone(LoadType, ZoneType)
%   -> LoadType ∈ {standard, fragile, biochemical, dangerous}
%   -> ZoneType ∈ {common, vehicles_only, robots_only}
compatible_zone(standard, common).
compatible_zone(standard, vehicles_only).
compatible_zone(standard, robots_only).

compatible_zone(fragile, common).
compatible_zone(fragile, vehicles_only).
compatible_zone(fragile, robots_only).

compatible_zone(biochemical, vehicles_only).
compatible_zone(biochemical, robots_only).

compatible_zone(dangerous, robots_only).

% passable(cell(X,Y), LoadType)
passable(cell(X,Y), LoadType) :-
    cell(X,Y),
    passable_floor(cell(X,Y), LoadType),
    passable_zone(cell(X,Y), LoadType).

passable_floor(Cell, LoadType) :-
    floor_type(Cell, FloorType),
    compatible_floor(LoadType, FloorType).

passable_zone(Cell, _) :- \+ zone_type(Cell, _).
passable_zone(Cell, LoadType) :-
    zone_type(Cell, ZoneType),
    compatible_zone(LoadType, ZoneType).

% update_keys(+Cell, +KeysIn, -KeysOut)
update_keys(Cell, KeysIn, KeysOut) :-
    findall(K, located_at(key(K), Cell), NewKeys),
    append(KeysIn, NewKeys, Temp),
    sort(Temp, KeysOut).   % quita duplicados

% can_enter(+Cell, +Keys)
% Se puede entrar si NO hay puerta,
%o si hay puerta D y llevas la llave D.
can_enter(Cell, _) :-
    \+ located_at(door(_), Cell).

can_enter(Cell, Keys) :-
    located_at(door(D), Cell),
    member(D, Keys).

% step(+From, +To, +LoadType, +KeysIn, -KeysOut)
% Desde From a To:
%  1) deben estar conectadas
%  2) To debe ser pasable para la carga
%  3) To debe poder “entrarse” según puertas/llaves y barreras/pulsadores
%  4) al entrar, se actualizan las llaves recogidas y el estado de los pulsadores
% step(+From, +To, +LoadType, +KeysIn, -KeysOut, +SwitchesIn, -SwitchesOut)
step(From, To, LoadType, KeysIn, KeysOut, SwitchesIn, SwitchesOut) :-
    connected(From, To),
    passable(To, LoadType),
    can_enter(To, KeysIn, SwitchesIn),
    update_keys(To, KeysIn, KeysOut),
    update_switches(To, SwitchesIn, SwitchesOut).


% toggle_switch(+SwitchId, +ActiveIn, -ActiveOut)
toggle_switch(S, ActiveIn, ActiveOut) :-
    (   select(switch(S), ActiveIn, Rest)   % si estaba activo, lo quita
    ->  ActiveOut = Rest
    ;   ActiveOut = [switch(S)|ActiveIn]    % si no estaba, lo añade
    ).

% update_switches(+Cell, +ActiveIn, -ActiveOut)
% Al ENTRAR en una celda con pulsador, cambia su estado (toggle).
update_switches(Cell, ActiveIn, ActiveOut) :-
    (   located_at(switch(S), Cell)
    ->  toggle_switch(S, ActiveIn, ActiveOut)
    ;   ActiveOut = ActiveIn
    ).

% barrier_open(+BarrierId, +ActiveSwitches)
% Una barrera está abierta si existe algún switch activo que la controle.
barrier_open(B, ActiveSwitches) :-
    member(switch(B), ActiveSwitches).

% can_enter(+Cell, +Keys, +ActiveSwitches)
% 1) primero aplica tu regla actual de puertas/llaves
% 2) si hay barrera en la celda, solo se entra si está abierta
can_enter(Cell, Keys, ActiveSwitches) :-
    can_enter(Cell, Keys),   % reutiliza tu can_enter/2 actual
    (   located_at(barrier(B), Cell)
    ->  barrier_open(B, ActiveSwitches)
    ;   true
    ).

% route(+Cell, +LoadType, -Path)
route(LoadType, PathPython) :- 
    located_at(robot, Cell), 
    located_at(goal, Goal),
    astar(Cell, Goal, LoadType, Path), 
    path_to_python(Path, PathPython).

% ---------------------------------------------------------
% A* Algorithm
% ---------------------------------------------------------

% manhattan(+CellA, +CellB, -Distance)
manhattan(cell(X1, Y1), cell(X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

% astar(+Start, +Goal, +LoadType, -Path)
astar(Start, Goal, LoadType, Path) :-
    manhattan(Start, Goal, D),
    
    %State: 
    %    path(F, G, state(Cell, Keys, Switches), CellHistory)
    %    F = G + D. Init G=0.
    InitState = state(Start, [], []),
    InitPath  = path(D, 0, InitState, [Start]),
    
    astar_search([InitPath], Goal, LoadType, SolutionState),
    
    SolutionState = path(_, _, _, ReversePath),
    reverse(ReversePath, Path).

% ---------------------------------------------------------
% Search engine A*
% ---------------------------------------------------------

astar_search([path(_, _, state(Goal, _, _), Path) | _], Goal, _, path(_, _, state(Goal, _, _), Path)).
astar_search([BestPath | RestQueue], Goal, LoadType, Solution) :-
    BestPath = path(_, G, CurrentState, History),

    findall(
        ChildPath,
        (
            % a) Generar transición válida usando tu predicado STEP
            transition(CurrentState, NextState, LoadType),
            
            % b) Evitar ciclos simples (no volver a un estado ya en ESTE camino)
            NextState = state(NextCell, _, _),
            \+ member(NextCell, History), 
            
            % c) Calcular nuevos valores F, G, H
            NewG is G + 1,
            NextState = state(NextCell, _, _),
            manhattan(NextCell, Goal, D),
            NewF is NewG + D,
            
            % d) Crear estructura del nuevo camino
            ChildPath = path(NewF, NewG, NextState, [NextCell | History])
        ),
        Children
    ),
    
    append(Children, RestQueue, UnsortedQueue),
    sort(UnsortedQueue, SortedQueue),
    
    astar_search(SortedQueue, Goal, LoadType, Solution).

transition(state(Current, KeysIn, SwitchesIn), state(Next, KeysOut, SwitchesOut), LoadType) :-
    step(Current, Next, LoadType, KeysIn, KeysOut, SwitchesIn, SwitchesOut).

% translate the path into a data structure manageable by Python
path_to_python([],[]).
path_to_python([cell(X,Y) | NextCells], [[X,Y] | NextCellsPython]) :- path_to_python(NextCells, NextCellsPython).
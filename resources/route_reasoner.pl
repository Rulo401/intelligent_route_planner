:- use_module(library(clpfd)).
% cell(X, Y) available cells
:- dynamic cell/3.
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

% floor_type(cell, type) -> type ∈ {smooth, uneven, slope_x, slope_y}
:- dynamic floor_type/2.
% located_at(element, cell) -> element ∈ {key, goal}
:- dynamic located_at/2.
% entry(conveyor, cell)
:- dynamic entry/2.
% exit(conveyor, cell)
:- dynamic exit/2.
% controls(switch, barrier)
:- dynamic controls/2.

% zone(cell(X,Y), Zone)
% Zone ∈ {common_zone, vehicle_zone, robot_zone}
:- dynamic zone/2.


adjacent(cell(X,Y1), cell(X,Y2)) :- cell(X,Y1), cell(X,Y2), Y1 #= Y2-1.
adjacent(cell(X,Y1), cell(X,Y2)) :- cell(X,Y1), cell(X,Y2), Y1 #= Y2+1.
adjacent(cell(X1,Y), cell(X2,Y)) :- cell(X1,Y), cell(X2,Y), X1 #= X2+1.
adjacent(cell(X1,Y), cell(X2,Y)) :- cell(X1,Y), cell(X2,Y), X1 #= X2-1.

connected(A, B) :- adjacent(A, B). %todo caso de las puertas y barreras
connected(A, B) :- conveyor(C, _), entry(C, A), exit(C, B).

% compatible(LoadType, FloorType)
%   -> LoadType ∈ {standard, fragile, biochemical, dangerous}
%   -> FloorType ∈ {smooth, uneven, mesh, carpet}
compatible(standard, smooth).
compatible(standard, uneven).
compatible(standard, mesh).
compatible(standard, carpet).

compatible(fragile, smooth).
compatible(fragile, mesh).
compatible(standard, carpet).

compatible(biochemical, smooth).
compatible(biochemical, uneven).

compatible(dangerous, smooth).
compatible(dangerous, mesh).

% passable(+Cell, +LoadType)
passable(Cell, LoadType) :-
    passable_floor(Cell, LoadType),
    zone_of(Cell, Zone),
    zone_allows(Zone, LoadType).

% door_in_cell(+Cell, -DoorId)
door_in_cell(Cell, D) :-
    cell(X,Y),
    Cell = cell(X,Y),
    located_at(door(D), Cell).

% update_keys(+Cell, +KeysIn, -KeysOut)
update_keys(Cell, KeysIn, KeysOut) :-
    findall(K, located_at(key(K), Cell), NewKeys),
    append(KeysIn, NewKeys, Temp),
    sort(Temp, KeysOut).   % quita duplicados

% can_enter(+Cell, +Keys)
% Se puede entrar si NO hay puerta,
%o si hay puerta D y llevas la llave D.
can_enter(Cell, _) :-
    \+ door_in_cell(Cell, _).

can_enter(Cell, Keys) :-
    door_in_cell(Cell, D),
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


% barrier_in_cell(+Cell, -BarrierId)
barrier_in_cell(Cell, B) :-
    located_at(barrier(B), Cell).

% switch_in_cell(+Cell, -SwitchId)
switch_in_cell(Cell, S) :-
    located_at(switch(S), Cell).

% toggle_switch(+SwitchId, +ActiveIn, -ActiveOut)
toggle_switch(S, ActiveIn, ActiveOut) :-
    (   select(S, ActiveIn, Rest)   % si estaba activo, lo quita
    ->  ActiveOut = Rest
    ;   ActiveOut = [S|ActiveIn]    % si no estaba, lo añade
    ).

% update_switches(+Cell, +ActiveIn, -ActiveOut)
% Al ENTRAR en una celda con pulsador, cambia su estado (toggle).
update_switches(Cell, ActiveIn, ActiveOut) :-
    (   switch_in_cell(Cell, S)
    ->  toggle_switch(S, ActiveIn, ActiveOut)
    ;   ActiveOut = ActiveIn
    ).

% barrier_open(+BarrierId, +ActiveSwitches)
% Una barrera está abierta si existe algún switch activo que la controle.
barrier_open(B, ActiveSwitches) :-
    controls(switch(S), barrier(B)),
    member(S, ActiveSwitches).

% can_enter(+Cell, +Keys, +ActiveSwitches)
% 1) primero aplica tu regla actual de puertas/llaves
% 2) si hay barrera en la celda, solo se entra si está abierta
can_enter(Cell, Keys, ActiveSwitches) :-
    can_enter(Cell, Keys),   % reutiliza tu can_enter/2 actual
    (   barrier_in_cell(Cell, B)
    ->  barrier_open(B, ActiveSwitches)
    ;   true
    ).

% zone_of(+Cell, -Zone)
zone_of(Cell, Zone) :-
    (   zone(Cell, Z)
    ->  Zone = Z
    ;   Zone = common_zone
    ).

% zone_allows(+Zone, +LoadType)

% Common Zone (pedestrians + robots)
zone_allows(common_zone, standard).
zone_allows(common_zone, fragile).

% Vehicle Zone (vehicles + robots)
zone_allows(vehicle_zone, standard).
zone_allows(vehicle_zone, fragile).
zone_allows(vehicle_zone, biochemical).

% Robot Zone (robots only)
zone_allows(robot_zone, standard).
zone_allows(robot_zone, fragile).
zone_allows(robot_zone, biochemical).
zone_allows(robot_zone, dangerous).

% passable_floor(+Cell, +LoadType)
passable_floor(cell(X,Y), LoadType) :-
    cell(X,Y),
    floor_type(cell(X,Y), FloorType),
    compatible(LoadType, FloorType).
% ---------------------------- BORRADORES ----------------------------------

% path(cell(X1,Y1), cell(X2,Y2), LoadType) :-
% route(cell(X,Y), LoadType) :- located_at(robot, cell(RX,RY)), path(cell(RX,RY), cell(X,Y), LoadType).

% % allowed_surface(TipoMercancia, Superficie).

% % Por suelo liso puedes ir con todo
% allowed_surface(_, suelo_liso).

% % Por cinta mecánica puedes ir con todo
% allowed_surface(_, cinta_mecanica).

% % Por rampa solo estandar, bioquimico y fragil
% allowed_surface(estandart, rampa).
% allowed_surface(bioquimico, rampa).
% allowed_surface(fragil,     rampa).

% % Por suelo irregular solo estandar y bioquimico
% allowed_surface(estandart,    suelo_irregular).
% allowed_surface(bioquimico,   suelo_irregular).

% % door_ok(InfoPuerta, LlavesQueLlevo).

% door_ok(none, _).
% door_ok(door(Id), Keys) :-
%     member(Id, Keys).

% % Recoger llaves al entrar en una celda

% update_keys(X, Y, KeysIn, KeysOut) :-
%     findall(K, key(K, X, Y), NewKeys),
%     append(KeysIn, NewKeys, All),
%     sort(All, KeysOut).   % elimina duplicados y ordena

% % neighbor(+X, +Y, -NX, -NY) : (NX,NY) es vecina de (X,Y)

% neighbor(X, Y, NX, Y) :-
%     NX is X + 1,
%     cell(NX, Y, _, _).

% neighbor(X, Y, NX, Y) :-
%     NX is X - 1,
%     cell(NX, Y, _, _).

% neighbor(X, Y, X, NY) :-
%     NY is Y + 1,
%     cell(X, NY, _, _).

% neighbor(X, Y, X, NY) :-
%     NY is Y - 1,
%     cell(X, NY, _, _).

% % cost_surface(Superficie, CostePaso).
% cost_surface(suelo_liso,      1).
% cost_surface(cinta_mecanica,  0.5).  % ejemplo: más rápida
% cost_surface(rampa,           2).
% cost_surface(suelo_irregular, 3).


% % step(+Mercancia, +Keys, +X, +Y, -NX, -NY, -Coste)

% step(Mercancia, Keys, X, Y, NX, NY, Cost) :-
%     neighbor(X, Y, NX, NY),
%     cell(NX, NY, Surface, DoorInfo),
%     allowed_surface(Mercancia, Surface),
%     door_ok(DoorInfo, Keys),
%     cost_surface(Surface, Cost).


% % path(+Mercancia, +X, +Y, +GX, +GY, +KeysIn, +Visitados, -Camino, -CosteTotal).

% % Caso base: ya estamos en el destino
% path(_, X, Y, X, Y, Keys, _, [(X,Y)], 0) :-
%     % recogemos posibles llaves del destino
%     update_keys(X, Y, Keys, _).

% % Caso recursivo
% path(Mercancia, X, Y, GX, GY, KeysIn, Visited, [(X,Y) | RestPath], TotalCost) :-

%     % al entrar en (X,Y), recogemos llaves
%     update_keys(X, Y, KeysIn, Keys1),

%     % movimiento a una celda vecina
%     step(Mercancia, Keys1, X, Y, NX, NY, StepCost),

%     \+ member((NX,NY), Visited),            % evitar ciclos simples

%     path(Mercancia, NX, NY, GX, GY, Keys1,
%          [(NX,NY) | Visited], RestPath, RestCost),

%     TotalCost is StepCost + RestCost.

% % shortest_route(+Mercancia, +SX, +SY, +GX, +GY, -Camino, -Coste)

% shortest_route(Mercancia, SX, SY, GX, GY, BestPath, BestCost) :-
%     setof(Cost-Path,
%           path(Mercancia, SX, SY, GX, GY,
%                [],         % sin llaves al inicio
%                [(SX,SY)],  % visitados
%                Path, Cost),[BestCost-BestPath | _]).

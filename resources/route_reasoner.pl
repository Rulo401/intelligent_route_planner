% cell(X, Y, Superficie, InfoPuerta)
% Superficie ∈ {suelo_liso, rampa, suelo_irregular, cinta_mecanica}
% InfoPuerta ∈ {none, door(IdPuerta)}

cell(1,1, suelo_liso,      none).
cell(1,2, rampa,           none).
cell(1,3, cinta_mecanica,  none).
cell(2,1, suelo_irregular, none).
cell(2,2, suelo_liso,      door(p1)).
...

% key(IdLlave, X, Y)
key(p1, 1,3).     % la llave p1 está en la celda (1,3)


% allowed_surface(TipoMercancia, Superficie).

% Por suelo liso puedes ir con todo
allowed_surface(_, suelo_liso).

% Por cinta mecánica puedes ir con todo
allowed_surface(_, cinta_mecanica).

% Por rampa solo estandar, bioquimico y fragil
allowed_surface(estandart, rampa).
allowed_surface(bioquimico, rampa).
allowed_surface(fragil,     rampa).

% Por suelo irregular solo estandar y bioquimico
allowed_surface(estandart,    suelo_irregular).
allowed_surface(bioquimico,   suelo_irregular).

% door_ok(InfoPuerta, LlavesQueLlevo).

door_ok(none, _).
door_ok(door(Id), Keys) :-
    member(Id, Keys).

% Recoger llaves al entrar en una celda

update_keys(X, Y, KeysIn, KeysOut) :-
    findall(K, key(K, X, Y), NewKeys),
    append(KeysIn, NewKeys, All),
    sort(All, KeysOut).   % elimina duplicados y ordena

% neighbor(+X, +Y, -NX, -NY) : (NX,NY) es vecina de (X,Y)

neighbor(X, Y, NX, Y) :-
    NX is X + 1,
    cell(NX, Y, _, _).

neighbor(X, Y, NX, Y) :-
    NX is X - 1,
    cell(NX, Y, _, _).

neighbor(X, Y, X, NY) :-
    NY is Y + 1,
    cell(X, NY, _, _).

neighbor(X, Y, X, NY) :-
    NY is Y - 1,
    cell(X, NY, _, _).

% cost_surface(Superficie, CostePaso).
cost_surface(suelo_liso,      1).
cost_surface(cinta_mecanica,  0.5).  % ejemplo: más rápida
cost_surface(rampa,           2).
cost_surface(suelo_irregular, 3).


% step(+Mercancia, +Keys, +X, +Y, -NX, -NY, -Coste)

step(Mercancia, Keys, X, Y, NX, NY, Cost) :-
    neighbor(X, Y, NX, NY),
    cell(NX, NY, Surface, DoorInfo),
    allowed_surface(Mercancia, Surface),
    door_ok(DoorInfo, Keys),
    cost_surface(Surface, Cost).


% path(+Mercancia, +X, +Y, +GX, +GY, +KeysIn, +Visitados, -Camino, -CosteTotal).

% Caso base: ya estamos en el destino
path(_, X, Y, X, Y, Keys, _, [(X,Y)], 0) :-
    % recogemos posibles llaves del destino
    update_keys(X, Y, Keys, _).

% Caso recursivo
path(Mercancia, X, Y, GX, GY, KeysIn, Visited, [(X,Y) | RestPath], TotalCost) :-

    % al entrar en (X,Y), recogemos llaves
    update_keys(X, Y, KeysIn, Keys1),

    % movimiento a una celda vecina
    step(Mercancia, Keys1, X, Y, NX, NY, StepCost),

    \+ member((NX,NY), Visited),            % evitar ciclos simples

    path(Mercancia, NX, NY, GX, GY, Keys1,
         [(NX,NY) | Visited], RestPath, RestCost),

    TotalCost is StepCost + RestCost.

% shortest_route(+Mercancia, +SX, +SY, +GX, +GY, -Camino, -Coste)

shortest_route(Mercancia, SX, SY, GX, GY, BestPath, BestCost) :-
    setof(Cost-Path,
          path(Mercancia, SX, SY, GX, GY,
               [],         % sin llaves al inicio
               [(SX,SY)],  % visitados
               Path, Cost),[BestCost-BestPath | _]).

% atiende(Nombre, Dia, HoraQueEmpieza, HoraQueTermina).

atiende(dodain, lunes, 9, 15).
atiende(dodain, miercoles, 9, 15).
atiende(dodain, viernes, 9, 15).

atiende(lucas, martes, 10, 20).

atiende(juanC, sabado, 18, 22).
atiende(juanC, domingo, 18, 22).

atiende(juanFdS, jueves, 10, 20).
atiende(juanFdS, viernes, 12, 20).

atiende(leoC, lunes, 14, 18).
atiende(leoC, miercoles, 14, 18).

atiende(martu, miercoles, 23, 24).

% PUNTO 1

atiende(vale, Dia, HoraQueEmpieza, HoraQueTermina):-
    atiende(dodain, Dia, HoraQueEmpieza, HoraQueTermina).

atiende(vale, Dia, HoraQueEmpieza, HoraQueTermina):-
    atiende(juanC, Dia, HoraQueEmpieza, HoraQueTermina).

% si nadie hace el horario de leoC, por principio de universo cerrado, no agregamos a la base de conocimiento aquello que no tiene sentido.

% lo mismo que en el punto anterior, lo desconocido se presume como falso, por principio de universo cerrado, entonces no lo agregamos.

% PUNTO 2

quienAtiende(Persona, Dia, HorarioPuntual):-
    atiende(Persona, Dia, HoraQueEmpieza, HoraQueTermina),
    between(HoraQueEmpieza, HoraQueTermina, HorarioPuntual).

% PUNTO 3

foreverAlone(Persona, Dia, HorarioPuntual):-
    quienAtiende(Persona, Dia, HorarioPuntual),
    not((quienAtiende(OtraPersona, Dia, HorarioPuntual), Persona \= OtraPersona)).

% PUNTO 4


posibilidadesAtencion(Dia, PersonasPosibles):-
    findall(Persona, quienAtiende(Persona, Dia, _), PersonasPosibles).

/*
posibilidadesAtencion(Dia, Personas):-
    findall(Persona, distinct(Persona, quienAtiende(Persona, Dia, _)), PersonasPosibles),
    combinar(PersonasPosibles, Personas).
  
combinar([], []).
combinar([Persona|PersonasPosibles], [Persona|Personas]):-combinar(PersonasPosibles, Personas).
combinar([_|PersonasPosibles], Personas):-combinar(PersonasPosibles, Personas).
*/
% Qué conceptos en conjunto resuelven este requerimiento?
% - findall como herramienta para poder generar un conjunto de soluciones que satisfacen un predicado.
% - mecanismo de backtracking de Prolog permite encontrar todas las soluciones posibles.

% PUNTO 5

venta(dodain, fecha(10, 8), [golosinas(1200), cigarrillos(jockey), golosinas(50)]).
venta(dodain, fecha(12, 8), [bebidas(true, 8), bebidas(false, 1), golosinas(10)]).
venta(martu, fecha(12, 8), [golosinas(1000), cigarrillos([chesterfield, colorado, parisiennes])]).
venta(lucas, fecha(11, 8), [golosinas(600)]).
venta(lucas, fecha(18, 8), [bebidas(false, 2), cigarrillos(derby)]).

personaSuertuda(Persona):-
    venta(Persona, _, _),
    forall(venta(Persona, _, [Venta|_]), ventaImportante(Venta)).

ventaImportante(golosinas(Precio)):-
    Precio > 100.
ventaImportante(cigarrillos(Marcas)):-
    length(Marcas, Cantidad),
    Cantidad > 2.
ventaImportante(bebidas(true, _)).
ventaImportante(bebidas(_, Cantidad)):-
    Cantidad > 5.

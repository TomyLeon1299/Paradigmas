%herramientasRequeridas(Tarea, Herramientas).
herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

%tareaPedida(Cliente, Tarea, MetrosCuadrados).
tareaPedida(dana, ordenarCuarto, 20).
tareaPedida(walter, cortarPasto, 50).
tareaPedida(walter, limpiarTecho, 70).
tareaPedida(louis, limpiarBanio, 15).

%precio(Tarea, PrecioPorMetroCuadrado).
precio(ordenarCuarto, 13).
precio(limpiarTecho, 20).
precio(limpiarBanio, 55).
precio(cortarPasto, 10).
precio(encerarPisos, 7).

% PUNTO 1

%tiene(Persona, Herramienta).
tiene(egon, aspiradora(200)).
tiene(egon, trapeador).
tiene(peter, trapeador).
tiene(winston, varitaDeNeutrones).
% la bordeadora no la ponemos en la base de conocimientos ya que nadie la tiene.

% PUNTO 2

satisfaceNecesidad(Persona, Herramienta):-
    tiene(Persona, Herramienta).

satisfaceNecesidad(Persona, aspiradora(PotenciaRequerida)):-
    tiene(Persona, aspiradora(Potencia)),
    between(0, Potencia, PotenciaRequerida).

satisfaceNecesidad(Persona, ListaHerramientas):-
    member(Herramienta, ListaHerramientas),
    satisfaceNecesidad(Persona, Herramienta).
    
% PUNTO 3

puedeHacerTarea(Persona, Tarea):-
    herramientasRequeridas(Tarea, _),
    tiene(Persona, varitaDeNeutrones).

puedeHacerTarea(Persona, Tarea):-
    tiene(Persona, _),
    requiereHerramienta(Tarea, _),
    forall(requiereHerramienta(Tarea, Herramienta), satisfaceNecesidad(Persona, Herramienta)).

requiereHerramienta(Tarea, Herramienta):-
    herramientasRequeridas(Tarea, ListaHerramientas),
    member(Herramienta, ListaHerramientas).

% PUNTO 4

precioACobrar(Cliente, PrecioTotal):-
    tareaPedida(Cliente, _, _),
    findall(Precio, precioPorTareaPedida(Cliente, _, Precio), ListaPrecios),
    sumlist(ListaPrecios, PrecioTotal).

precioPorTareaPedida(Cliente, Tarea, Precio):-
    tareaPedida(Cliente, Tarea, Metros),
    precio(Tarea, PrecioPorMetro),
    Precio is PrecioPorMetro * Metros.

% PUNTO 5

tareaCompleja(Tarea):-
    herramientasRequeridas(Tarea, Herramienta),
    herramientasRequeridas(Tarea, OtraHerramienta),
    Herramienta \= OtraHerramienta.

aceptarPedido(Trabajador, Cliente):-
    puedeHacerPedido(Trabajador, Cliente),
    estaDispuesto(Trabajador, Cliente).

puedeHacerPedido(Trabajador, Cliente):-
    tareaPedida(Cliente, _, _),
    tiene(Trabajador, _),
    forall(tareaPedida(Cliente, Tarea, _), puedeHacerTarea(Trabajador, Tarea)).

estaDispuesto(ray, Cliente):-
    not(tareaPedida(Cliente, limpiarTecho, _)).

estaDispuesto(winston, Cliente):-
    precioACobrar(Cliente, PrecioTotal),
    PrecioTotal >= 500.

estaDispuesto(egon, Cliente):-
    not((tareaPedida(Cliente, Tarea, _), tareaCompleja(Tarea))).

estaDispuesto(peter, _).

% PUNTO 6


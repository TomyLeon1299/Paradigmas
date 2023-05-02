% …jugador(Nombre, Rating, Civilizacion).
jugador(juli, 2200, jemeres).
jugador(aleP, 1600, mongoles).
jugador(feli, 500000, persas).
jugador(aleC, 1723, otomanos).
jugador(ger, 1729, ramanujanos).
jugador(juan, 1515, britones).
jugador(marti, 1342, argentinos).

% …tiene(Nombre, QueTiene).
tiene(aleP, unidad(samurai, 199)).
tiene(aleP, unidad(espadachin, 10)).
tiene(aleP, unidad(granjero, 10)).
tiene(aleP, recurso(800, 600, 100)).
tiene(aleP, edificio(casa, 40)).
tiene(aleP, edificio(castillo, 1)).
tiene(juan, unidad(carreta, 10)).
tiene(tomi, recurso(0, 900, 300)).
tiene(tomi, edificio(herreria, 70)).

% militar(Tipo, costo(Madera, Alimento, Oro), Categoria).
militar(espadachin, costo(0, 60, 20), infanteria).
militar(arquero, costo(25, 0, 45), arqueria).
militar(mangudai, costo(55, 0, 65), caballeria).
militar(samurai, costo(0, 60, 30), unica).
militar(keshik, costo(0, 80, 50), unica).
militar(tarcanos, costo(0, 60, 60), unica).
militar(alabardero, costo(25, 35, 0), piquero).

% aldeano(Tipo, produce(Madera, Alimento, Oro)).
aldeano(lenador, produce(23, 0, 0)).
aldeano(granjero, produce(0, 32, 0)).
aldeano(minero, produce(0, 0, 23)).
aldeano(cazador, produce(0, 25, 0)).
aldeano(pescador, produce(0, 23, 0)).
aldeano(alquimista, produce(0, 0, 25)).

% edificio(Edificio, costo(Madera, Alimento, Oro)).
edificio(casa, costo(30, 0, 0)).
edificio(granja, costo(0, 60, 0)).
edificio(herreria, costo(175, 0, 0)).
edificio(castillo, costo(650, 0, 300)).
edificio(maravillaMartinez, costo(10000, 10000, 10000)).

% PUNTO 1

esUnAfano(Jugador, OtroJugador):-
    jugador(Jugador, Rating, _),
    jugador(OtroJugador, OtroRating, _),
    abs(Rating - OtroRating) > 500.

% PUNTO 2

esEfectivo(Tipo, OtroTipo):-
    militar(Tipo, _, Categoria),
    militar(OtroTipo, _, OtraCategoria),
    puedeGanar(Categoria, OtraCategoria).

esEfectivo(samurai, Tipo):-
    militar(Tipo, _, unica).

puedeGanar(caballeria, arqueria).
puedeGanar(arqueria, infanteria).
puedeGanar(infanteria, piquero).
puedeGanar(piquero, caballeria).

% PUNTO 3

alarico(Jugador):-
    tiene(Jugador, _),
    soloTieneUnidadesDe(infanteria, Jugador).

soloTieneUnidadesDe(Categoria, Jugador):-
    forall(tiene(Jugador, unidad(Tipo, _)), militar(Tipo, _, Categoria)).

% PUNTO 4

leonidas(Jugador):-
    tiene(Jugador, _),
    soloTieneUnidadesDe(piquero, Jugador).

% PUNTO 5

nomada(Jugador):-
    tiene(Jugador, _),
    not(tiene(Jugador, edificio(casa, _))).

% PUNTO 6

cuantoCuesta(Tipo, Costo):-
    edificio(Tipo, Costo).

cuantoCuesta(Tipo, Costo):-
    militar(Tipo, Costo, _).

cuantoCuesta(Tipo, costo(0, 50, 0)):-
    aldeano(Tipo, _).

cuantoCuesta(Tipo, costo(100, 0, 50)):-
    esCarretaOUrna(Tipo).

esCarretaOUrna(carreta).
esCarretaOUrna(urnaMercante).

% PUNTO 7

produccion(Tipo, ProduccionPorMinuto):-
    aldeano(Tipo, ProduccionPorMinuto).

produccion(Tipo, produce(0, 0, 32)):-
    esCarretaOUrna(Tipo).

produccion(keshik, produce(0, 0, 10)).

produccion(Tipo, produce(0, 0, 0)):-
    militar(Tipo, _, _),
    Tipo \= keshik.

% PUNTO 8

produccionTotal(Jugador, Recurso, ProduccionTotalPorMinuto):-
    tiene(Jugador, _),
    recursos(Recurso),
    findall(Produccion, loTieneYProduce(Jugador, Recurso, Produccion), ListaProduccion),
    sumlist(ListaProduccion, ProduccionTotalPorMinuto).
    
recursos(madera).
recursos(alimento).
recursos(oro).

loTieneYProduce(Jugador, Recurso, Produccion):-
    tiene(Jugador, unidad(Tipo, CuantasTiene)),
    produccion(Tipo, ProduccionTotal),
    produccionDelRecurso(Recurso, ProduccionTotal, ProduccionRecurso),
    Produccion is ProduccionRecurso * CuantasTiene.

produccionDelRecurso(madera, produccion(Madera, __, _), Madera).
produccionDelRecurso(alimento, produccion(_, Alimento, _), Alimento).
produccionDelRecurso(oro, produccion(_, _, Oro), Oro).

% PUNTO 9

estaPeleado(Jugador, OtroJugador):-
    not(esUnAfano(Jugador, OtroJugador)),
    not(esUnAfano(OtroJugador, Jugador)),
    cantidadDeUnidades(Jugador, Cantidad),
    cantidadDeUnidades(OtroJugador, Cantidad),
    verProduccionSegun(Recurso, Jugador, OtroJugador).

cantidadDeUnidades(Jugador, Cantidad):-
    findall(Unidad, tiene(Jugador, unidad(Unidad)), Unidades),
    length(Unidades, Cantidad).

% ??????????

% PUNTO 10

avanzaA(_, edadMedia).

avanzaA(Jugador, edadFeudal):-
    jugador(Jugador, _, _),
    cumpleAlimento(Jugador, 500),
    tiene(Jugador, edificio(casa, _)).

avanzaA(Jugador, edadDeLosCastillos):-
    jugador(Jugador, _, _),
    cumpleAlimento(Jugador, 800),
    cumpleOro(Jugador, 200),
    edificioEdadDeLosCastillos(Edificio),
    tiene(Jugador, edificio(Edificio, _)).

edificioEdadDeLosCastillos(herreria).
edificioEdadDeLosCastillos(establo).
edificioEdadDeLosCastillos(galeriaDeTiro).

cumpleAlimento(Jugador, Cantidad):-
    tiene(Jugador, recurso(_, Alimento, _)),
    Alimento > Cantidad.

cumpleOro(Jugador, Cantidad):-
    tiene(Jugador, recurso(_, _, Oro)),
    Oro > Cantidad.

avanzaA(Jugador, edadImperial):-
    jugador(Jugador, _, _),
    cumpleAlimento(Jugador, 1000),
    cumpleOro(Jugador, 800),
    edificioImperial(Edificio),
    tiene(Jugador, edificio(Edificio, _)),
    edificioImperial(OtroEdificio),
    tiene(Jugador, edificio(OtroEdificio, _)).

edificioImperial(castillo).
edificioImperial(universidad).
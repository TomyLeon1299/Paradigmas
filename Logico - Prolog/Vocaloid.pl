% CANTANTES

% vocaloid(nombre, cancion).
vocaloid(megurineLuka, cancion(nightFever, 4)).
vocaloid(megurineLuka, cancion(foreverYoung, 5)).
vocaloid(hatsuneMiku, cancion(tellYourWorld, 4)).
vocaloid(gumi, cancion(foreverYoung, 4)).
vocaloid(gumi, cancion(tellYourWorld, 5)).
vocaloid(seeU, cancion(novemberRain, 6)).
vocaloid(seeU, cancion(nightFever, 5)).
%kaito no sabe cantar ninguna cancion, entonces no se pone en la base de conocimientos.

% PUNTO 1

novedoso(Vocaloid):-
    sabeAlMenosDosCanciones(Vocaloid),
    tiempoTotalCanciones(Vocaloid, Tiempo),
    Tiempo > 15.

sabeAlMenosDosCanciones(Vocaloid):-
    vocaloid(Vocaloid, Cancion),
    vocaloid(Vocaloid, OtraCancion),
    Cancion \= OtraCancion.

tiempoTotalCanciones(Vocaloid, TiempoTotal):-
    vocaloid(Vocaloid, _),
    findall(Tiempo, vocaloid(Vocaloid, cancion(_, Tiempo)), Tiempos),
    sumlist(Tiempos, TiempoTotal).

% PUNTO 2

acelerado(Vocaloid):-
    vocaloid(Vocaloid, _),
    not((vocaloid(Vocaloid, cancion(_, Tiempo)), Tiempo > 4)).

% CONCIERTO

% concierto(Nombre, Pais, Fama, Tipo).
% gigante(CantidadCancionesMinimas, DuracionTotalMinima).
% mediano(DuracionTotalMaxima).
% pequenio(DuracionMinimaUnaCancion).

concierto(mikuExpo, eeuu, 2000, gigante(2, 6)).
concierto(magicalMirai, japon, 3000, gigante(3, 10)).
concierto(vocalektVisions, eeuu, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, pequenio(4)).

% PUNTO 2

puedeParticipar(hatsuneMiku, Concierto):-
    concierto(Concierto, _, _, _).

puedeParticipar(Vocaloid, Concierto):-
    vocaloid(Vocaloid, _),
    Vocaloid \= hatsuneMiku,
    concierto(Concierto, _, _, Requisitos),
    cumpleRequisitos(Vocaloid, Requisitos).

cumpleRequisitos(Vocaloid, gigante(CantidadCancionesMinimas, DuracionTotalMinima)):-
    cantidadCancion(Vocaloid, Cantidad),
    Cantidad >= CantidadCancionesMinimas,
    tiempoTotalCanciones(Vocaloid, TiempoTotal),
    TiempoTotal > DuracionTotalMinima.

cumpleRequisitos(Vocaloid, mediano(DuracionTotalMaxima)):-
    tiempoTotalCanciones(Vocaloid, TiempoTotal),
    TiempoTotal < DuracionTotalMaxima.

cumpleRequisitos(Vocaloid, pequenio(DuracionMinimaUnaCancion)):-
    vocaloid(Vocaloid, cancion(_, Tiempo)),
    Tiempo > DuracionMinimaUnaCancion.

cantidadCancion(Vocaloid, Cantidad):-
    vocaloid(Vocaloid, _),
    findall(Cancion, vocaloid(Vocaloid, Cancion), Canciones),
    length(Canciones, Cantidad).

% PUNTO 3

masFamoso(Vocaloid):-
    nivelFama(Vocaloid, NivelMasFamoso),
    forall(nivelFama(Vocaloid, Nivel), NivelMasFamoso >= Nivel).

nivelFama(Vocaloid, Nivel):-
    famaTotal(Vocaloid, FamaTotal),
    cantidadCancion(Vocaloid, Cantidad),
    Nivel is FamaTotal * Cantidad.

famaTotal(Vocaloid, FamaTotal):-
    vocaloid(Vocaloid, _),
    findall(Fama, (puedeParticipar(Vocaloid, Concierto), concierto(Concierto, _, Fama, _)), CantidadesFama),
    sumlist(CantidadesFama, FamaTotal).

% PUNTO 4

conoce(magurineLuka, hatsuneMiku).
conoce(magurineLuka, gumi).
conoce(gumi, seeU).
conoce(seeU, kaito).

unicoParticipanteEntreConocidos(Vocaloid, Concierto):-
    puedeParticipar(Vocaloid, Concierto),
    not((conocido(Vocaloid, OtroVocaloid), puedeParticipar(OtroVocaloid, Concierto))).

conocido(Vocaloid, OtroVocaloid):-
    conoce(Vocaloid, OtroVocaloid).
conocido(Vocaloid, OtroVocaloid):-
    conoce(Vocaloid, OtroVocaloid),
    conocido(OtroVocaloid, OtroOtroVocaloid).

% PUNTO 5

% En la solución planteada habría que agregar una claúsula en el predicado cumpleRequisitos/2 que tenga en cuenta el nuevo functor con sus respectivos requisitos.
% El concepto que facilita los cambios para el nuevo requerimiento es el polimorfismo, que nos permite dar un tratamiento en particular a cada uno de los conciertos en la cabeza de la cláusula.
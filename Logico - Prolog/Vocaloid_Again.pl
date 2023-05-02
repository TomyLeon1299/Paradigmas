% CANTANTES

%vocaloid(Nombre, cancion(NombreCancion, Duracion)).
vocaloid(megurineLuka, cancion(nightFever, 4)).
vocaloid(megurineLuka, cancion(foreverYoung, 5)).
vocaloid(hatsuneMiku, cancion(tellYourWorld, 4)).
vocaloid(gumi, cancion(foreverYoung, 5)).
vocaloid(seeU, cancion(novemberRain, 6)).
vocaloid(seeU, cancion(nightFever, 5)).
% kaito no sabe cantar ninguna cancion, entonces no lo agregamos a la base de conocimientos.

% PUNTO 1

esNovedoso(Vocaloid):-
    sabeAlMenosDosCanciones(Vocaloid),
    tiempoTotalCanciones(Vocaloid, Tiempo),
    Tiempo < 15.

sabeAlMenosDosCanciones(Vocaloid):-
    vocaloid(Vocaloid, Cancion),
    vocaloid(Vocaloid, OtraCancion),
    Cancion \= OtraCancion.

tiempoTotalCanciones(Vocaloid, TiempoTotal):-
    vocaloid(Vocaloid, _),
    findall(Tiempo, vocaloid(Vocaloid, cancion(_, Tiempo)), Tiempos),
    sumlist(Tiempos, TiempoTotal).

% PUNTO 2

esAcelerado(Vocaloid):-
    vocaloid(Vocaloid, _),
    not((vocaloid(Vocaloid, cancion(_, Tiempo)), Tiempo > 4)).

% CONCIERTOS

%concierto(Nombre, Pais, Fama, Tipo).
%gigante(CantidadCancionesMinimas, DuracionTotalMinima).
%mediano(DuracionTotalMaxima).
%pequenio(DuracionMinimaUnaCancion).

% PUNTO 1

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
    tiempoTotalCanciones(Vocaloid, TiempoTotal),
    TiempoTotal > DuracionTotalMinima,
    cantidadCancion(Vocaloid, Cantidad),
    Cantidad > CantidadCancionesMinimas.

cumpleRequisitos(Vocaloid, mediano(DuracionTotalMaxima)):-
    tiempoTotalCanciones(Vocaloid, TiempoTotal),
    TiempoTotal < DuracionTotalMaxima.

cumpleRequisitos(Vocaloid, pequenio(DuracionMinimaUnaCancion)):-
    vocaloid(Vocaloid, cancion(_, Tiempo)),
    Tiempo > DuracionMinimaUnaCancion.

cantidadCancion(Vocaloid, Cantidad):-
    vocaloid(Vocaloid, _),
    findall(Cancion, vocaloid(Vocaloid, cancion(Cancion, _)), Canciones),
    length(Canciones, Cantidad).

% PUNTO 3

masFamoso(Vocaloid):-
    nivelFama(Vocaloid, NivelMasFamoso),
    forall(nivelFama(vocaloid, Nivel), NivelMasFamoso >= Nivel).

nivelFama(Vocaloid, Nivel):-
    vocaloid(Vocaloid, _),
    findall(Fama, (puedeParticipar(Vocaloid, Concierto), concierto(Concierto, _, Fama, _)), ListaFama),
    sumlist(ListaFama, FamaTotal),
    cantidadCancion(Vocaloid, Cantidad),
    Nivel is FamaTotal * Cantidad.

% PUNTO 4

conoce(megurineLuka, hatsuneMiku).
conoce(megurineLuka, gumi).
conoce(gumi, seeU).
conoce(seeU, kaito).

unicoParticipanteEntreConocidos(Vocaloid):-
    vocaloid(Vocaloid, _),
    not((conocido(Vocaloid, OtroVocaloid), puedeParticipar(OtroVocaloid, Concierto))).

conocido(Vocaloid, OtroVocaloid):-
    conoce(Vocaloid, OtroVocaloid).
conocido(Vocaloid, OtroVocaloid):-
    conoce(Vocaloid, OtroVocaloid),
    conocido(OtroVocaloid, OtroOtroVocaloid).

% PUNTO 5

% Primero, habria que ver si existe algun concierto de ese tipo y agragarlo a la base de conocimiento. Luego, en puedeParticipar hay que agregar el cumpleRequisitos del nuevo tipo de concierto.

% Polimorfismo, que nos permite dar un tratamiento en particular a cada uno de los conciertos en la cabeza de la cláusula.

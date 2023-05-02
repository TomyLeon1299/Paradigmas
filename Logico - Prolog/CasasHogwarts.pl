%%%% PARTE 1 - Sombrero Seleccionador

casa(gryffindor).
casa(slytherin).
casa(hufflepuff).
casa(ravenclaw).

sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, pura).

caracteristica(harry, coraje).
caracteristica(harry, amistad).
caracteristica(harry, orgullo).
caracteristica(harry, inteligencia).
caracteristica(draco, inteligencia).
caracteristica(draco, orgullo).
caracteristica(hermione, inteligencia).
caracteristica(hermione, orgullo).
caracteristica(hermione, responsabilidad).

odiariaEntrar(harry, slytherin).
odiariaEntrar(draco, hufflepuff).

caracteristicaBuscada(gryffindor, coraje).
caracteristicaBuscada(slytherin, orgullo).
caracteristicaBuscada(slytherin, inteligencia).
caracteristicaBuscada(ravenclaw, inteligencia).
caracteristicaBuscada(ravenclaw, responsabilidad).
caracteristicaBuscada(hufflepuff, amistad).

permiteEntrar(slytherin, Mago):-
    sangre(Mago, TipoSangre),
    TipoSangre \= impura.

permiteEntrar(Casa, Mago):-
    casa(Casa),
    sangre(Mago, _),
    Casa \= slytherin.

tieneCaracterApropiado(Casa, Mago):-
    casa(Casa),
    sangre(Mago, _),
    forall(caracteristicaBuscada(Casa, Caracteristica), caracteristica(Mago, Caracteristica)).

puedeQuedarSeleccionadoPara(gryffindor, hermione).
puedeQuedarSeleccionadoPara(Casa, Mago):-
    tieneCaracterApropiado(Casa, Mago),
    permiteEntrar(Casa, Mago),
    not(odiariaEntrar(Mago, Casa)).

cadenaDeAmistades(Magos):-
    sonAmistosos(Magos),
    cadenaCasas(Magos).

sonAmistosos(Magos):-
    forall(member(Mago, Magos), caracteristica(Mago, amistad)).

cadenaCasas(MAgos):-
    forall(consecutivos(Mago1, Mago2, Magos), puedeQuedarEnLaMismaCasa(Mago1, Mago2, _)).

consecutivos(Anterior, Siguiente, Lista):-
    nth1(IndiceAnterior, Lista, Anterior),
    IndiceSiguiente is IndiceAnterior + 1,
    nth1(IndiceSiguiente, Lista, Siguiente).

puedeQuedarEnLaMismaCasa(Mago1, Mago2, Casa):-
    puedeQuedarSeleccionadoPara(Mago1, Casa),
    puedeQuedarSeleccionadoPara(Mago2, Casa),
    Mago1 \= Mago2.

%%%% PARTE 2 - La copa de las casas

hizo(harry, fueraDeCama).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, irA(seccionRestringida)).
hizo(harry, irA(bosque)).
hizo(harry, irA(tercerPiso)).
hizo(draco, irA(mazmorras)).
hizo(ron, buenaAccion(50, ganarAjedrezMagico)).
hizo(hermione, buenaAccion(50, salvarAmigos)).
hizo(harry, buenaAccion(60, ganarleAVoldemort)).

lugarProhibido(bosque, 50).
lugarProhibido(seccionRestringida, 10).
lugarProhibido(tercerPiso, 75).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

buenAlumno(Mago):-
    hizo(Mago, _),
    not(hizoAlgoMalo(Mago)).

hizoAlgoMalo(Mago):-
    hizo(Mago, Accion),
    puntajeQueGenera(Accion, Puntaje),
    Puntaje < 0.

puntajeQueGenera(fueraDeCama, -50).
puntajeQueGenera(irA(Lugar), Puntaje):-
    lugarProhibido(Lugar, Puntos),
    PuntajeQueResta is Puntos * -1.
puntajeQueGenera(buenaAccion(Puntaje, _), Puntaje).

accionRecurrente(Accion):-
    hizo(Mago, Accion),
    hizo(OtroMago, Accion),
    Mago \= OtroMago.

puntajeTotalCasa(Casa, PuntajeTotal):-
    esDe(_, Casa),
    findall(Puntos, (esDe(Mago, Casa), hizo(Mago, Accion), puntajeQueGenera(Accion, Puntos)), ListaPuntos),
    sumlist(ListaPuntos, PuntajeTotal).

casaGanadora(Casa):-
    puntajeTotalCasa(Casa, PuntajeMayor),
    forall((puntajeTotalCasa(OtraCasa, PuntajeMenor)), PuntajeMayor > PuntajeMenor).

hizo(hermione, responderPregunta(dondeSeEncuentraUnBezoar, 20, snape)).
hizo(hermione, responderPregunta(comoHacerLevitarUnaPluma, 25, flitwick)).

puntajeQueGenera(responderPregunta(_, Dificultad, snape), Puntaje):-
    Puntaje is Dificultad / 2.
puntajeQueGenera(responderPregunta(_, Dificultad, Profesor), Dificultad):-
    Profesor \= snape.
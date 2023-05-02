%1) BASE DE CONOCIMIENTO
%creeEn(Persona, Personaje)
creeEn(gabriel, campanita).
creeEn(gabriel, magoDeOz).
creeEn(gabriel, cavenaghi).
creeEn(juan, conejoDePascua).
creeEn(macarena, reyesMagos).
creeEn(macarena, magoCapria).
creeEn(macarena, campanita).

%suenio(Persona, Suenio)
suenio(gabriel, loteria([5,9])).
suenio(gabriel, futbolista(arsenal)).
suenio(juan, cantante(100000)).
suenio(macarena, cantante(10000)).

%amigo(Personaje1, Personaje2)
amigo(campanita, reyesMagos).
amigo(campanita, conejoDePascua).
amigo(conejoDePascua, cavenaghi).

%2) Ambicioso
ambicioso(Persona):-
    suenio(Persona, _),
    findall(DificultadSuenio, (suenio(Persona, Suenio), dificultad(Suenio, DificultadSuenio)), Lista),
    sumlist(Lista, Sumatoria),
    Sumatoria > 20.

%dificultad(suenio, valor)
dificultad(cantante(Discos), 6):- Discos > 500000.
dificultad(cantante(Discos), 4):- Discos =< 500000.
dificultad(loteria(ListaNros), Valor):- length(ListaNros, CantNros), Valor is (10 * CantNros).
dificultad(futbolista(Equipo), 3):- equipoChico(Equipo).
dificultad(futbolista(Equipo), 16):- not(equipoChico(Equipo)).

equipoChico(arsenal).
equipoChico(aldosivi).

%3) tieneQuimica(Personaje, Persona)
tieneQuimica(Persona, Personaje):-
    creeEn(Persona, _),
    creeEn(_, Personaje),
    creeEn(Persona, Personaje),
    condicionQuimica(Persona, Personaje).

condicionQuimica(Persona, campanita):-
    not(forall(suenio(Persona, Suenio),
    (dificultad(Suenio, DificultadSuenio),
    DificultadSuenio > 5))).

condicionQuimica(Persona, _):-
    not(ambicioso(Persona)), 
    forall(suenio(Persona, Suenio),
    suenioPuro(Suenio)).

suenioPuro(futbolista(_)).
suenioPuro(cantante(Discos)):- Discos < 200000.

%4) puedeAlegrar(Personaje, Persona)
puedeAlegrar(Personaje, Persona):-
    creeEn(Persona, _),
    creeEn(_, Personaje),
    suenio(Persona, _),
    tieneQuimica(Persona, Personaje),
    cumpleCondicionAlegrar(Personaje, Persona).

cumpleCondicionAlegrar(Personaje, _):-
    not(enfermo(Personaje)).
cumpleCondicionAlegrar(Personaje, _):-
    enfermo(Personaje),
    sonAmigos(Personaje, PersonajeBackup),
    not(enfermo(PersonajeBackup)).

%enfermo(Personaje)
enfermo(campanita).
enfermo(reyesMagos).
enfermo(conejoDePascua).

%sonAmigos(Personaje1, Personaje2)
sonAmigos(Personaje1, Personaje2):-
    amigo(Personaje1, Personaje2).
sonAmigos(Personaje1, Personaje2):-
    amigo(Personaje1, PersonajeIntermedio),
    amigo(PersonajeIntermedio, Personaje2),
    PersonajeIntermedio \= Personaje2,
    PersonajeIntermedio \= Personaje1.
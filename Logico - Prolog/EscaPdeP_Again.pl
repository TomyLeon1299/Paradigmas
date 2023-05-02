%persona(Apodo, Edad, Peculiaridades).
persona(ale, 15, [claustrofobia, cuentasRapidas, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).

%esSalaDe(NombreSala, Empresa).
esSalaDe(elPayasoExorcista, salSiPuedes).
esSalaDe(socorro, salSiPuedes).
esSalaDe(linternas, elLaberintoso).
esSalaDe(guerrasEstelares, escapepepe).
esSalaDe(fundacionDelMulo, escapepepe).

%terrorifica(CantidadDeSustos, EdadMinima).
%familiar(Tematica, CantidadDeHabitaciones).
%enigmatica(Candados).

%sala(Nombre, Experiencia).
sala(elPayasoExorcista, terrorifica(100, 18)).
sala(socorro, terrorifica(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(guerrasEstelares, familiar(futurista, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).

% PUNTO 1

nivelDeDificultadDeLaSala(Sala, Dificultad):-
    sala(Sala, Experiencia),
    nivelDeDificultad(Experiencia, Dificultad).

nivelDeDificultad(terrorifica(CantidadDeSustos, EdadMinima), Dificultad):-
    Dificultad is CantidadDeSustos - EdadMinima.

nivelDeDificultad(familiar(futurista, _), 15).

nivelDeDificultad(familiar(Tematica, CantidadDeHabitaciones), CantidadDeHabitaciones):-
    Tematica \= futurista.

nivelDeDificultad(enigmatica(Candados), Dificultad):-
    length(Candados, Dificultad).

% PUNTO 2

puedeSalir(Persona, Sala):-
    nivelDeDificultadDeLaSala(Sala, 1),
    noEsClaustrofobico(Persona).

puedeSalir(Persona, Sala):-
    persona(_, Edad, _),
    Edad > 13, 
    nivelDeDificultadDeLaSala(Sala, Dificultad),
    Dificultad < 5,
    noEsClaustrofobico(Persona).

noEsClaustrofobico(Persona):-
    persona(Persona, _, Peculiaridades),
    not(member(claustrofobia, Peculiaridades)).

% PUNTO 3

tieneSuerte(Persona, Sala):-
    persona(Persona, _, []),
    puedeSalir(Persona, Sala).

% PUNTO 4

esMacabra(Empresa):-
    esSalaDe(_, Empresa),
    forall(esSalaDe(NombreSala, Empresa), sala(NombreSala, terrorifica(_, _))).

% PUNTO 5

empresaCopada(Empresa):-
    not(esMacabra(Empresa)),
    promedioDificultad(Empresa, Promedio),
    Promedio < 4.

promedioDificultad(Empresa, Promedio):-
    esSalaDe(_, Empresa),
    findall(Dificultad, (esSalaDe(NombreSala, Empresa), nivelDeDificultadDeLaSala(Sala, Dificultad)), ListaDificultades),
    promedio(ListaDificultades, Promedio).

promedio(ListaDificultades, Promedio):-
    length(ListaDificultades, Cantidad),
    sumlist(ListaDificultades, Sumatoria),
    Promedio is Sumatoria / Cantidad.

% PUNTO 6

esSalaDe(estrellasDePelea, supercelula).
esSalaDe(choqueDeLaRealeza, supercelula).
sala(estrellasDePelea, familiar(videojuegos, 7)).

esSalaDe(miseriaDeLaNoche, skPista).
sala(miseriaDeLaNoche, terrorifica(150, 21)).

% Como la empresa Vertigo todavia no cuenta con ninguna sala, no lo agregamos a la base de conocimientos.
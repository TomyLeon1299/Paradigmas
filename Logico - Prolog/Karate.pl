%alumnoDe(Maestro, Alumno)
alumnoDe(miyagui, sara).
alumnoDe(miyagui, bobby).
alumnoDe(miyagui, sofia).
alumnoDe(chunLi, guidan).

%patadaRecta(potencia, distancia).
%patadaDeGiro(potencia, punteria, distancia).
%patadaVoladora(potencia, distancia, altura, punteria)
%codazo(potencia),
%golpeRecto(distancia, potencia).

% destreza(alumno, velocidad, [habilidades]).
destreza(sofia, 80, [golpeRecto(40, 3),codazo(20)]).
destreza(sara, 70, [patadaRecta(80, 2), patadaDeGiro(90, 95, 2), golpeRecto(1, 90)]).
destreza(bobby, 80, [patadaVoladora(100, 3, 2, 90), patadaDeGiro(50, 20, 1)]).
destreza(guidan, 70, [patadaRecta(60, 1), patadaVoladora(100, 3, 2, 90), patadaDeGiro( 70, 80 1)]).

% categoria(Alumno, Cinturones)
categoria(sofia, [blanco]).
categoria(sara, [blanco, amarillo, naranja, rojo, verde, azul, violeta, marron, negro]).
categoria(bobby, [blanco, amarillo, naranja, rojo, verde, azul, violeta, marron, negro]).
categoria(guidan, [blanco, amarillo, naranja]).

%1) esBueno/1: Se verifica si el alumno sabe hacer al menos dos patadas distintas o
%puede realizar un golpe recto a una velocidad media (entre 50 y 80, inclusive los límites).

esBueno(Alumno):-
    destreza(Alumno, _, ListaHabilidades),
    member(Habilidad1, ListaHabilidades),
    member(Habilidad2, ListaHabilidades),
    esPatada(Habilidad1),
    esPatada(Habilidad2),
    Habilidad1 \= Habilidad2.

esPatada(patadaRecta(_, _)).
esPatada(patadaDeGiro(_, _, _)).
esPatada(patadaVoladora(_, _, _, _)).

esBueno(Alumno):-
    destreza(Alumno, Velocidad, ListaHabilidades),
    member(golpeRecto(_, _), ListaHabilidades), 
    Velocidad >= 50,
    Velocidad =< 80.

%2)esAptoParaTorneo/1, se verifica si el alumno es bueno y además haya alcanzado el cinturón verde (puede que lo haya superado).

esAptoParaTorneo(Alumno):-
    esBueno(Alumno),
    categoria(Alumno, ListaCinturones),
    member(verde, ListaCinturones).

%3)totalPotencia/2, relaciona un alumno con la potencia total de todas sus habilidades. 
%La potencia total se calcula como la suma de las potencias de todas sus habilidades.

totalPotencia(Alumno, PotenciaTotal):-
    destreza(Alumno, _, ListaHabilidades),
    findall(Potencia, sacarPotencia(ListaHabilidades, Potencia), ListaPotencias),
    sumlist(ListaPotencias, PotenciaTotal).

sacarPotencia(ListaHabilidades, Potencia):-
    member(Habilidad, ListaHabilidades), 
    potenciaHabilidad(Habilidad, Potencia).

potenciaHabilidad(patadaRecta(Potencia,_),Potencia).
potenciaHabilidad(patadaDeGiro(Potencia,_,_),Potencia).
potenciaHabilidad(patadaVoladora(Potencia,_,_,_),Potencia).
potenciaHabilidad(codazo(Potencia),Potencia).
potenciaHabilidad(golpeRecto(_,Potencia),Potencia).

%4)alumnoConMayorPotencia/1,conocer el alumno que sea máximo según su cantidad de potencia.

alumnoConMayorPotencia(Alumno):-
    totalPotencia(Alumno, PotenciaTotal),
    forall(totalPotencia(OtraCosa, PotenciaTotal2), PotenciaTotal >= PotenciaTotal2).

%5)sinPatadas/1, permite conocer si un alumno no sabe realizar patadas.

sinPatadas(Alumno):-
    destreza(Alumno, _, ListaHabilidades),
    forall(member(Habilidad, ListaHabilidades), not(esPatada(Habilidad))).

%6)soloSabePatear/1, se verifica si un alumno sólo sabe únicamente realizar patadas.

soloSabePatear(Alumno):-
    destreza(Alumno, _, ListaHabilidades),
    forall(member(Habilidad, ListaHabilidades), esPatada(Habilidad)).

%Qué comiencen los Torneos!!.
%7) potencialesSemifinalistas/1, conocer el conjunto de los posibles alumnos semifinalistas.
%Para llegar a la semifinal debe cumplir alguna de las condiciones:
%   ● Ser aptos para el torneo;
%   ● Provenir de un maestro que tenga más de un alumno.
%   ● Deben poder realizar alguna habilidad con buen estilo artístico, es decir con una potencia de 100 o una puntería de 90.

potencialesSemifinalistas(Lista):-
    findall(Alumno, potencialSemifinalista(Alumno), Lista).

potencialSemifinalista(Alumno):-
    esAptoParaTorneo(Alumno),
    alumnoDe(Maestro, Alumno),
    alumnoDe(Maestro, OtroAlumno),
    Alumno \= OtroAlumno,
    

%8) semifinalistas/1, conocer todos los posibles conjuntos de alumnos que llegan a la semifinal.
%Tener en cuenta que en la semifinal llegan sólo 4 alumnos.



%9) Justificar donde se aprovecharon los conceptos de polimorfismo y orden superior. Y qué beneficios tiene su uso en la solución.
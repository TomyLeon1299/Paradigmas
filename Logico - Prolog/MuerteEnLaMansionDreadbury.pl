viveEnLaMansion(agatha).
viveEnLaMansion(mayordomo).
viveEnLaMansion(charles).

odia(agatha, Nombre):-
    viveEnLaMansion(Nombre),
    Nombre \= mayordomo.

odia(charles, Nombre):-
    noOdia(Nombre, agatha).

odia(mayordomo, Nombre):-
    odia(agatha, Nombre).

noOdia(PrimerNombre, SegundoNombre):-
    viveEnLaMansion(PrimerNombre),
    viveEnLaMansion(SegundoNombre),
    not(odia(SegundoNombre, PrimerNombre)).

esMasRicoQue(agatha, Nombre):-
    noOdia(Nombre, mayordomo).

mata(Asesino, Muerto):-
    odia(Asesino, Muerto),
    not(esMasRicoQue(Muerto, Asesino)).

% 1)     mata(Asesino, agatha).         devuelve:    Asesino = agatha.

% 2)
% odia(_, milhouse).          devuelve:    false.
% odia(charles, Nombre).      devuelve:    Nombre = mayordomo.
% odia(Nombre, agatha).       devuelve:    Nombre = agatha;    Nombre = mayordomo.
% ??
% odia(mayordomo, _).         devuelve:    true.
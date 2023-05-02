% 2do Parcial - Logico Prolog - Tomas Leon

% esPersonaje/1 nos permite saber qué personajes tendrá el juego
esPersonaje(aang).
esPersonaje(katara).
esPersonaje(zoka).
esPersonaje(appa).
esPersonaje(momo).
esPersonaje(toph).
esPersonaje(tayLee).
esPersonaje(zuko).
esPersonaje(azula).
esPersonaje(iroh).

% esElementoBasico/1 nos permite conocer los elementos básicos que pueden controlar algunos personajes
esElementoBasico(fuego).
esElementoBasico(agua).
esElementoBasico(tierra).
esElementoBasico(aire).

% elementoAvanzadoDe/2 relaciona un elemento básico con otro avanzado asociado
elementoAvanzadoDe(fuego, rayo).
elementoAvanzadoDe(agua, sangre).
elementoAvanzadoDe(tierra, metal).

% controla/2 relaciona un personaje con un elemento que controla
controla(zuko, rayo).
controla(toph, metal).
controla(katara, sangre).
controla(aang, aire).
controla(aang, agua).
controla(aang, tierra).
controla(aang, fuego).
controla(azula, rayo).
controla(iroh, rayo).

% visito/2 relaciona un personaje con un lugar que visitó. Los lugares son functores que tienen la siguiente forma:
% reinoTierra(nombreDelLugar, estructura)
% nacionDelFuego(nombreDelLugar, soldadosQueLoDefienden)
% tribuAgua(puntoCardinalDondeSeUbica)
% temploAire(puntoCardinalDondeSeUbica)
visito(aang, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(iroh, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(zuko, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(toph, reinoTierra(fortalezaDeGralFong, [cuartel, dormitorios, enfermeria, salaDeGuerra, templo, zonaDeRecreo])).
visito(aang, nacionDelFuego(palacioReal, 1000)).
visito(katara, tribuAgua(norte)).
visito(katara, tribuAgua(sur)).
visito(aang, temploAire(norte)).
visito(aang, temploAire(oeste)).
visito(aang, temploAire(este)).
visito(aang, temploAire(sur)).
visito(tomi, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).

% PUNTO 1

esElAvatar(Personaje):-
    controla(Personaje, fuego),
    controla(Personaje, agua),
    controla(Personaje, tierra),
    controla(Personaje, aire).

% PUNTO 2

noEsMaestro(Personaje):-
    esPersonaje(Personaje),
    not(controla(Personaje, _)).

esMaestroPrincipiante(Personaje):-
    controla(Personaje, Elemento),
    esElementoBasico(Elemento),
    not(esMaestroAvanzado(Personaje)).

esMaestroAvanzado(Personaje):-
    esElAvatar(Personaje).
esMaestroAvanzado(Personaje):-
    controla(Personaje, ElementoAvanzado),
    elementoAvanzadoDe(_, ElementoAvanzado).

% PUNTO 3

sigueA(zuko, aang).
sigueA(Personaje, OtroPersonaje):-
    visito(Personaje, _),
    visito(OtroPersonaje, _),
    Personaje \= OtroPersonaje,
    forall(visito(Personaje, Lugar), visito(OtroPersonaje, Lugar)).

% PUNTO 4

esDignoDeConocer(temploAire(_)).
esDignoDeConocer(tribuAgua(norte)).
esDignoDeConocer(reinoTierra(Nombre, Estructura)):-
    visito(_, reinoTierra(Nombre, Estructura)),
    not(member(muro, Estructura)).
% como ningun lugar de la nacion del fuego es digno de conocer diractamente no lo tomamos en cuenta.

% PUNTO 5

esPopular(Lugar):-
    visito(_, Lugar),
    findall(Visitante, visito(Visitante, Lugar), Visitantes),
    length(Visitantes, Cantidad),
    Cantidad > 4.

% PUNTO 6

esPersonaje(bumi).
controla(bumi, tierra).
visito(bumi, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).

esPersonaje(suki).
visito(suki, nacionDelFuego(presionMaximaSeguridad, 200)).
% como suki no controla ningun elemento, no agregamos nada sobre esta informacion a la base de conocimientos.
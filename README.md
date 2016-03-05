# Como compilarlo

- Bajarse [cc65](http://cc65.github.io/cc65/) y ponerlo en el path
- Bajarse [exomizer](http://hem.bredband.net/magli143/exo/) y ponerlo en el path
- Bajarse [VICE](http://vice-emu.sourceforge.net/) y ponerlo en el path

Y luego darle `make`


# Internals

## Directorio

- `src`: todo lo que se compila o se incluye: source code, assets listos para ser incluidos, SIDs comprimidos (.exo)
- `res`: los assets en formato "original". Ej: archivos .vcharproj, .sid, etc.
- `bin`: el binario compilado

## Memoria

Ver el archivo [intro.cfg](intro.cfg) que muestra como se usa la memoria. 


# Creditos

. Fonts del scroll: Del jueguito [Caren](http://csdb.dk/release/?id=141659)
. Logo: De Commodore
. Letras "MANIA": Ni idea. Me lo pasaron, pero supongo que fueron rippeadas de algun lado
. Fonts usados para texto del medio: Ni idea.
. Código: [riq](http://retro.moe/)

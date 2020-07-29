# Proyecto de Haskell.

## Haboard

### Nombre: Ignacio Delgadillo Vera 
### Rol 2503020-6

:star: Se modificó la función  de evaluacion ```foxAndHoundsEval``` con una búsqueda
 en amplitud para determinar un ppuntaje del numero de pasos para que el Fox llegue
  al otro lado del tablero. El código se explica a continuación :question::question::

```haskell
foxAndHoundsEval :: FoxAndHounds -> Float
foxAndHoundsEval (FoxAndHounds c pcs) = let
```
:bulb: La firma, que debia mantenerse, recibe un tipo de dato ```FoxAndHounds```y 
retorna un valor ```Float```; la funcion recibe el tipo de dato y se asigna a ```c```
el jugador y a ```pcs```la lista con los estados de cada pieza. ```let``` como las 
variables pre asignadas antes de entregar la respuesta en ```in```.

```haskell
    pc = filter (\(p,_,_,_) -> p == 0) pcs
```
:bulb: ```pc``` contiene una lista con los estados del fox que se extraen de ```pcs```, en un principio solo la pocisión inicial, que posteriormente será una lista de posibles estados del Fox.

```haskell
    dirtyMoves = map (pieceMoves (FoxAndHounds c pcs)) pc 
``` 
:bulb: Obtenemos todos los movimientos posibles del fox utilizando la función 
```pieceMoves```, le entregamos un ```FoxAndHounds```y la pieza que deseamos que 
calcule, en este caso ```pc```, y entrega una lista ```[(String, FoxAndHounds)]```
con los movimientos posibles. Se utiliza ```map``` porque ```pc```puede ser una lista de movimientos de fox que se suman al árbol de posibilidades. 

```haskell
    setMoves = foldl (++) [] dirtyMoves
    stateMoves = map (snd) setMoves
    allMoves = [m |(FoxAndHounds c m) <- stateMoves]
    listMoves = foldl (++) [] allMoves
```
:bulb: La lista ```dirtyMoves``` debe limpiarse ya que contiene datos que no será útiles; 
como ```dirtyMoves``` entrega una listas con listas de movimientos, lo primero es juntar las listas, para esto se utiliza ```setMoves``` que utiliza ```foldl``` para formar una sola lista. 
Lo siguiente es ```stateMoves``` que toma el segundo elemento de cada tupla que en este caso contiene un ```FoxAndHounds```; ```allMoves``` obtiene las listas de estados del 
```FoxAndHounds```, y finalmente, ```listMoves``` junta las listas de listas de estados dejando
una sola lista de estados.

```haskell
    foxMoves = filter (\(p,_,_,_) -> p== 0) listMoves
```
:bulb: ```foxMoves``` filtra solo los movimientos del fox de todos lo demas estados, para formar el nivel del árbol y reutilizar sus estados para generar los nuevos mmovimientos.

```haskell
    end = filter (\(_,_,y,_) -> y == 0) foxMoves
    hounds = filter (\(p,_,_,_) -> p == 1) pcs
    newFoxAndHounds = FoxAndHounds c (foxMoves ++ hounds)
```
:bulb: ```end``` contiene los estados del fox que llegó al otro lado del tablero, para indicar
que es un movimiento de término. ```hounds``` rescata de ```pcs```los estados de los hounds en
el truno para reutilizarlos y formar en ```newFoxAndHounds``` un tipo ```FoxAndHounds``` con los estados de los hounds mas todos los posibles movimientos del fox.

```haskell
in (1.0 + (if ((length foxMoves) > 500) then 10.0 else if (null end) then (foxAndHoundsEval (newFoxAndHounds)) else 1.0))*(if c==0 then 1.0 else -1.0)
``` 
:bulb: ```in``` contiene la salida que es 1 + , dependiendo de la situación, 
```(length foxMoves) > 500``` evalúa si los movimientos tienden a un loop evaluando 
con 10 puntos que expresa que la ruta esta bloqueada, ```(null end)```comprueba reliza
 recursión si no hay elementos en ```end``` que significa que el árbol no esta completo, 
 ó evalúa con 1 si el árbol termina; lo anterior se multiplica por 1 o -1 dependiendo si 
 el jugador es un fox ó son los hounds. 



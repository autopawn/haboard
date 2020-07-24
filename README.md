# haboard

## Por Pedro Yáñez, 201873049-1

Se desarrolló el juego Hare and Hounds, para esto me basé en el código entregado para el juego Fox and Hounds.

## Instrucciones de Compilación

Acceder al directorio haboard desde la consola y escribir:

`make harehounds`

luego ingresar el siguiente comando:

`./bin/harehounds`

## Definir el Tamaño del Tablero

El tablero tiene, en teoría, un tamaño de 9 x 5, sin embargo en la práctica el tamaño del tablero no tiene muchos de los espacios que usaría un espacio 9 x 5, esto se explica más adelante.

## Definir la Instancia Hare and Hounds

Me basé en la instancia Fox and Hounds:

* Current es igual al original.
* Winner (condiciones de victoria) cambia:
  * Aquí se define que si Rabbit logra pasar a todas las fichas enemigas sin ser acorralado, habrá ganado. Para lograr esto se tuvo que definir una función getPos en Piece.hs para obtener la posición de una ficha y pode iterar con ella (esto porque no supe obtener la posición de la pieza Hare mientras estaba "viendo" a las pieza Hound). Se crea una lista usando filter, siendo la función del filter que la pieza actual sea un perro y además la distancia en x entre ese perro y la liebre sea positiva (ej: si la liebre está en x = 4 y el perro está en x = 6, la distancia calculada sería -2), si la lista resultante del filter está vacía, significa que no hay ningún perro por pasar y la liebre ganó. ![Primera línea en la modificación de las condiciones de victoria](media/1.png?raw=true "1")

  * La otra condición de victoria, es que un jugador se quede sin movimientos, si le pasa a los perros, significa que llegaron a la parte más a la derecha del tablero por lo tanto habrán perdido (solo pueden moverse a la derecha), si le pasa a la liebre es porque la han acorralado y a perdido.

## Definir los Movimientos Posibles para Cada Pieza

Para definir correctamente los movimientos de cada pieza en un momento determinado, se definieron una serie de posiciones en el tablero (las posiciones cambian dependiendo si la pieza es Hound o Hare). Esto se hizo así para minimizar la cantidad de guards, en vez de hacer un caso para cada pieza, se agruparon las posiciones que comparten movimientos en una sola (por ejemplo, si la pieza está en x = 2 y = 0 o en x = 2 y = 0, tendrá los mismos movimientos pero se filtrarán los movimientos no legales mas adelante, o sea los que están fuera del tablero)

* Posiciones para la Liebre: ![Posiciones para la Liebre](media/2.png?raw=true "2")

* Posiciones para los Perros: ![Posiciones para los Perros](media/3.png?raw=true "3")

El filtrado de los movimientos válidos es simple, todos los movimientos que no estén en y = 2 y en x ∈ {0,2,4,6,8} o en y ∈ {0,4} y en x ∈ {2,4,6} (o séa, los espacios válidos en el tablero) son excluídos de la lista de movimientos final.

## Estado Inicial del Juego

Hay 4 piezas:

* Tres piezas Hound:

  * Una pieza en x = 2 e y = 0.

  * Una pieza en x = 0 e y = 2.

  * Una pieza en x = 4 e y = 4.

* Una pieza Hare en x = 8 e y = 2.

Se hizo que el jugador 1 (Hounds) sea el primero en mover para adherirse a las reglas del juego original.

## Transformación del Estado a String

Para lograr esto se apromeximó el problema de una manera similar a lo que se hizo para la lista de movimientos, se agruparon ciertas posiciones en el tablero que contienen los mismos símbolos, de la siguiente forma:

![Guards para convertir a String](media/4.png?raw=true "4")

## Main

Para correr el juego se imprimen mensajes que explican un poco el funcionamiento del juego y también le da nombre al jugador humano.

## Supuestos

* No se modificó la inteligencia (cpuEval) entregada en Fox And Hounds, ya que se entiende que no es parte de la Issue.
* La única forma de ganar, para Hare, es pasar a las fichas oponentes.
* La única forma de ganar, para Hound, es acorralar a la ficha Hare (dejándolo sin movimientos).
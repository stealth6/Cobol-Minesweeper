Cobol-Minesweeper
=================

Project - Mijnenveger

Als het programma begint krijgt u eerst een menu te zien:
-New game
-Load game
-Highscores
-Quit

New Game
Als de gebruiker kiest om een nieuw spel te beginnen dan wordt hij gevraagd om een naam in te geven. Deze is nodig als hij een highscore haalt.
Nu wordt door de programma een 10 x 10 matrix opgesteld en de mijnen worden geplaatst, maar de gebruiker weet niet waar ze zitten. Het is nu aan de gebruiker om co�rdinaten in te geven tot alle vakjes zichtbaar zijn waar geen mijn zit.
Indien de gebruiker een co�rdinaat ingeeft waaronder een mijn ligt is het spel afgelopen.
In plaats van een co�rdinaat in te geven kan de gebruiker ook '0' intypen. Dan wordt de huidige stand van het spel opgeslagen.
Door 11 in te geven word het spel be�indigd en keert de gebruiker terug naar het hoofd menu.

Begin situatie:

01 |    |    |    |    |    |    |    |    |    |    |
02 |    |    |    |    |    |    |    |    |    |    |
03 |    |    |    |    |    |    |    |    |    |    |
04 |    |    |    |    |    |    |    |    |    |    |
05 |    |    |    |    |    |    |    |    |    |    |
06 |    |    |    |    |    |    |    |    |    |    |
07 |    |    |    |    |    |    |    |    |    |    |
08 |    |    |    |    |    |    |    |    |    |    |
09 |    |    |    |    |    |    |    |    |    |    |
10 |    |    |    |    |    |    |    |    |    |    |
-----------------------------------------------
	01	 02	  03	04	 05	  06   07	08	09	 10

speciaal geval: Als een vakje een waarde van 0 heeft (leeg) dan wordt niet alleen deze vakje onthuld maar ook alle omliggende vakjes. Als er dan in een van die omliggende vakjes ook 0 zit worden ook zijn omliggende onthuld enzovoort.

Mijnen Toevoegen
Er werd al gezegd dat de mijnen door de programma geplaatst worden. Om een mijn te leggen kiest het programma twee willekeurige getallen van 1-10 en gebruikt deze als co�rdinaten van een nieuwe mijn. Als er al een mijn op die plek ligt dan worden 2 nieuwe getallen gekozen. het programma blijft willekeurige co�rdinaten proberen tot het 10 mijnen heeft gelegd, dan pas begint het spel.
Als een mijn wordt toegevoegd krijgt de vakje als waarde M en wordt de waarde van alle omliggende vakjes met 1 verhoogt:

1 | 1 | 1
1 | M | 1
1 | 1 | 1






In het geval dat 2 mijnen dicht bij elkaar liggen krijgen we zoiets:

1 | 1 | 1 | 
1 | M | 2 | 1 | 1
1 | 1 | 2 | M | 1
0 | 0 | 1 | 1 | 1

Load game
Als de gebruiker load game kiest dan wordt er een opgeslagen spel geladen vanuit een bestand.

Spel Opslaan
Tijden het spel kan de gebruiker in plaats van een co�rdinaat ingeven 0 ingeven. Dan wordt de huidige stand van het spel weggeschreven in een bestand. (dan kan je het vanaf de hoofd menu herladen)

Highscores
Als de gebruiker highscores kiest krijg hij de 5 snelste tijden te zien. Als hij een spel wint wordt ook gekeken of hij een highscore heeft behaald. Dit wil zeggen dat hij sneller was dan een van de 5 tijden in de highscore bestand. Als dit het geval is wordt zijn naam en tijd op de desbetreffende plaats gezet en worden de andere opgeschoven. De laatste valt dan weg.

Bestanden structuur

Bestand mijnenveld
Deze bestand bewaart een opgeslagen spel.

Voorbeeld structuur:

Regel 01 :0000000000
Regel 02 :0111000000
Regel 03 :01M1001110
Regel 04 :0112111M10
Regel 05 :1111M12220
Regel 06 :2M21111M10
Regel 07 :2M20001110
Regel 08 :0000000000
Regel 09 :0000000000
Regel 10 :0000000000
Regel 11 :0000000000
Regel 12 :0111000000
Regel 13 :01 1001110
Regel 14 :01 2111 10
Regel 15 :
Regel 16 :
Regel 17 :
Regel 18 :
Regel 19 :
Regel 20 :
Regel 21 :000030

De Eerste 10 regels bevatten informatie over de layout van het spel. Dit wil zeggen waar liggen de mijnen, en hoe zien de omliggende vakken eruit.
De volgende 10 regels bevatten informatie over hoe ver de gebruiker was met te zoeken. (De matrix die hij zag op zijn scherm)
Op de 21ste regel staat de reeds verlopen tijd in seconden, zodat als het spel hervat wordt de timer niet opnieuw begint.


Bestand Highscores
Deze bestand bewaart de highscores. Dit wil zeggen de 5 snelste tijden, gesorteerd met de snelste van boven.
Regel 1 :Naam1     000900
Regel 2 :Naam2     001000
Regel 3 :Naam3     002000
Regel 4 :Naam4     003000
Regel 5 :Naam4     004560


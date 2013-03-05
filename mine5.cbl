       IDENTIFICATION DIVISION.
       PROGRAM-ID. Minesweeper.
       
      * This is a RM COBOL version of Minesweeper
      * Made by Rory & Aquinas
      * Have fun!
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT oldgame ASSIGN TO "project/Mfield.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           
      *     http://www.mac13.nl/tvc/hoofdstuk05/paragraaf5.06.html
           
           SELECT highscores ASSIGN TO "project/scores.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD oldgame.
       01 line1.
         02 oldValue PIC x OCCURS 10.
       
       FD highscores.
       01 line2.
         02 highName PIC x(10).
         02 hScore PIC 9(6).
       
      * ---------------------------------------------------------------
       
       WORKING-STORAGE SECTION.
      * File Vars
       77 PIC x value "n".
         88 eof1 value "j".
       77 PIC x value "n".
         88 eof2 value "j".
       
      * Menu input
       77 playerName PIC x(10).
       77 menuInput PIC 9 value 0.
       
      * Random Vars
       77 randomSeed PIC 9(6) value 0.
       77 randomNum PIC 9(12) value 0.
       
      * Current time
       77 ctime PIC 9(8) value 0.
       
      * Check Random stuck
       77 temp PIC 9(6) value 0.
       
      * Random Row & Column
       77 ranRow PIC 99 value 0.
       77 ranCol PIC 99 value 0.
       77 temp1 PIC 9 value 0.
       
      * Adding Mines
       77 tempRow PIC 99 value 0.
       77 tempCol PIC 99 value 0.
       77 tempVal PIC 9.
       77 minesLeft PIC 99 value 10.
       
      * Iterators
       77 i PIC 99 value 1.
       77 j PIC 99 value 1.
       
      * Checks
       77 check1 PIC 9 value 0.
       77 check2 PIC 9 value 0.
       
      * Playing the game
       77 coordX PIC 99 value 1.
       77 coordY PIC 99 value 1.
       77 quit PIC 9 value 0.
       
      * HighScore Start time & End time
       77 stime PIC 9(8).
       77 etime PIC 9(8).
       77 rtime PIC 9(6).
       77 caltime PIC 99.
       77 savetime PIC 9(6).
       
      * BubbleSort
       77 mini PIC 99 value 1.
       77 temp2 PIC 99 value 0.
       
      * File Output Structure
       01 tempLine.
         02 lineVal1 PIC x OCCURS 10.
         
       01 highLine.
         02 nameLine PIC x(10).
         02 scoreLine PIC 9(6).
      * ---------------------------------------------------------------
      * Two tables:
      * One with the layout of the minefield
      * One with the current level of visibility
       
       01 layout.
         02 mineRow OCCURS 10.
           03 mineCol OCCURS 10.
             04 FILLER PIC xxx value " | ".
             04 mineField PIC x value "0".
               88 isMine value "M".
       
       01 visibility.
         02 visiRow OCCURS 10.
           03 visiCol OCCURS 10.
             04 FILLER PIC xxx value " | ".
             04 visiField PIC x value " ".
               88 isEmpty value " ".
               88 explode value "M".
               88 isZero value "0".
       
      * ---------------------------------------------------------------
      * Highscores table
       
       01 highTable.
         02 highRow OCCURS 5.
           03 player PIC x(10) value spaces.
           03 pScore PIC 9(6) value zeroes.
       
      * ---------------------------------------------------------------
      
       PROCEDURE DIVISION.
       Hoofd.
           PERFORM IntroPlayer
           PERFORM RandomInit
           PERFORM Menu UNTIL menuInput = 4
           STOP RUN
           .
       
      * Generates a 12 digit number using the Middle square method
      * If the Seed is 0 or itself then we reinitialise
      * The initial Seed is the last 6 digits of the current time
      * minutes (2) / seconds (2) / split seconds (2)
       
       RandomInit.
           ACCEPT ctime FROM TIME
           MOVE ctime TO randomSeed
           .
           
       Randomizer.
           COMPUTE randomNum = randomSeed * randomSeed
           COMPUTE randomSeed = randomNum / 1000
           IF randomSeed = 0 OR randomSeed = temp THEN
               PERFORM RandomInit
           END-IF
           MOVE randomSeed TO temp
           .
           
      * ---------------------------------------------------------------
      * Main Menu
      
       Menu.
           DISPLAY "Main Menu"
           DISPLAY "1 - New Game"
           DISPLAY "2 - Load Game"
           DISPLAY "3 - Highscores"
           DISPLAY "4 - Exit"
           ACCEPT menuInput NO BEEP
           
           PERFORM UNTIL menuInput > 0 AND < 5
               DISPLAY "Error incorrect input. Try again"
               ACCEPT menuInput NO BEEP
           END-PERFORM
           
           IF menuInput = 1 THEN
             PERFORM NewGame
           ELSE IF menuInput = 2 THEN
               PERFORM LoadGame
             ELSE IF menuInput = 3 THEN
                 PERFORM Highscores
               ELSE IF menuInput = 4 THEN
                    DISPLAY "Like you have anything better to do!"
                 END-IF
               END-IF
             END-IF
           END-IF
           .
       
       IntroPlayer.
           DISPLAY "Welcome to Minesweeper"
           DISPLAY "Please Enter your alias : "
           ACCEPT playerName
           DISPLAY "Welcome " playerName " Have fun!"
           .
       
      * ---------------------------------------------------------------
      * New Game
       
       NewGame.
           PERFORM InitFields
           PERFORM AddMines
           PERFORM Play
           .
       
       InitFields.
           MOVE 0 To quit
           MOVE 10 TO minesLeft
           PERFORM ResetField VARYING i FROM 1 BY 1 UNTIL i > 10
             AFTER j FROM 1 BY 1 UNTIL j > 10
           .
       
       ResetField.
           MOVE "0" TO mineField(i, j)
           MOVE " " TO visiField(i, j)
           .
       
      * Initialize MineField
       AddMines.
      * We will generate 2 random numbers from 1-10
      * We fill them in row and column and try to add a mine
           PERFORM UNTIL minesLeft <= 0
             
             PERFORM Randomizer
             COMPUTE temp1 = randomNum / 10000000
             MOVE temp1 TO ranRow
             ADD 1 TO ranRow
             
      *       Debug Random Row
      *       DISPLAY " randomNum " randomNum
      *       DISPLAY " temp " temp1
      *       DISPLAY " ranRow " ranRow
             
             PERFORM Randomizer
             COMPUTE temp1 = randomNum / 10000000
             MOVE temp1 TO ranCol
             ADD 1 TO ranCol
             
      *       Debug Random Column
      *       DISPLAY " randomNum " randomNum
      *       DISPLAY " temp " temp1
      *       DISPLAY " ranCol " ranCol
             
             IF NOT isMine(ranRow, ranCol) THEN
               SET isMine(ranRow, ranCol) TO TRUE
               PERFORM BlastArea
               SUBTRACT 1 FROM minesLeft
             END-IF
             
             MOVE 00 TO ranRow
             MOVE 00 TO ranCol
           END-PERFORM
           .
       
      * Edits the surrounding fields adding 1 to their value
       BlastArea.
           SUBTRACT 1 FROM ranRow GIVING tempRow
           SUBTRACT 1 FROM ranCol GIVING tempCol
           PERFORM UNTIL tempRow >= ranRow + 2
             PERFORM UNTIL tempCol >= ranCol + 2
               IF ( tempRow > 0 AND <= 10 ) AND
      -           ( tempCol > 0 AND <= 10 )
                 IF NOT isMine(tempRow, tempCol) THEN
                   MOVE mineField(tempRow, tempCol) TO tempVal
                   ADD 1 TO tempVal
                   MOVE tempVal TO mineField(tempRow, tempCol)
                 END-IF
               END-IF
               ADD 1 TO tempCol
             END-PERFORM
             ADD 1 TO tempRow
             SUBTRACT 3 FROM tempCol
           END-PERFORM
           .
       
      * ---------------------------------------------------------------
      * Display Options
      
      * Display Current Status of game
       DispMineField.
           MOVE 1 TO i
           PERFORM 10 TIMES
               DISPLAY i "|" visiRow(i) " |"
               ADD 1 To i
           END-PERFORM
           DISPLAY "---------------------------------------------"
           DISPLAY "    |01 |02 |03 |04 |05 |06 |07 |08 |09 |10 |"
           .
       
      * Made for debugging purposes
       DispLayout.
           MOVE 1 TO i
           PERFORM 10 TIMES
               DISPLAY i "|" mineRow(i) " |"
               ADD 1 To i
           END-PERFORM
           DISPLAY "---------------------------------------------"
           DISPLAY "    |01 |02 |03 |04 |05 |06 |07 |08 |09 |10 |"
           .
       
      * ---------------------------------------------------------------
      * Main Game
       
      * Game Loop, quits when you win or loose
      * DISPLAYS for debugging
       Play.
           PERFORM UNTIL quit > 0
             PERFORM DispMineField
             PERFORM GetCoords
      *       DISPLAY " COORDS DONE "
             PERFORM Reveal
      *       DISPLAY " REVEAL DONE "
             PERFORM GameStatus
      *       DISPLAY " GAME STATUS DONE "
           END-PERFORM
           PERFORM Celebrate
           .
      * User input coordinates or save
       GetCoords.
           DISPLAY "Enter the Coordinates or 0 to save."
           MOVE 0 TO coordX
           MOVE 0 TO coordY
           
      *     Accept Row or Save
           PERFORM UNTIL coordX NOT = 0
             DISPLAY "Enter the Row (Numbers on the left) : "
             ACCEPT coordX NO BEEP
             PERFORM UNTIL coordX >= 0 AND <= 10
               DISPLAY "Error: Incorrect input, must be between 0-10"
               ACCEPT coordX NO BEEP
             END-PERFORM
             IF coordX = 0 THEN
               PERFORM SaveGame
             END-IF
           END-PERFORM
           
      *     Accept Column or Save
           PERFORM UNTIL coordY NOT = 0
             DISPLAY "Enter the Column (Numbers on the bottom) : "
             ACCEPT coordY NO BEEP
             PERFORM UNTIL coordY >= 0 AND <= 10
               DISPLAY "Error: Incorrect input, must be between 0-10"
               ACCEPT coordY NO BEEP
             END-PERFORM
             IF coordY = 0 THEN
               PERFORM SaveGame
             END-IF
           END-PERFORM
           .
       
      * ---------------------------------------------------------------
      * Revealing Fields
       
      * Reveal a square
       Reveal.
           IF (coordX > 0 AND <= 10) AND (coordY > 0 AND <= 10)
            IF isEmpty(coordX, coordY) THEN
            MOVE mineField(coordX, coordY) TO visiField(coordX, coordY)
             IF isZero(coordX, coordY) THEN
               PERFORM CheckNext
             END-IF
            END-IF
           END-IF
           .
       
      * In case of 0 also reveal neighbouring squares
       CheckNext.
           PERFORM CheckBlastArea
           PERFORM 5 TIMES
             PERFORM CheckAll VARYING i FROM 1 BY 1 UNTIL i > 10
               AFTER j FROM 1 BY 1 UNTIL j > 10
           END-PERFORM
           .
       
       CheckBlastArea.
           SUBTRACT 1 FROM coordX GIVING tempRow
           SUBTRACT 1 FROM coordY GIVING tempCol
           PERFORM UNTIL tempRow >= coordX + 2
             PERFORM UNTIL tempCol >= coordY + 2
               IF ( tempRow > 0 AND <= 10 ) AND
      -           ( tempCol > 0 AND <= 10 )
                 IF isEmpty(tempRow, tempCol) THEN
       MOVE mineField(tempRow, tempCol) TO visiField(tempRow, tempCol)
                 END-IF
               END-IF
               ADD 1 TO tempCol
             END-PERFORM
             ADD 1 TO tempRow
             SUBTRACT 3 FROM tempCol
           END-PERFORM
           .
       
       CheckAll.
           IF isZero(i, j) THEN
             MOVE i TO coordX
             MOVE j TO coordY
             PERFORM CheckBlastArea
           END-IF
           .
       
      * Reveal all squares
       RevealAll.
           PERFORM RevealAllLoop VARYING i FROM 1 BY 1 UNTIL i > 10
             AFTER j FROM 1 BY 1 UNTIL j > 10
           .
             
       RevealAllLoop.
           IF isEmpty(i, j) THEN
             MOVE mineField(i, j) TO visiField(i, j)
           END-IF
           .
       
      * ---------------------------------------------------------------
      * Load Game
       
       LoadGame.
           DISPLAY " LOADING "
           OPEN INPUT oldgame
      * Move first 10 lines to mineField
      * Represents where the mines were
           MOVE 1 TO i
           MOVE 1 TO j
           PERFORM 10 TIMES
             READ oldgame AT END SET eof1 TO TRUE END-READ
             PERFORM 10 TIMES
               MOVE oldValue(j) TO mineField(i, j)
               ADD 1 TO j
             END-PERFORM
             MOVE 1 TO j
             ADD 1 TO i
           END-PERFORM
      * Move the Last 10 lines to the visible Field
      * Represents the players progress when he saved
           MOVE 1 TO i
           MOVE 1 TO j
           PERFORM 10 TIMES
             READ oldgame AT END SET eof1 TO TRUE END-READ
             PERFORM 10 TIMES
               MOVE oldValue(j) TO visiField(i, j)
               ADD 1 TO j
             END-PERFORM
             MOVE 1 TO j
             ADD 1 TO i
           END-PERFORM
           CLOSE oldgame
           DISPLAY " GAME LOADED "
      * Game Loaded - Time to play :)
           PERFORM Play
           .
       
      * ---------------------------------------------------------------
      * Save Game
       
       SaveGame.
           DISPLAY " SAVING "
           OPEN OUTPUT oldgame
           
           MOVE 1 TO i
           MOVE 1 TO j
           PERFORM 10 TIMES
             PERFORM 10 TIMES
               MOVE mineField(i, j) TO lineVal1(j)
               ADD 1 TO j
             END-PERFORM
             WRITE line1 FROM tempLine END-WRITE
             MOVE 1 TO j
             ADD 1 TO i
           END-PERFORM
           
           MOVE 1 TO i
           MOVE 1 TO j
           PERFORM 10 TIMES
             PERFORM 10 TIMES
               MOVE visiField(i, j) TO lineVal1(j) 
               ADD 1 TO j
             END-PERFORM
             WRITE line1 FROM tempLine END-WRITE
             MOVE 1 TO j
             ADD 1 TO i
           END-PERFORM
           
           CLOSE oldgame
           
           DISPLAY " GAME SAVED "
           .
       
      * ---------------------------------------------------------------
      * Highscores
       
       Highscores.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
             DISPLAY " HIGHSCORES "
             DISPLAY player(i) " - " pScore(i)
           END-PERFORM
           .
       
       LoadScores.
           MOVE 1 To i
           OPEN INPUT highscores
           READ highscores AT END SET eof2 TO TRUE END-READ
           PERFORM UNTIL eof2
             MOVE highName TO player(i)
             MOVE hScore TO pScore(i)
             ADD 1 To i
             READ highscores AT END SET eof2 TO TRUE END-READ
           END-PERFORM
           CLOSE highscores
           .
       
       WriteScores.
           OPEN OUTPUT highscores
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
             MOVE player(i) TO highName
             MOVE pScore(i) TO hScore
             WRITE line2 END-WRITE
           END-PERFORM
           CLOSE highscores
           .
       
       GetStartTime.
           ACCEPT stime FROM TIME
           .
       
       GetEndTime.
           ACCEPT etime FROM TIME
           .
       
       isNewRec.
           DISPLAY " NOTHING "
           .
       
       BubbleGo.
           MOVE 0 TO check1
           PERFORM VARYING i FROM 10 BY -1 UNTIL i < mini
             IF pScore(i) < pScore(i - 1) THEN
               MOVE pScore(i) TO temp2
               MOVE pScore(i - 1) TO pScore(i)
               MOVE temp2 TO pScore(i - 1)
               MOVE 1 TO check1
             END-IF
           END-PERFORM
           ADD 1 TO mini
           ADD 1 TO j
           .
       
       BubbleMain.
           MOVE 1 TO j
           PERFORM BubbleGo UNTIL j > 9 OR check NOT = 1
           .
       
       ConvertToSec.
           COMPUTE stime = ( etime - stime )
           COMPUTE caltime = ( stime / 100 )
           ADD caltime TO rtime
           COMPUTE caltime = ( stime / 10000 )
           ADD caltime TO rtime
           MOVE stime TO caltime
           ADD caltime TO rtime
           ADD savetime TO rtime
           .
       
      * ---------------------------------------------------------------
      * Check current status (Do we continue playing?)
       
       GameStatus.
           MOVE 0 TO check1
           MOVE 0 TO check2
           PERFORM isGameOver VARYING i FROM 1 BY 1 UNTIL i > 10
             AFTER j FROM 1 BY 1 UNTIL j > 10
           IF check1 = 1 THEN
             MOVE 1 TO quit
           END-IF
           IF check2 = 0 THEN
             MOVE 2 To quit
           END-IF
           .
       
       isGameOver.
           IF explode(i, j) THEN
             MOVE 1 TO check1
           END-IF
           IF isEmpty(i, j) AND NOT isMine(i, j) THEN
             MOVE 1 TO check2
           END-IF
           .
       
      * ---------------------------------------------------------------
      * End Game (Win or Loose)
       
       Celebrate.
           IF quit = 1 THEN
             DISPLAY "****GAME OVER****"
             PERFORM DispLayout
           ELSE IF quit = 2 THEN
               DISPLAY "****CONGRATULATIONS!****"
               DISPLAY "***YOU BEAT THE GAME!***"
             END-IF
           END-IF
           .
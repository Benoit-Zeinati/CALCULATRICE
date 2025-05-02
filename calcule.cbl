       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCUL.
       AUTHOR. Benoit Zeinati.
       DATE-WRITTEN. 23/04/202
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-CALC-N1     PIC S9(9)V99 VALUE 0.
       01 WS-CALC-N2     PIC S9(9)V99 VALUE 0.
       01 WS-CALC-RESULT PIC S9(12)V99 VALUE 0.
       01 WS-CALC-OPER   PIC X.
       01 WS-CALC-RETRY  PIC A.
       01 WS-CALC-PWRIDX PIC 9(9).
       01 WS-CALC-ZRES   PIC -Z(9).99.


       
       PROCEDURE DIVISION.
       PARA-CALCUL.
       
       DISPLAY ' '.
       DISPLAY '              _________________________________'.
       DISPLAY '              |                               |'.
       DISPLAY '              |     THE COBOL CALCULATOR      |'.
       DISPLAY '              |                               |'.
       DISPLAY '              |-------------------------------|'.
       DISPLAY '              |                000000000000.00|'.
       DISPLAY '              |-------------------------------|'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |       |   %   |   ^   |   /   |'.
       DISPLAY '              |-------|-------|-------|-------|'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |   7   |   8   |   9   |   *   |'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |-------|-------|-------|-------|'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |   4   |   5   |   6   |   -   |'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |-------|-------|-------|-------|'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |   1   |   2   |   3   |   +   |'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |-------|-------|-------|-------|'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |       |   0   |   .   |   =   |'.
       DISPLAY '              |       |       |       |       |'.
       DISPLAY '              |-------|-------|-------|-------|'.
       DISPLAY '"^" stands for the power sign'.
       DISPLAY ' '.

       PERFORM UNTIL WS-CALC-RETRY = 'N' OR 'n'
         DISPLAY 'Enter first number (9 didgit max):'
         ACCEPT WS-CALC-N1
         MOVE WS-CALC-N1 TO WS-CALC-RESULT
         MOVE WS-CALC-N1 TO WS-CALC-ZRES
         MOVE SPACE TO WS-CALC-OPER
         PERFORM UNTIL WS-CALC-OPER = '='
            DISPLAY 'Select an operation: +, -, *, /, ^, %, ='
            MOVE SPACE TO WS-CALC-OPER
            PERFORM UNTIL WS-CALC-OPER = '+' OR '-' or '*' or '/' OR '^' 
               OR '%' OR '='
               ACCEPT WS-CALC-OPER
            END-PERFORM

            IF WS-CALC-OPER = '=' THEN
               DISPLAY ' '
      *         DISPLAY WS-CALC-RESULT
                DISPLAY WS-CALC-ZRES
            ELSE 
                  DISPLAY 'Enter next number (9 didgit max):'
                  ACCEPT WS-CALC-N2
      * 
                  EVALUATE WS-CALC-OPER
                           WHEN '+' 
                                COMPUTE WS-CALC-RESULT =  WS-CALC-RESULT
                                 + WS-CALC-N2
                                MOVE WS-CALC-RESULT TO WS-CALC-ZRES
                           WHEN '-'
                                COMPUTE WS-CALC-RESULT =  WS-CALC-RESULT
                                 - WS-CALC-N2
                                MOVE WS-CALC-RESULT TO WS-CALC-ZRES
                           WHEN '*'
                                COMPUTE WS-CALC-RESULT =  WS-CALC-RESULT
                                 * WS-CALC-N2
                                MOVE WS-CALC-RESULT TO WS-CALC-ZRES
                           WHEN '/'
                                IF WS-CALC-N2 = 0 THEN
                                   DISPLAY 'INVALID OPERATION'
                                ELSE 
                                   COMPUTE 
                                        WS-CALC-RESULT =  WS-CALC-RESULT
                                        / WS-CALC-N2
                                   MOVE WS-CALC-RESULT TO WS-CALC-ZRES
                           WHEN '^'
                                PERFORM PARA-POWER THRU PARA-POWER-END
                           WHEN '%'
                                COMPUTE WS-CALC-RESULT = WS-CALC-RESULT
                                 * WS-CALC-N2 / 100
                                MOVE WS-CALC-RESULT TO WS-CALC-ZRES
                  END-EVALUATE
      *     
            END-IF

         END-PERFORM
      *
         DISPLAY ' '
         Display 'RETRY Y/N:'
         MOVE SPACE TO WS-CALC-RETRY
         PERFORM UNTIL WS-CALC-RETRY = 'Y' OR 'y' OR 'N' OR 'n'
                 ACCEPT WS-CALC-RETRY
         END-PERFORM
       END-PERFORM.

       STOP RUN.

       PARA-POWER.
       IF WS-CALC-N2 < 0 THEN
          DISPLAY 'INVALID OPERATION'
       ELSE  
          COMPUTE WS-CALC-RESULT = WS-CALC-RESULT ** WS-CALC-N2
          MOVE WS-CALC-RESULT TO WS-CALC-ZRES
       END-IF.
       PARA-POWER-END.
       EXIT.
       

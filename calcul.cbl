       IDENTIFICATION DIVISION.
       PROGRAM-ID. calcul.
       AUTHOR. Thomas Baudrin.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       
       01  WS-NUM-1            PIC S9(3)V99.
       01  WS-NUM-2            PIC S9(3)V99.
       01  WS-VISUAL-NUM-1     PIC Z(2)9.99.
       01  WS-VISUAL-NUM-2     PIC Z(2)9.99.
       01  WS-OPERATOR         PIC X.
       01  WS-RESULT           PIC S9(9)v99.
       01  WS-VISUAL-RESULT    PIC Z(8)9.99.
       01  WS-STORAGE          PIC X(255).
       01  WS-CALCUL           PIC X(100).
       01  WS-INPUT            PIC X(5).
       01  WS-BOOL             PIC X            VALUE 'N'.
       01  WS-CONTINUE         PIC X            VALUE 'N'.

       PROCEDURE DIVISION.

       PERFORM UNTIL FUNCTION LOWER-CASE(WS-INPUT) = "quit0"
            
           DISPLAY "Entrez quit pour quitter" 
           DISPLAY "Entrez last pour voir les précédents calculs"
           DISPLAY "Entrez un nombre (3 chiffres max et 2 décimal)"
           ACCEPT WS-INPUT

           MOVE WS-INPUT TO WS-NUM-1

           PERFORM 0100-INSPECT-INPUT THRU 0100-INSPECT-INPUT-END
           
           PERFORM 0200-VERIFY-NUM-1 THRU 0200-VERIFY-NUM-1-END
           
           PERFORM UNTIL FUNCTION LOWER-CASE(WS-INPUT) = "quit0" 
               OR FUNCTION LOWER-CASE(WS-INPUT) = "non"

               PERFORM 0300-OPERATOR THRU 0300-OPERATOR-END
      
               PERFORM 0400-NUM-2 THRU 0400-NUM-2-END
      
               PERFORM 0600-CALCUL THRU 0600-CALCUL-END
      
               PERFORM 0700-CONTINUE-CALCUL 
                   THRU 0700-CONTINUE-CALCUL-END

           END-PERFORM

       END-PERFORM.    
       STOP RUN.

      ******************************************************************

       0100-INSPECT-INPUT.
           INSPECT WS-INPUT REPLACING ALL SPACES BY '0'
           INSPECT WS-INPUT REPLACING FIRST "-" BY '0'
           INSPECT WS-INPUT REPLACING FIRST "." BY '0'
       .

       0100-INSPECT-INPUT-END.
           EXIT
       .

       0200-VERIFY-NUM-1.
           
           IF FUNCTION LOWER-CASE(WS-INPUT) = "quit0"

               CONTINUE

           ELSE IF FUNCTION LOWER-CASE(WS-INPUT) = "last0"

               DISPLAY WS-STORAGE
               MOVE "non" TO WS-INPUT

           ELSE IF WS-INPUT IS NOT NUMERIC

               DISPLAY "Saisissez un nombre"
               MOVE "non" TO WS-INPUT

           ELSE 

               MOVE WS-NUM-1 TO WS-VISUAL-NUM-1

               PERFORM 0210-CALCUL-STRING-1 
                   THRU 0210-CALCUL-STRING-1-END

           END-IF
       .

       0200-VERIFY-NUM-1-END.
           EXIT
       .

       0210-CALCUL-STRING-1.
           IF WS-NUM-1 < 0
               STRING 
                   " (-" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-VISUAL-NUM-1) DELIMITED BY SIZE
                       INTO WS-CALCUL
               END-STRING
           ELSE 
               STRING 
                   " (" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-VISUAL-NUM-1) DELIMITED BY SIZE
                       INTO WS-CALCUL
               END-STRING
           END-IF
           
       .

       0210-CALCUL-STRING-1-END.
           EXIT 
       . 

       0300-OPERATOR.
           PERFORM UNTIL WS-BOOL = 'Y'
               
               DISPLAY WS-CALCUL
               DISPLAY "Entrez l'opérateur ( +, -, x, /, ^ )"
               ACCEPT WS-INPUT
               
               IF WS-INPUT = "+" OR WS-INPUT = "-" OR WS-INPUT = "x"
                   OR WS-INPUT = "/" OR WS-INPUT = "^"

                  MOVE WS-INPUT TO WS-OPERATOR
                  MOVE 'Y' TO WS-BOOL

                  PERFORM 0310-OPERATOR-STRING 
                      THRU 0310-OPERATOR-STRING-END

               ELSE

                   DISPLAY "Mauvaise saisi"

               END-IF
               
           END-PERFORM

           PERFORM 0500-INITIALIZE-WS-BOOL 
               THRU 0500-INITIALIZE-WS-BOOL-END
       .
    
       0300-OPERATOR-END.
           EXIT
       .

       0310-OPERATOR-STRING.
           STRING

               FUNCTION TRIM(WS-CALCUL) 
                   DELIMITED BY SIZE
               WS-OPERATOR DELIMITED BY SIZE
               INTO WS-CALCUL

           END-STRING
       .

       0310-OPERATOR-STRING-END.
           EXIT 
       .

       0400-NUM-2.
           PERFORM UNTIL WS-BOOL = 'Y'

               DISPLAY WS-CALCUL
               DISPLAY "Entrez un autre nombre (3 chiffres max)"
               ACCEPT WS-INPUT
               MOVE WS-INPUT TO WS-NUM-2

               PERFORM 0100-INSPECT-INPUT THRU 0100-INSPECT-INPUT-END

               IF WS-OPERATOR = "/" AND WS-NUM-2 = 0

                   DISPLAY "Pas de division par 0"

               ELSE IF WS-INPUT IS NUMERIC

                   MOVE 'Y' TO WS-BOOL
                   MOVE WS-NUM-2 TO WS-VISUAL-NUM-2

                   PERFORM 0410-CALCUL-STRING-2 
                       THRU 0410-CALCUL-STRING-2-END
                 
               ELSE

                   DISPLAY "Mauvaise saisi"

               END-IF

           END-PERFORM

           PERFORM 0500-INITIALIZE-WS-BOOL 
               THRU 0500-INITIALIZE-WS-BOOL-END
       .

       0400-NUM-2-END.
           EXIT 
       .

       0410-CALCUL-STRING-2.
           IF WS-CONTINUE = "Y"

                   IF WS-NUM-2 < 0
                      STRING
                        FUNCTION TRIM(WS-CALCUL) 
                               DELIMITED BY SIZE
                        "(-" DELIMITED BY SIZE
                        FUNCTION TRIM(WS-VISUAL-NUM-2) 
                               DELIMITED BY SIZE
                        ")" DELIMITED BY SIZE
                        INTO WS-CALCUL
                      END-STRING
                   ELSE 
                    STRING
                        FUNCTION TRIM(WS-CALCUL) 
                               DELIMITED BY SIZE
                        FUNCTION TRIM(WS-VISUAL-NUM-2) 
                               DELIMITED BY SIZE
                        INTO WS-CALCUL
                    END-STRING
                END-IF
            ELSE
                IF WS-NUM-2 < 0
                      STRING
                        FUNCTION TRIM(WS-CALCUL) 
                               DELIMITED BY SIZE
                        "(-" DELIMITED BY SIZE
                        FUNCTION TRIM(WS-VISUAL-NUM-2) 
                               DELIMITED BY SIZE
                        "))" DELIMITED BY SIZE
                        INTO WS-CALCUL
                      END-STRING
                ELSE 
                    STRING
                        FUNCTION TRIM(WS-CALCUL) 
                               DELIMITED BY SIZE
                        FUNCTION TRIM(WS-VISUAL-NUM-2) 
                               DELIMITED BY SIZE
                        ")" DELIMITED BY SIZE
                        INTO WS-CALCUL
                    END-STRING

                END-IF

           END-IF
       .

       0410-CALCUL-STRING-2-END.
           EXIT 
       .

       0500-INITIALIZE-WS-BOOL.
           MOVE 'N' TO WS-BOOL
       .

       0500-INITIALIZE-WS-BOOL-END.
           EXIT 
       .

       0600-CALCUL.

           EVALUATE WS-OPERATOR

               WHEN "+"
                  ADD WS-NUM-1 WS-NUM-2 GIVING WS-RESULT

               WHEN "-"
                  SUBTRACT WS-NUM-2 FROM WS-NUM-1 GIVING WS-RESULT

               WHEN "x"
                  MULTIPLY WS-NUM-1 BY WS-NUM-2 GIVING WS-RESULT

               WHEN "/"
                  DIVIDE WS-NUM-1 BY WS-NUM-2 GIVING WS-RESULT

               WHEN "^"
                  COMPUTE WS-RESULT = WS-NUM-1 ** WS-NUM-2

           END-EVALUATE
       .

       0600-CALCUL-END.
           EXIT 
       .

       0700-CONTINUE-CALCUL.

           MOVE WS-RESULT TO WS-VISUAL-RESULT
           DISPLAY WS-CALCUL
           IF WS-RESULT < 0
              DISPLAY "= -" FUNCTION TRIM(WS-VISUAL-RESULT)
           ELSE
              DISPLAY "= " FUNCTION TRIM(WS-VISUAL-RESULT)
           END-IF
           
           DISPLAY "Continuez le calcul ? (oui/non)"

           PERFORM UNTIL FUNCTION LOWER-CASE(WS-INPUT) = "oui" 
               OR FUNCTION LOWER-CASE(WS-INPUT) = "non"

               ACCEPT WS-INPUT

               EVALUATE FUNCTION LOWER-CASE(WS-INPUT)

                   WHEN "oui"
                      MOVE WS-RESULT TO WS-NUM-1
                      MOVE 'Y' TO WS-CONTINUE

                   WHEN "non"
                       
                       PERFORM 0710-RESULT-STRING 
                           THRU 0710-RESULT-STRING-END
                       
                       MOVE SPACES TO WS-CALCUL
                       MOVE 'N' TO WS-CONTINUE

                   WHEN OTHER

                      DISPLAY "oui ou non"

               END-EVALUATE  

           END-PERFORM
       .

       0700-CONTINUE-CALCUL-END.
           EXIT 
       .

       0710-RESULT-STRING.
           IF WS-RESULT < 0
               STRING
                    FUNCTION TRIM(WS-STORAGE) 
                        DELIMITED BY SIZE
                    " " DELIMITED BY SIZE
                    FUNCTION TRIM(WS-CALCUL)
                    "= -" DELIMITED BY SIZE
                    FUNCTION TRIM(WS-VISUAL-RESULT) 
                        DELIMITED BY SIZE
                    INTO WS-STORAGE
               END-STRING
           ELSE 
               STRING
                    FUNCTION TRIM(WS-STORAGE) 
                        DELIMITED BY SIZE
                    " " DELIMITED BY SIZE
                    FUNCTION TRIM(WS-CALCUL)
                    "= " DELIMITED BY SIZE
                    FUNCTION TRIM(WS-VISUAL-RESULT) 
                        DELIMITED BY SIZE
                    INTO WS-STORAGE
               END-STRING
           END-IF
       .

       0710-RESULT-STRING-END.
           EXIT 
       .

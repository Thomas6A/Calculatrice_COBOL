       IDENTIFICATION DIVISION.
       PROGRAM-ID. calcul.
       AUTHOR. Thomas Baudrin.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       
      *Variables correspondant aux nombres saisi par l'utilisateur
      *    Signé de taille 3 chiffres et 2 décimales
       01  WS-NUM-1            PIC S9(3)V99.
       01  WS-NUM-2            PIC S9(3)V99.
      *Variables permmettant d'afficher proprement les calculs 
       01  WS-VISUAL-NUM-1     PIC Z(2)9.99.
       01  WS-VISUAL-NUM-2     PIC Z(2)9.99.
      *Variable coorespondant à l'opérateur du calcul composé d'un 
      *    Alphanumérique
       01  WS-OPERATOR         PIC X.
      *Variable corespondant au résultat du calcul
      *    Signé de taille 9 chiffres et 2 décimales
       01  WS-RESULT           PIC S9(9)v99.
      *Variable permettant d'afficher proprement le résultat
       01  WS-VISUAL-RESULT    PIC Z(8)9.99.
      *Variables permettant de stocker l'historique des calculs pour
      *    WS-STORAGE et le calcul en cour pour WS-CALCUL
       01  WS-STORAGE          PIC X(255).
       01  WS-CALCUL           PIC X(100).
      *Variable correspondant à la saisi de l'utilisateur
       01  WS-INPUT            PIC X(5).
      *Variable booléenne utilisé pour confirmer certaine situation
       01  WS-BOOL             PIC X            VALUE 'N'.
       01  WS-CONTINUE         PIC X            VALUE 'N'.

       PROCEDURE DIVISION.

      *Boucle permettant d'afficher le menu en permanence jusqu'à ce
      *    que l'utilisateur ecris quit et avec la vérification de
      *    WS-INPUT tous les espaces devienne des 0 donc quit0
       PERFORM UNTIL FUNCTION LOWER-CASE(WS-INPUT) = "quit0"
            
      *Affichage du menu puis saisi de l'utilisateur
           DISPLAY "Entrez quit pour quitter" 
           DISPLAY "Entrez last pour voir les précédents calculs"
           DISPLAY "Entrez un nombre (3 chiffres max et 2 décimal)"
           ACCEPT WS-INPUT

      *Association de la saisi de l'utilisateur à WS-NUM-1
           MOVE WS-INPUT TO WS-NUM-1

      *Paragraphe permettant de modifier la saisi afin de 
      *    vérifier si l'utilisateur à bien saisi les données
           PERFORM 0100-INSPECT-INPUT THRU 0100-INSPECT-INPUT-END
           
      *Paragraphe permettant de vérifier la saisi utilisateur
           PERFORM 0200-VERIFY-NUM-1 THRU 0200-VERIFY-NUM-1-END
           
      *Boucle permettant de continuer le calcul avec le résultat
      *    s'arrétant si l'utilisateur quitte ou ne continue pas le
      *    calcul
           PERFORM UNTIL FUNCTION LOWER-CASE(WS-INPUT) = "quit0" 
               OR FUNCTION LOWER-CASE(WS-INPUT) = "non"

      *Paragraphe permettant la saisi de l'opérateur
               PERFORM 0300-OPERATOR THRU 0300-OPERATOR-END
      
      *Paragraphe permettant la saisi du second nombre
               PERFORM 0400-NUM-2 THRU 0400-NUM-2-END
      
      *Paragraphe permettant le calcul
               PERFORM 0600-CALCUL THRU 0600-CALCUL-END
      
      *Paragraphe permettant à l'utilisateur de continuer ou non le 
      *    calcul
               PERFORM 0700-CONTINUE-CALCUL 
                   THRU 0700-CONTINUE-CALCUL-END

           END-PERFORM

       END-PERFORM.    
       STOP RUN.

      ******************************************************************

      *Paragraphe permettant de modifier la saisi afin de 
      *    vérifier si l'utilisateur à bien saisi les données
       0100-INSPECT-INPUT.
      *INSPECT permettant de remplacer les espaces par des 0, le premier
      *    - et . par des 0 afin de vérifier si la saisi est numérique
           INSPECT WS-INPUT REPLACING ALL SPACES BY '0'
           INSPECT WS-INPUT REPLACING FIRST "-" BY '0'
           INSPECT WS-INPUT REPLACING FIRST "." BY '0'
       .

       0100-INSPECT-INPUT-END.
           EXIT
       .

      *Paragraphe permettant de vérifier la saisi utilisateur
       0200-VERIFY-NUM-1.
      
      *Condition vérifiant la saisi de l'utilisateur
      *Si quit on continue on vérifie avec lower case pour vérifier
      *    peu importe les majuscules 
           IF FUNCTION LOWER-CASE(WS-INPUT) = "quit0"

               CONTINUE

      *Si last on affiche l'historique
           ELSE IF FUNCTION LOWER-CASE(WS-INPUT) = "last0"

               DISPLAY WS-STORAGE
               MOVE "non" TO WS-INPUT

      *Si ce n'est pas numérique on affiche un message d'erreur et on
      *    empêche de rentrer dans la prochaine boucle
           ELSE IF WS-INPUT IS NOT NUMERIC

               DISPLAY "Saisissez un nombre"
               MOVE "non" TO WS-INPUT

      *Sinon on associe WS-NUM-1 à WS-VISUAL-NUM-1 puis on construit 
      *    la string calcul
           ELSE 

               MOVE WS-NUM-1 TO WS-VISUAL-NUM-1
      
      *Paragraphe permettant d'ajouter le premier nombre à la variable 
      *    calcul
               PERFORM 0210-CALCUL-STRING-1 
                   THRU 0210-CALCUL-STRING-1-END

           END-IF
       .

       0200-VERIFY-NUM-1-END.
           EXIT
       .

      *Paragraphe permettant d'ajouter le premier nombre à la variable 
      *    calcul
       0210-CALCUL-STRING-1.
      *Condition permettant la construction d'une string selon si c'est
      *    négatif ou positif
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

      *Paragraphe permettant la saisi de l'opérateur
       0300-OPERATOR.

      *Boucle permettant la saisi de l'opérateur jusqu'à ce que 
      *    l'utilisateur mette une valeur valide 
           PERFORM UNTIL WS-BOOL = 'Y'
               
      *Affiche le calcul jusque là puis affiche le menu puis la saisi         
               DISPLAY WS-CALCUL
               DISPLAY "Entrez l'opérateur ( +, -, x, /, ^ )"
               ACCEPT WS-INPUT

      *Si l'utilisateur rentre une valeur valide on associe son input 
      *    puis on fait en sorte de pouvoir sortir de la boucle     
               IF WS-INPUT = "+" OR WS-INPUT = "-" OR WS-INPUT = "x"
                   OR WS-INPUT = "/" OR WS-INPUT = "^"

                  MOVE WS-INPUT TO WS-OPERATOR
                  MOVE 'Y' TO WS-BOOL

      *Paragraphe permettant d'ajouter l'opérateur à la variable calcul
                  PERFORM 0310-OPERATOR-STRING 
                      THRU 0310-OPERATOR-STRING-END

               ELSE

                   DISPLAY "Mauvaise saisi"

               END-IF
               
           END-PERFORM

      *Paragraphe réinitialisant la variable WS-BOOL
           PERFORM 0500-INITIALIZE-WS-BOOL 
               THRU 0500-INITIALIZE-WS-BOOL-END
       .
    
       0300-OPERATOR-END.
           EXIT
       .

      *Paragraphe permettant d'ajouter l'opérateur à la variable calcul
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

      *Paragraphe permettant la saisi du second nombre
       0400-NUM-2.

      *Boucle permettant la saisi de l'opérateur jusqu'à ce que 
      *    l'utilisateur mette une valeur valide 
           PERFORM UNTIL WS-BOOL = 'Y'

      *Affiche le calcul jusque là puis affiche le menu puis la saisi 
               DISPLAY WS-CALCUL
               DISPLAY "Entrez un autre nombre (3 chiffres max)"
               ACCEPT WS-INPUT

               MOVE WS-INPUT TO WS-NUM-2

      *Paragraphe permettant de modifier la saisi afin de 
      *    vérifier si l'utilisateur à bien saisi les données
               PERFORM 0100-INSPECT-INPUT THRU 0100-INSPECT-INPUT-END

      *Condition vérifiant la saisi de l'utilisateur
               IF WS-OPERATOR = "/" AND WS-NUM-2 = 0

                   DISPLAY "Pas de division par 0"

               ELSE IF WS-INPUT IS NUMERIC

                   MOVE 'Y' TO WS-BOOL
                   MOVE WS-NUM-2 TO WS-VISUAL-NUM-2

      *Paragraphe permettant d'ajouter le second nombre à la variable 
      *    calcul
                   PERFORM 0410-CALCUL-STRING-2 
                       THRU 0410-CALCUL-STRING-2-END
                 
               ELSE

                   DISPLAY "Mauvaise saisi"

               END-IF

           END-PERFORM

      *Paragraphe réinitialisant la variable WS-BOOL
           PERFORM 0500-INITIALIZE-WS-BOOL 
               THRU 0500-INITIALIZE-WS-BOOL-END
       .

       0400-NUM-2-END.
           EXIT 
       .

      *Paragraphe permettant d'ajouter le second nombre à la variable 
      *    calcul
       0410-CALCUL-STRING-2.
      *Condition vérifiant si c'est le début du calcul ou non et si le 
      *    nombre est négatif
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

      *Paragraphe réinitialisant la variable WS-BOOL
       0500-INITIALIZE-WS-BOOL.
           MOVE 'N' TO WS-BOOL
       .

       0500-INITIALIZE-WS-BOOL-END.
           EXIT 
       .

      *Paragraphe permettant le calcul
       0600-CALCUL.

      *Condition permettant de faire le calcul selon l'opérateur
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

      *Paragraphe permettant à l'utilisateur de continuer ou non le 
      *    calcul
       0700-CONTINUE-CALCUL.

      *Affichage du calcul et du résultat selon si il est positif et
      *    négatif
           MOVE WS-RESULT TO WS-VISUAL-RESULT
           DISPLAY WS-CALCUL
           IF WS-RESULT < 0
              DISPLAY "= -" FUNCTION TRIM(WS-VISUAL-RESULT)
           ELSE
              DISPLAY "= " FUNCTION TRIM(WS-VISUAL-RESULT)
           END-IF
           
           DISPLAY "Continuez le calcul ? (oui/non)"

      *Boucle permettant l'affichage d'un menu tant que l'utilisateur
      *    ne saisi pas une valeur attendu
           PERFORM UNTIL FUNCTION LOWER-CASE(WS-INPUT) = "oui" 
               OR FUNCTION LOWER-CASE(WS-INPUT) = "non"

               ACCEPT WS-INPUT

               EVALUATE FUNCTION LOWER-CASE(WS-INPUT)

                   WHEN "oui"
                      MOVE WS-RESULT TO WS-NUM-1
                      MOVE 'Y' TO WS-CONTINUE

                   WHEN "non"
                       
      *Paragraphe permettant l'ajout du calcul et du résultat dans
      *    l'historique
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

      *Paragraphe permettant l'ajout du calcul et du résultat dans
      *    l'historique
       0710-RESULT-STRING.
      *Condition vérifiant si le résultat est positif et négatif
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

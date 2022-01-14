!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
! PROGAM FOR 21 CARD GAME IN FORTRAN
! Source code: JIAN ASIADO, WILBORN ROSEL 11/19/19, 2nd edit: 11/26/19, 3rd: 11/28/19

! PROGRAM aims to simulate a modified 21 card game in fortran
! As fulfillment of the partial requirements for APHY 10.1
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
PROGRAM MAIN
    INTEGER :: I, N, A(52)
    REAL :: USER_BAL, BET
    CHARACTER(LEN= 8):: USER, CHOICE
    USER_BAL = 2500
    PRINT*, "INITIALIZING 21 CARD GAME..."
    PRINT*, "SHUFFLING......"
    PRINT*, 
    PRINT*, 
    CALL PREGAME(USER_BAL, USER)
END PROGRAM MAIN

    
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
! PREGAME - PROVIDES DETAILS BEFORE ACTUAL GAME
!         - INCLUDES STARTING BALANCE OF PLAYER
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE BALANCE(USER, USER_BAL,BET)
    REAL :: USER_BAL, BET
    CHARACTER(LEN=8) :: USER
    PRINT*, "YOUR CURRENT BALANCE:", USER_BAL
END SUBROUTINE

SUBROUTINE PREGAME(USER_BAL, USER)
    IMPLICIT NONE 
    REAL :: USER_BAL, BET
    INTEGER :: I, K, A(52)
    CHARACTER(LEN=8) ::  CHOICE, Y, N
    CHARACTER(LEN=8) :: USER
    USER_BAL = 2500
    
    ! INPUT FOR UNIQUE USERNAME AND EXPERIENCE
    PRINT*, "INPUT 8-CHARACTER GAMER NAME: "
    READ*, USER 
    PRINT*,
    PRINT*, "WELCOME ", USER, " TO 21 CARD GAME!"
    CALL BALANCE(USER, USER_BAL,BET)
    PRINT*,
    
    ! GAME MECHANICS 
    PRINT*, "The aim of the game is to get as close to 21 as possible."
    PRINT*, "The dealer and all other players have two cards. "
    PRINT*, "With the exception of the dealer, all the players have their cards face up."
    PRINT*, "Player must decide if they want another card HIT or will stand on what they have."
    PRINT*, "You can have as many cards as you like as long as you DO NOT go over 21. "
    PRINT*, "You are given an initial balance of 2500 pesos."
    PRINT*, 
    PRINT*, "ARE YOU READY TO PLAY 21? (Y/N)" 
    CALL DECISION(CHOICE, USER, USER_BAL, BET)
END SUBROUTINE

!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
! PSUEDO - RANDOM NUMBER GENERATOR FOR UNBIASED CARD DISTRIBUTION IN GAME 
! Source code: 
! https://gcc.gnu.org/onlinedocs/gcc-4.6.4/gfortran/RANDOM_005fSEED.html
! https://gcc.gnu.org/onlinedocs/gcc-4.6.4/gfortran/RANDOM_005fNUMBER.html#RANDOM_005fNUMBER
! https://gcc.gnu.org/onlinedocs/gfortran/SYSTEM_005fCLOCK.html
! https://stackoverflow.com/questions/23057213/how-to-generate-integer-random-number-in-fortran-90-in-the-range-0-5
! Wilborn Rosel 11/12/19 
!
! Techinical Terms:

! SEED --- the number that controls the random num generator
!      --- determines if random numgen will output new number or repeat.
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 SUBROUTINE RANGEN(USER) ! Random number generator
  REAL :: R, J
  INTEGER :: I, A(52), N
  INTEGER :: count, count_rate, count_max
  CHARACTER(LEN=8) :: USER
  CALL SYSTEM_CLOCK(count, count_rate, count_max) 
  OPEN(UNIT = 17, FILE = "rangen.csv", STATUS = "old") ! SAVES RANDOM NUMBERS IN AN EXCEL FILE  
  CALL INIT_RANDOM_SEED() 
  DO I = 1, 52
    CALL random_number(r)
    A(I) = int(j)
    J = 1 + floor(52*r) 
    IF (a(i) /= 0 .and. a(i) /= a(i+1) .and. a(i) /= a(i-1) .and. a(i-1) /= a(i+1) .and. a(i-3) /= a(i+3)) then
        WRITE(17,*) A(i)
    ELSE
    END IF
  END DO
  CLOSE(17)
END SUBROUTINE 

SUBROUTINE INIT_RANDOM_SEED() ! Random seed initializer
    INTEGER :: I, N, CLOCK
    INTEGER, DIMENSION(:), ALLOCATABLE :: SEED
    CALL RANDOM_SEED(size = n) 
    ALLOCATE(seed(n))
    CALL SYSTEM_CLOCK(COUNT=clock) ! usess intrinsic clock function of fortran
    SEED = CLOCK + 37 * (/ (i - 1, i = 1, n) /) ! seed change algorithm
    CALL RANDOM_SEED(PUT = seed)
    DEALLOCATE(seed)
 END SUBROUTINE

!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINES FOR GAMEPLAY 
! 
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 SUBROUTINE DECISION(CHOICE, USER, USER_BAL,BET)
    CHARACTER(LEN=8) :: USER, CHOICE
    REAL :: USER_BAL, BET
    INTEGER :: A(52)
    1 READ*, CHOICE
    IF (CHOICE == "Y" .or. CHOICE == "y" .or. choice == "yes") THEN
        IF (USER_BAL > 2500 .OR. USER_BAL < 2500) THEN
            CALL GAME_SEQ(USER, A, BET, USER_BAL)
        ELSE 
            CALL GAME_SEQ(USER, A, BET, USER_BAL)
        END IF
    ELSE IF (CHOICE == "N" .OR. CHOICE == "NO" .OR. CHOICE == "no") then
        PRINT*,"THANKS FOR COMING. COME BACK SOON!"
    ELSE
        PRINT*, "INVALID RESPONSE" 
        GOTO 1
    END IF
END SUBROUTINE


 SUBROUTINE GAME_SEQ(USER, A, BET, USER_BAL) 
    IMPLICIT NONE
    REAL :: BET, USER_BAL
    INTEGER :: I, A(52),K, NUM
    CHARACTER(LEN=8):: USER, IOMSG, CHOICE
    PRINT*,
    CALL BALANCE(USER, USER_BAL,BET)
    PRINT*, "YOUR BETTING PHASE: "
    READ*, BET
    CALL RANGEN(USER) 
    CALL GETCARD(USER, A, USER_BAL, BET)
    PRINT*,
END SUBROUTINE

SUBROUTINE DECISION_HS(CHOICE, USER, USER_BAL,BET)
    CHARACTER(LEN=8) :: USER, CHOICE 
    REAL :: USER_BAL, BET
    PRINT*, "HIT OR STAND? [H/S]"
    1 READ*, CHOICE
    IF (CHOICE /= "H" .AND. CHOICE /= "S") THEN
        PRINT*, "INVALID RESPONSE" 
        GOTO 1
    END IF
END SUBROUTINE

SUBROUTINE GETCARD(USER, A, USER_BAL, BET) 
    INTEGER :: i , k, N = 52, A(52), CARDSUM, CARDSUMD
    REAL :: BET, USER_BAL
    CHARACTER(LEN=8) :: USER, CHOICE
    PRINT*,
    CALL CARDVAL(A)
    CALL CARDNAME(A)
    PRINT*, USER, "'S CARD1: ", A(1)
    PRINT*, "DEALER'S CARD: ", A(2)
    PRINT*,
    PRINT*, USER, "'S CARD2: ", A(3)
    PRINT*, "DEALER'S CARD2: ",  "  ? - (card faced down)" 

    CARDSUM  = A(1) + A(3)
    CARDSUMD = A(2) + A(5)
    PRINT*,
    PRINT*, USER,"s CARD SUM =", CARDSUM
        IF (CARDSUM == 21) THEN
            CALL WINLOSE(SCORE, A, CARDSUM, CARDSUMD, USER_BAL, BET)
            CALL TRY(USER, CHOICE, USER_BAL, BET)
        END IF
    !PRINT*, "DEALER'S CARDSUM =", CARDSUMD
    PRINT*,
    CALL DECISION_HS(CHOICE, USER, USER_BAL,BET)
        IF (CHOICE == "H") THEN
           CALL GETU1CARD(USER, A, CARDSUM, CARDSUMD, USER_BAL, BET)
        ELSE IF (CHOICE == "S") THEN
            CALL GETA1CARD(USER, A, CARDSUM, CARDSUMD, USER_BAL, BET)
        END IF
END SUBROUTINE

SUBROUTINE GETU1CARD(USER, A, CARDSUM, CARDSUMD, USER_BAL, BET)
    INTEGER :: I , K , N = 52, A(52), CARDSUM, CARDSUMD
    CHARACTER(LEN=8) :: USER, CHOICE
    CALL CARDVAL(A)
    CALL CARDNAME(A)
    PRINT*, USER, "'S CARD3:", A(4)
    CARDSUM = CARDSUM + A(4)
    PRINT*, USER, "'S CARDSUM:", CARDSUM
        IF (CARDSUM < 21) THEN
            CALL DECISION_HS(CHOICE, USER,USER_BAL,BET)
               IF (CHOICE == "H") THEN
                    CALL GETU2CARD(USER, A, CARDSUM, USER_BAL, BET)
                ELSE  IF (CHOICE == "S") THEN
                CALL GETA1CARD(USER, A, CARDSUM, CARDSUMD, USER_BAL, BET)
             END IF  
        ELSE IF (CARDSUM > 21) THEN
                PRINT*, 
                PRINT*, "DEALER'S CARD", A(5)
                PRINT*, "DEALER'S CARDSUM", CARDSUMD
                 CALL WINLOSE(SCORE, A, CARDSUM, CARDSUMD, USER_BAL, BET) 
        END IF
END SUBROUTINE

SUBROUTINE GETA1CARD(USER, A, CARDSUM, CARDSUMD, USER_BAL, BET)
    INTEGER :: I , K , N = 52, A(52), CARDSUM, CARDSUMD
    CHARACTER(LEN=8) :: USER, CHOICE
    CALL CARDVAL(A)
    CALL CARDNAME(A)
    PRINT*, "DEALER'S CARD2:", A(5)
    CARDSUMD = CARDSUMD
    PRINT*, "DEALER'S CARDSUM:", CARDSUMD
        IF ( CARDSUMD < 21) THEN
            CARDSUMD = CARDSUMD + A(6)
            PRINT*,
            PRINT*, "DEALER HITS!" 
            PRINT*, "DEALER'S CARD3:", A(6)
            PRINT*, "DEALER'S CARDSUM", CARDSUMD
            PRINT*, USER, "'S CARDSUM: ", CARDSUM
                IF ( CARDSUMD < 21) THEN
                    CALL GETA2CARD(USER, A, CARDSUM, CARDSUMD, USER_BAL, BET)
                END IF
            CALL WINLOSE(SCORE, A, CARDSUMD, CARDSUM, USER_BAL, BET)
        ELSE IF (CARDSUMD > 21) THEN 
            CALL WINLOSE(SCORE, A, CARDSUMD, CARDSUM, USER_BAL, BET)
        END IF
END SUBROUTINE

SUBROUTINE GETA2CARD(USER, A, CARDSUM, CARDSUMD, USER_BAL, BET)
    INTEGER :: I , K , N = 52, A(52), CARDSUM, CARDSUMD
    CHARACTER(LEN=8) :: USER, CHOICE
    CALL CARDVAL(A)
    CALL CARDNAME(A)
    PRINT*, "DEALER'S CARD4:", A(6)
    CARDSUMD = CARDSUMD
    PRINT*, "DEALER'S CARDSUM:", CARDSUMD
        IF (CARDSUMD < 21) THEN
            CARDSUMD = CARDSUMD + A(7)
            PRINT*,
            PRINT*, "DEALER'S CARD3:", A(7)
            PRINT*, "DEALER'S CARDSUM", CARDSUMD
            PRINT*, USER, "'S SUM: ", CARDSUM
            CALL WINLOSE(SCORE, A, CARDSUMD, CARDSUM, USER_BAL, BET)
        ELSE 
            CALL WINLOSE(SCORE, A, CARDSUMD, CARDSUM, USER_BAL, BET)
        END IF
END SUBROUTINE

SUBROUTINE GETU2CARD(USER, A, CARDSUM, USER_BAL, BET)
    INTEGER :: I , K , N = 52, A(52), CARDSUM, CARDSUMD
    CHARACTER(LEN=8) :: USER, CHOICE
    CALL CARDVAL(A)
    CALL CARDNAME(A)
    PRINT*,
    PRINT*, USER, "'S CARD:", A(8)
    CARDSUM = CARDSUM + A(8)
    PRINT*, USER, "'S CARDSUM:", CARDSUM
        IF (CARDSUM < 21) THEN
            CALL DECISION_HS(CHOICE, USER, USER_BAL, BET)
               IF (CHOICE == "H") THEN  
                    CALL WINLOSE(SCORE, A, CARDSUM, CARDSUMD, USER_BAL, BET) 

                ELSE  IF (CHOICE == "S") THEN
                    CALL GETA2CARD(USER, A, CARDSUM, CARDSUM, USER_BAL, BET)
                    CALL WINLOSE(SCORE, A, CARDSUM, CARDSUMD, USER_BAL, BET)
                END IF
        ELSE IF (CARDSUM > 21) THEN
            PRINT*, 
            PRINT*, "DEALER'S CARD", A(5)
            PRINT*, "DEALER'S CARDSUM", CARDSUM
             CALL WINLOSE(SCORE, A, CARDSUM, CARDSUMD, USER_BAL, BET) 
        END IF
END SUBROUTINE

SUBROUTINE WINLOSE(SCORE, A, CARDSUM, CARDSUMD, USER_BAL, BET)
    REAL :: SCORE, BET, USER_BAL
    INTEGER :: A(52), CARDSUM, CARDSUMD
    CHARACTER(LEN=8) :: USER, CHOICE
    IF (CARDSUM > 21 .AND. CARDSUMD <= 21) THEN
        PRINT*,
        PRINT*, "END OF ROUND. DEALER WINS!" 
        USER_BAL = USER_BAL - BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUM == 21 .AND. CARDSUMD == 21) THEN 
        PRINT*,
        PRINT*, "END OF ROUND. DRAW!"
        USER_BAL = USER_BAL
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF  (CARDSUM <= 21 .AND. CARDSUM > CARDSUMD .AND. CARDSUMD < 21) THEN
        PRINT*,
        PRINT*, "END OF ROUND. YOU HAVE WON!"
        USER_BAL = USER_BAL + BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUMD > 21 .AND. CARDSUM <= 21) THEN
        PRINT*,
        PRINT*, "END OF ROUND. YOU HAVE WON!"
        USER_BAL = USER_BAL + BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUM < 21 .AND. CARDSUM < CARDSUMD .AND. CARDSUMD <= 21) THEN 
        PRINT*,
        PRINT*, "END OF ROUND. DEALER WINS!" 
        USER_BAL = USER_BAL - BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUMD < CARDSUM .AND. CARDSUMD < 21 .AND. CARDSUM <= 21 .AND. CARDSUM /= CARDSUMD) THEN
        PRINT*,
        PRINT*, "END OF ROUND. DEALER WINS!" 
        USER_BAL = USER_BAL - BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUMD > CARDSUM .AND. CARDSUMD <= 21 .AND. CARDSUM < 21 .AND. CARDSUM /= CARDSUMD) THEN
        PRINT*,
        PRINT*, "END OF ROUND. DEALER WINS!" 
        USER_BAL = USER_BAL - BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUMD <= 21 .AND. CARDSUM > 21) THEN
        PRINT*,
        PRINT*, "END OF ROUND. DEALER WINS!" 
        USER_BAL = USER_BAL - BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUM <= 21 .AND. CARDSUMD > 21) THEN
        PRINT*,
        PRINT*, "END OF ROUND. YOU HAVE WON!"
        USER_BAL = USER_BAL + BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    ELSE IF (CARDSUM > 21 .AND. CARDSUM <=21 .AND. CARDSUM > CARDSUMD) THEN
        PRINT*,
        PRINT*, "END OF ROUND. YOU HAVE WON!"
        USER_BAL = USER_BAL + BET
        CALL BALANCE(USER, USER_BAL,BET)
        CALL TRY(USER, CHOICE, USER_BAL, BET)
    END IF
END SUBROUTINE

SUBROUTINE TRY(USER, CHOICE, USER_BAL, BET)
    REAL :: SCORE, BET, USER_BAL
    CHARACTER(LEN=8) :: USER, CHOICE
    INTEGER :: A(52)
    SCORE = USER_BAL
    PRINT*,
    PRINT*, "WOULD YOU LIKE TO PLAY AGAIN?"
    OPEN(UNIT = 16, FILE = "score.csv", STATUS = "OLD")
    WRITE(16,*) SCORE, USER
    CLOSE(16)
    CALL DECISION(USER,CHOICE, USER_BAL,BET)
END SUBROUTINE

!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINES FOR CARDS
!
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE CARDVAL(A)
    INTEGER :: k, N = 52, A(52), CARDSUM
    OPEN(unit = 17, file = "rangen.csv", status = "old") ! READS FROM EXCEL FILE
    DO I = 1, 20
    READ(17,*) A(i)
   ! TRANSLATION OF RANDOM NUMBER TO CARD VALUE
        IF (A(I) == 1 .OR. A(I) == 14 .OR. A(I) == 27 .OR. A(I) == 40) THEN
            A(I) = 1
        ELSE IF (A(I) == 2 .OR. A(I) == 15 .OR. A(I) == 28 .OR. A(I) == 41) THEN 
            A(I) = 2
        ELSE IF (A(I) == 3 .OR. A(I) == 16 .OR. A(I) == 29 .OR.  A(I) == 42) THEN 
            A(I) = 3
        ELSE IF (A(I) == 4 .OR. A(I) == 17 .OR.A(I) == 30 .OR. A(I) == 43) THEN 
            A(I) = 4
        ELSE IF (A(I) == 5 .OR. A(I) == 18 .OR. A(I) == 31 .OR. A(I) == 44) THEN 
            A(I) = 5
        ELSE IF (A(I) == 6 .OR. A(I) == 19 .OR. A(I) == 32 .OR. A(I) == 45) THEN 
            A(I) = 6   
        ELSE IF (A(I) == 7 .OR. A(I) == 20 .OR. A(I) == 33 .OR. A(I) == 46) THEN 
            A(I) = 7
        ELSE IF (A(I) == 8 .OR. A(I) == 21 .OR. A(I) == 34 .OR. A(I) == 47) THEN 
            A(I) = 8
        ELSE IF (A(I) == 9 .OR. A(I) == 22 .OR. A(I) == 35 .OR. A(I) == 48) THEN 
            A(I) = 9
        ELSE IF (A(I) == 10 .OR. A(I) == 23 .OR. A(I) == 36 .OR. A(I) == 49) THEN 
            A(I) = 10
        ELSE IF (A(I) == 11 .OR. A(I) == 24 .OR. A(I) == 37 .OR. A(I) == 50) THEN 
            A(I) = 10 
        ELSE IF (A(I) == 12 .OR. A(I) == 25 .OR. A(I) == 38 .OR. A(I) == 51) THEN 
            A(I) = 10 
        ELSE IF (A(I) == 13 .OR. A(I) == 26 .OR. A(I) == 39 .OR. A(I) == 52) THEN 
            A(I) = 11
        END IF
    END DO    
    CLOSE(17) 
END SUBROUTINE

SUBROUTINE CARDNAME(A)
    INTEGER :: k, N = 52, A(52), CARDSUM
    IF (A(I) == 1) THEN
        PRINT*, "ACE[1]"
    ELSE IF (A(I) == 2) THEN 
       PRINT*, "TWO [2]"
    ELSE IF (A(I) == 3) THEN 
        PRINT*, "THREE [3]"
    ELSE IF (A(I) == 4) THEN 
        PRINT*, "FOUR [4]"
    ELSE IF (A(I) == 5) THEN 
        PRINT*, "FIVE [5]"
    ELSE IF (A(I) == 6) THEN 
        PRINT*, "SIX [6]"
        A(I) = 6   
    ELSE IF (A(I) == 7) THEN 
        PRINT*, "SEVEN [7]"
    ELSE IF (A(I) == 8) THEN 
        PRINT*, "EIGHT [8]"
    ELSE IF (A(I) == 9) THEN 
        PRINT*, "NINE [9]"
    ELSE IF (A(I) == 10) THEN 
        PRINT*, "JACK [10]"
    ELSE IF (A(I) == 11) THEN 
        PRINT*, "QUEEN [10]"
    ELSE IF (A(I) == 12) THEN 
        PRINT*, "KING [10]" 
    ELSE IF (A(I) == 13) THEN 
        PRINT*, "ACE[11]"
    END IF
END SUBROUTINE

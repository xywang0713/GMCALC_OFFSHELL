C=======================================================================
C GMCALC: a calculator for the Georgi-Machacek model
C (with the most general custodial-symmetry-invariant scalar potential)
C http://people.physics.carleton.ca/~logan/gmcalc/
C========================================================================

      PROGRAM GMMG5
C Program to generate the MG5 param_card for the Georgi-Machacek model.
      IMPLICIT NONE
C Common blocks:
      DOUBLE PRECISION MU2SQ,MU3SQ,LAMBDA1,LAMBDA2,LAMBDA3,LAMBDA4,
     .     LAMBDA5,M1,M2
      COMMON/LPARAMS/MU2SQ,MU3SQ,LAMBDA1,LAMBDA2,LAMBDA3,LAMBDA4,
     .     LAMBDA5,M1,M2
      INTEGER UNIOK, BFBOK, POSMSQOK, MINOK, INPUTOK
      COMMON/FLAGS/UNIOK, BFBOK, POSMSQOK, MINOK, INPUTOK
      INTEGER INPUTMODE, SILENT
      COMMON/CONTROLS/INPUTMODE,SILENT
      DOUBLE PRECISION MH
      INTEGER INPUTSET
      COMMON/INPUT/MH,INPUTSET
      DOUBLE PRECISION IMHL, IMHH, IMH3, IMH5, ISH, ISA
      COMMON/INPUT3/IMHL,IMHH,IMH3,IMH5,ISH,ISA
      INTEGER OFFSHELL, QCDCORRS
      COMMON/DECAYFLAGS/OFFSHELL, QCDCORRS
      CALL PRINT_BANNER
C==================================================================
C This initialization call must always come before anything else!
      CALL INITIALIZE_SM
C==================================================================

C================== user-defined options ==========================
C INPUTMODE = 0: take inputs from this program - enter values below
C INPUTMODE = 1: read inputs from the command line in interactive mode
      INPUTMODE = 0
C==================================================================
C Options for input sets:
C INPUTSET = 1: mu3^2, lambda1, lambda2, lambda3, lambda4, lambda5, M1, M2
C INPUTSET = 2: mu3^2, mh, lambda2, lambda3, lambda4, lambda5, M1, M2
C INPUTSET = 3: mh, mH, m3, m5, sin(thetaH), sin(alpha), M1, M2
C INPUTSET = 4: mh, m5, sin(thetaH), lambda2, lambda3, lambda4, M1, M2
C INPUTSET = 5: mh, mH, sin(thetaH), sin(alpha), lambda2, lambda3, lambda4, lambda5
C INPUTSET = 6: mh, m5, sin(thetaH), lambda2, lambda3, lambda4, lambda5, M2
      INPUTSET = 6
C==================================================================
C SILENT = 0: echo the inputs to the screen
C SILENT = 1: don't echo the inputs to the screen
      SILENT = 0
      OFFSHELL = 1
      QCDCORRS = 1
C==================================================================
C Modify these entries to set the input parameters (used if INPUTMODE = 0).
C Be sure to modify the correct block for your INPUTSET, as chosen above!
      IF (INPUTMODE.EQ.0) THEN
         IF (INPUTSET.EQ.6) THEN
            IMHL = 125.D0
              IMH5 = 70.000762939453125D0
              ISH = 0.0000012003D0
              LAMBDA2 = (IMH5/100)*0.08D0
              LAMBDA3 = -1.5D0
              LAMBDA4 = 1.5D0
              LAMBDA5 = -4*LAMBDA2
              M2 = 10.D0
         ELSE
            PRINT *, "INPUTSET = ", INPUTSET, "is not a valid option."
            PRINT *, "No param_card.dat written."
            GOTO 10
         ENDIF
      ENDIF

C==================================================================
      CALL LTSTARTER
      CALL LOAD_INPUTS
      PRINT *
      IF (INPUTOK.EQ.1) THEN
         CALL THYCHECK
         IF (UNIOK*BFBOK*MINOK.EQ.1) THEN
            CALL CALCPHYS
            PRINT *, "Theory checks passed; writing param_card files."
C In order to get the EFT parameters and the decay tables, need to call:
            CALL HLCOUPS
            CALL HHCOUPS
            CALL CALCDECAYS
            CALL WRITE_PARAM_CARD_LO
            CALL WRITE_PARAM_CARD_NLO
            CALL WRITE_PARAM_CARD_EFT_LO
            PRINT *
            PRINT *, "Three files written: "
            PRINT *, " * param_card-LO.dat, for use with MadGraph5"
            PRINT *, " * param_card-NLO.dat, for use with ",
     .           "MadGraph5_aMC@NLO"
            PRINT *, " * param_card-EFTLO.dat, ",
     .           "for use with the GM model with EFT couplings ",
     .           "in MadGraph5"
            PRINT *, "Rename the desired file to param_card.dat"
            PRINT *, "and move it to your MadGraph Cards directory."
            PRINT *, "Get the Georgi-Machacek model UFO files at"
            PRINT *, "http://feynrules.irmp.ucl.ac.be/wiki/",
     .           "GeorgiMachacekModel"
            PRINT *
         ELSE
            PRINT *, "Theory constraint(s) failed!  ",
     .           "No param_card.dat written."
         ENDIF
      ELSE
         PRINT *, "Bad input set!  No param_card.dat written."
      ENDIF
      CALL LTENDER

 10   STOP
      END

;; 1. Based on:     000
;; 2. Description:       
;;    NONMEM PK example for xpose
$PROBLEM    Parameter estimation
$INPUT      ID DOSE DV SCR AGE SEX CLASS WT ACE MED1 MED2 TAD TIME
            CLCR AMT SS II EVID
$DATA      mx19_2.csv IGNORE=@
$ABBREVIATED DERIV2=NO
$SUBROUTINE ADVAN2 TRANS1
$PK
 TVCL    = THETA(1)*(1+THETA(7)*(CLCR-65))
 TVV     = THETA(2)*WT
 CL      = TVCL*EXP(ETA(1))
 V       = TVV*EXP(ETA(2))  
 KA      = THETA(3)*EXP(ETA(3))
 ALAG1   = THETA(4)
 K       = CL/V

$ERROR
 A1      = A(1)
 A2      = A(2)
 IPRED   = A2/V
 IRES    = IPRED-DV
 W       = THETA(5)*IPRED + THETA(6)
 IWRES   = IRES/W
 Y       = IPRED + W*EPS(1)

$THETA  (0,25.7435)           ; TVCL
$THETA  (0,1.36688)           ; TVV
$THETA  (0,7.25257)           ; TVKA
$THETA  (0,0.215994)          ; LAG
$THETA  (0,0.215055)          ; Prop. Err
$THETA  (0,0.0096742)         ; Add. Err
$THETA  (0,0.00637473,.02941) ; CRCL on CL
$OMEGA  0.0728446             ; IIV CL
$OMEGA  0.0419272             ; IIV V
$OMEGA  2.33689               ; IIV KA
$SIGMA  1  FIX
$ESTIMATION METHOD=1 INTER MAXEVALS=9999 PRINT=1 NOABORT MSFO=msf001
$COVARIANCE PRINT=E
$TABLE      ID DOSE AMT SS II TIME TAD IPRED CWRES CPRED IWRES EVID A1
            A2 ONEHEADER NOPRINT FILE=sdtab001                ; Simple table
$TABLE      ID SEX MED1 MED2 ONEHEADER NOPRINT NOAPPEND
            FORMAT=,1PE11.4 FILE=catab001.csv                 ; Table in csv format
$TABLE      ID CLCR AGE WT ONEHEADER FIRSTONLY NOPRINT NOAPPEND
            FILE=cotab001                                     ; Table with firstonly option
$TABLE      ID KA CL V ALAG1 ETAS(1:LAST) FIRSTONLY ONEHEADER NOPRINT
            NOAPPEND FILE=patab001                            ; Table with firstonly option

; Problem no. 2
$PROBLEM    Model simulations
$INPUT      ID DOSE DV SCR AGE SEX CLASS WT ACE MED1 MED2 TAD TIME
            CLCR AMT SS II EVID
$DATA      mx19_2.csv IGNORE=@ REWIND
$MSFI      msf001
$SIMULATION (221287) ONLYSIM NSUB=20                          ; Small sim for illustration only
$TABLE      ID DV IPRED DOSE AMT TIME TAD EVID SEX CLCR AGE WT
            ONEHEADER NOPRINT NOAPPEND FILE=simtab001         ; Simulation table


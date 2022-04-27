      SUBROUTINE SDVINI(STATEV,COORDS,NSTATV,NCRDS,NOEL,NPT,
     1 LAYER,KSPT)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION STATEV(NSTATV),COORDS(NCRDS)

      DOUBLE PRECISION DEPTH,RPHI,G(10),RTHETA
      REAL RDEG
      INTEGER i, j
      CHARACTER FilLoc*31, sdvNum*5
C
      PARAMETER (ZERO=0.D0,ONE=1.D0,TWO=2.D0,TEN=10.D0,FOUR=4.D0,
     1 CONS1=5.235987755983D0,PI=3.14159265359D0,FFD=0.57735026919D0)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     THIS SUBROUTINE SETS THE INITIAL VALUES OF THE NON-HOMOGENEOUS MATERIAL PARAMETERS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     Note: this is a more generic subroutine, and I only activated the lines related to this study.
C
C     STATEV ARGUMENTS:
C     1       --->  CARTILAGE DEGENERATION TYPE
C     2       --->  FIBER CONSTANT
C     3       --->  INITIAL SOLID MATERIAL CONSTANT
C     4       --->  GAG CONSTANT
C     5       --->  NORMALIZED DEPTH
C     6       --->  GAG PRESSURE
C     7 - 12  --->  ---
C     13      --->  NEW SOLID MATERIAL CONSTANT
C     14      --->  NEW FIBER CONSTANT
C     15      --->  DET(F)
C     16 - 42 --->  UPDATED FIBRILLAR DIRECTIONAL UNIT VECTORS WITHOUT PRESTRESS EFFECTS
C     43 - 69 --->  INITIAL FIBRILLAR DIRECTIONAL UNIT VECTORS WITH PRESTRESS EFFECTS
C     70 - 75 --->  FIBRILLAR STRESS COMPONENTS
C     76 - 81 --->  NON-FIBRILLAR STRESS COMPONENTS
C     82 - 90 --->  INITIAL DFGRD1
C
C     FOR STATEV(1):
C     STATEV(1)=1 ==>> HEALTHY CARTILAGE
C     STATEV(1)=2 ==>> DEGENERATED CARTILAGE WITHOUT FIBRILARATION
C     STATEV(1)=3 ==>> DEGENERATED CARTILAGE WITH FIBRILARATION
C
C       
      FilLoc='C:\temp\HybridML\DATA.txt'
C
      DO i = 2, NSTATV ! INITIALIZTION OF STATE VARIBLES
       STATEV(i)=ZERO
      ENDDO
C
      open(UNIT=28,FILE=FilLoc,STATUS='OLD')
	  read(28,*) i
	  IF (i.NE.0) THEN
	   i = 0
	   DO WHILE (i.EQ.0)
        read(28,*,end=10) elLabel,intPoint,STATEV(4),STATEV(5),
     1   STATEV(2),STATEV(3)
C     2   ,STATEV(16),STATEV(17),STATEV(18),STATEV(19),STATEV(20)
C     3   ,STATEV(21),STATEV(22),STATEV(23),STATEV(24),STATEV(25)
C     4   ,STATEV(26),STATEV(27),STATEV(28),STATEV(29),STATEV(30)
C     5   ,STATEV(31),STATEV(32),STATEV(33),STATEV(34),STATEV(35)
C     6   ,STATEV(36),STATEV(37),STATEV(38),STATEV(39),STATEV(40)
C     7   ,STATEV(41),STATEV(42)
        IF (elLabel.EQ.NOEL .AND. intPoint.EQ.NPT) THEN
          GO TO 10
        ENDIF
       ENDDO
      ELSE
        read(28,*,end=10) DEPTH, STATEV(1)
        DEPTH=DBLE(COORDS(2)/DEPTH) !  DEPTH NOMALIZATION
        STATEV(5)=DEPTH !  NOMALIZED DEPTH
        STATEV(2)=DBLE(1.4*(DEPTH**TWO)-1.1*DEPTH+0.59) ! FIBER CONSTANT
        IF (STATEV(1).EQ.1) THEN ! SOLID MATERIAL CONSTANT
         STATEV(3)=DBLE(0.1+0.2*DEPTH)
        ELSE
         STATEV(3)=DBLE(0.05+0.2*DEPTH)
        ENDIF
C
        G(1)=0.005D0
        G(2)=0.01D0
        G(3)=0.025D0
        G(4)=0.035D0
        G(5)=0.042D0
        G(6)=0.048D0
        G(7)=0.053D0
        G(8)=0.058D0
        G(9)=0.06D0
        G(10)=0.06D0
        STATEV(4)=G(INT(DEPTH*9)+1) ! GAG CONSTANT
C       
        IF (DEPTH.GT.0.3) THEN
         RPHI = PI/2
         RTHETA = ZERO
        ELSEIF (DEPTH.LE.0.3) THEN
         RPHI=DBLE(CONS1*DEPTH)
         IF (STATEV(1).EQ.3) THEN
          CALL RANDOM_NUMBER (RDEG)
          RPHI=RDEG*PI/TWO
         ENDIF
        ENDIF
C       
        RTHETA = PI/TWO-RPHI ! JUST FOR 2D
C       
        STATEV(16)=COS(RPHI)      ! PRIMARY NVEC UNIT VECTOR DEGREES (RPHI, RTHETA, ...)
        STATEV(17)=COS(RTHETA)   
        STATEV(18)=ZERO
	    
        STATEV(19)=COS(PI-RPHI)     ! OTHER PRIMARY NVEC UNIT VECTOR
        STATEV(20)=COS(RTHETA)
        STATEV(21)=ZERO
	    
        STATEV(22)=ONE ! (0,PI/2,PI/2)
        STATEV(23)=ZERO
        STATEV(24)=ZERO
	    
        STATEV(25)=ZERO  ! (PI/2,0,PI/2)
        STATEV(26)=ONE
        STATEV(27)=ZERO
	    
        STATEV(28)=ZERO  ! (PI/2,PI/2,0)
        STATEV(29)=ZERO
        STATEV(30)=ONE
	    
        STATEV(31)=FFD  ! (PI/4,PI/4,PI/4)
        STATEV(32)=FFD  
        STATEV(33)=FFD  
	    
        STATEV(34)=-FFD     ! (-PI/4,PI/4,PI/4)
        STATEV(35)=FFD  
        STATEV(36)=FFD  
	    
        STATEV(37)=FFD  ! (PI/4,-PI/4,PI/4)
        STATEV(38)=-FFD     
        STATEV(39)=FFD  
	    
        STATEV(40)=FFD  ! (PI/4,PI/4,-PI/4)
        STATEV(41)=FFD
        STATEV(42)=-FFD
C
C
C
C     ADDED FOR THIS PARTICULAR STUDY
C
C
C
       STATEV(5) = -1.0D0   ! DEPTH
       STATEV(4) = 0.01D0   ! GAG CONSTANT
       STATEV(2) = 0.39D0 ! TOTAL FIBRILLAR DENSITY
       STATEV(3) = 0.15D0 ! SOLID VOLUME FRACTION CONSTANT
C
C
C
C
C
C
      ENDIF
10    close(UNIT=28)
C
      STATEV(6)=STATEV(4)
      STATEV(1)=ONE
      STATEV(15)=ONE
      DO i = 16,42
       STATEV(27+i) = STATEV(i) ! FIBRILLAR DIRECTIONS
      ENDDO
      STATEV(82) = ONE ! DFGRDC
      STATEV(85) = ONE
      STATEV(88) = ONE
C
C
C
      RETURN
      END
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C
C
      SUBROUTINE FLOW(H,SINK,U,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
      DIMENSION TIME(2), COORDS(3)
      CHARACTER*80 SNAME
      H=1
      SINK=0
      IF (COORDS(1).LE.12.5) THEN
       H=0
      ENDIF
      RETURN
      END
C
C
C
C
C
      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,JSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3),
     4 JSTEP(4)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     LOCAL PARAMETERS AND VARIABLES
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      DOUBLE PRECISION TRANF(3,3),DET,IDENT(3,3),INDEX(2,6),ALPHA1,
     1 ALPHA2,IGAGD,NVEC0(27),RH,W1,W2,W3,W4,W5,W6,STRG(NTENS,NTENS),
     2 EPS,FV1(3),HH,NEWV1(3),NSTR(NTENS),STR,LANDA,DELTAV(NTENS),
     3 BVEC(NTENS),C,NS0,E1MP,E2MP,K1MP,CSTR,DFGRD(3,3),EP,STRS(NTENS),
     4 STATE(NSTATV),GAG,VV(NTENS),DETC,EPSC

      INTEGER i,j,k,l,r,m,n,K1,K2,K3,K4,K5,K6,KKK,FF
      PARAMETER (ZERO=0.D0,ONE=1.D0,TWO=2.D0,THREE=3.D0,FOUR=4.D0,
     1 SEVEN=7.D0,SIX=6.D0,HALF=5.D-1,TEN=10D0)
C
C
C     Note: this is a more generic subroutine, and I only activated the lines related to this study.
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     INITIALIZATION
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      DO i = 1, 3
       DELTAV(i)=ONE ! KRONKER-DELTA IN VOIGT-NOTATION
       DO j = 1, 3
        IDENT(j,i) = ZERO ! KRONKER-DELTA 2ND ORDER TENSOR
        TRANF(j,i) = ZERO ! TRANSOPSE OF DEFORMATION GRADIENT TENSOR (DFGRD1) IN THE END OF THE INCREMNT
       ENDDO
       IDENT(i,i) = ONE
      ENDDO
      DO i = 1,NTENS
       STRESS(i)=ZERO ! CAUCHY STRESS TENSOR THAT SHOULD BE UPDATED
       STRS(i)=ZERO ! FIBRILLAR STRESS TENSOR
       DO j = 1,NTENS
        DDSDDE(j,i)=ZERO ! JACOBIAN MATRIX TENSOR THAT SHOULD BE UPDATED.
       ENDDO
      ENDDO
C
C     ALPHA1=STATEV(4) ! DEPTH-DEPENDENT GAG CONSTANT (DEACTIVATED FOR THIS PARTICULAR STUDY)
      ALPHA1=PROPS(3) ! DEPTH-DEPENDENT GAG CONSTANT (ONLY FOR THIS PARTICULAR STUDY)
C
      IF (JSTEP(1).EQ.1) THEN
        ALPHA2=ZERO ! GAG CONSTANT
        DETC=ONE
      ELSE
        ALPHA2=3.22D0
      ENDIF
      C=3.009D0 ! FIBRILLAR RELATIVE DENSITY CONSTANT
      RH=STATEV(2) ! TOTAL FIBRILLAR DENSITY
      NS0=STATEV(3) ! DEPTH-DEPENDENT SOLID VOLUME FRACTION CONSTANT
      DO i=4,NTENS ! NTENS CONTROLS THE DIMENTIONALITY OF THE CODE
       DELTAV(i)=ZERO
      ENDDO
      INDEX(1,1)=1 ! INDEX ARRAY ARE USED TO CIRCOMVENT ASSIGNING EQUAL COMPONENTS DUE TO SYMMYTRY OF HIYER ORDER TENSORS
      INDEX(2,1)=1
      INDEX(1,2)=2
      INDEX(2,2)=2
      INDEX(1,3)=3
      INDEX(2,3)=3
      INDEX(1,4)=1
      INDEX(2,4)=2
      INDEX(1,5)=1
      INDEX(2,5)=3
      INDEX(1,6)=2
      INDEX(2,6)=3
C
      DO i = 1,27
       NVEC0(i) = STATEV(42+i) ! INITIAL FIBRILLAR DIRECTIONS
      ENDDO
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     DDSDDE DERIVATION VIA PERTURBATION METHOD (INITIALIZATION)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      KKK=1
      EP=10D-8
      DO i = 1,3
       DO j = 1,3
        DFGRD(j,i)=DFGRD1(j,i)
       ENDDO
      ENDDO
      DO K6=1,NSTATV
       STATE(K6)=STATEV(K6)
      ENDDO
      i=1
      j=1
80    CONTINUE
      DO K1=1,3
       DO K2=1,3
        DFGRD1(K2,K1)=DFGRD(K2,K1)+(IDENT(K2,i)*DFGRD(j,K1)+IDENT(K2,j)
     1   *DFGRD(i,K1))*EP/TWO
       ENDDO
      ENDDO
90    CONTINUE
      DO K6=1,NTENS
       STRESS(K6)=ZERO
      ENDDO
      DO K6=1,NSTATV
       STATEV(K6)=STATE(K6)
      ENDDO
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     STRESS CALCULATIONS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      E1MP=PROPS(1) ! LINEAR MATERIAL CONSTANT OF FIBRILLAR PART
      E2MP=PROPS(2) ! NONLINEAR MATERIAL CONSTANT OF FIBRILLAR PART
      CALL TRANSPOSE(DFGRD1,TRANF)
      CALL VMATMUL(DFGRD1,TRANF,NTENS,BVEC) ! BVEC IS THE LEFT CAUCHY-GREEN OR FINGER DEFORMATION TENSOR
      CALL DETERMINANT(DFGRD1,DET) ! CURRENT VOLUME CHANGE FROM THE STRESS-FREE STATE
      IF (JSTEP(1).EQ.1) THEN
        STATEV(15)=DET ! DET(F) WITH RESPE
      ELSE
        DETC=DET/STATEV(15) ! CURRENT VOLUME CHANGE FROM THE PRESTRESSED STATE
      ENDIF
      HH=NS0*(RH*C)/(TWO*C+SEVEN) ! CNTRIBUTION OF OTHER CONSTANTS ON FIBRILAR STRESS
      IF (STATEV(1).NE.1) THEN
       E2MP=3670/FOUR   ! FOR OA
      ENDIF
      E1MP=E1MP*HH
      E2MP=E2MP*HH
C       write(6,*) E1MP, E2MP, JSTEP(1)
      DO i=0,8
       IF (i.EQ.2) THEN ! SECONDARY FIBRIL HAVE LOWER DENSITY BY C CONSTANT.
        E1MP=E1MP/C
        E2MP=E2MP/C
       ENDIF
       DO m = 1,3
        FV1(m)=ZERO ! FV VETOR IS THE INNER PRODUCT OF DFGRD1 AND NVEC0
        DO n = 1,3
         FV1(m)=DFGRD1(m,n)*NVEC0(3*i+n)+FV1(m)
        ENDDO
       ENDDO
       LANDA=SQRT(FV1(1)**TWO+FV1(2)**TWO+FV1(3)**TWO) ! LANDA IS THE ELONGATION
       EPS=LOG(LANDA) ! EPS IS THE FIBRIL LOGARITMIC STRIN
       DO n=1,3
        NEWV1(n)=FV1(n)/LANDA ! NEWV1 IS THE CURRENT FIBRIL DIRECTION
        STATEV(n+3*i+15)=NEWV1(n) ! DEGREES OF NEW DIRECTIONS
       ENDDO
       IF (EPS.GT.ZERO) THEN
        STR=(E1MP+E2MP*EPS)*EPS*LANDA/DET ! STR IS THE LOCAL FIBRIL STRESS
        DO K6=1,NTENS
         K3=INDEX(1,K6)
         K4=INDEX(2,K6)
         VV(K6)=NEWV1(K3)*NEWV1(K4) ! VV IS THE DYADIC PRODUCT OF CURRENT DIRECTION VECTORS THAT IS THE STRUCTRAL VECTOR
         STRS(K6)=STR*VV(K6) ! STRS IS THE GLOBAL FIBRIL STRESS
         STRESS(K6)=STRESS(K6)+STRS(K6)
        ENDDO
       ENDIF
      ENDDO
      DO K1=1,NTENS
        STATEV(69+K1)=STRESS(K1) ! STRESS IN THE FIBRILLAR PART
      ENDDO
C
      GM=0.723D0 ! PG NEO-HOOKEAN CONSTANT
      GM=GM*NS0*(ONE-RH) ! CONTRIBUTION OF OTHER CONSTANTS
      W5=GM/DET
      W6=((LOG(DET)/SIX)*(((THREE*NS0/(DET-NS0))
     1 *((DET*LOG(DET)/(DET-NS0))-TWO))-FOUR)+(DET**(TWO/THREE)))*W5
      DO K6=1,NTENS
       NSTR(K6)=-DELTAV(K6)*W6+BVEC(K6)*W5
      ENDDO
      DO K1=1,NTENS
        STATEV(75+K1)=NSTR(K1) ! STRESS IN THE FIBRILLAR PART
      ENDDO
C       GAG=ALPHA1*(DETC**(-ALPHA2))-ALPHA1
      GAG=ALPHA1*(DETC**(-ALPHA2))
      STATEV(6)=GAG ! S22 STRESS OF GAG PART
      DO K6=1,3
       STRESS(K6)=NSTR(K6)-GAG+STRESS(K6)
      ENDDO
      DO K6=4,NTENS
       STRESS(K6)=NSTR(K6)+STRESS(K6)
      ENDDO
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     DDSDDE DERIVATION VIA PERTURBATION METHOD (THE SECOND PART)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (KKK.LT.NTENS) THEN
       DO K5=1,NTENS
        DDSDDE(K5,KKK)=STRESS(K5)*DET
       ENDDO
       KKK=KKK+1
       i=INDEX(1,KKK)
       j=INDEX(2,KKK)
       GO TO 80
      ENDIF
      IF (KKK.EQ.NTENS) THEN
       DO K5=1,NTENS
        DDSDDE(K5,KKK)=STRESS(K5)*DET
       ENDDO
       DO K1=1,3
        DO K2=1,3
         DFGRD1(K2,K1)=DFGRD(K2,K1)
        ENDDO
       ENDDO
       KKK=KKK+1
       GO TO 90
      ENDIF
      W2=ONE/EP
      W1=W2/DET
      DO K6=1,NTENS
       DO K5=1,NTENS
        DDSDDE(K5,K6)=W1*DDSDDE(K5,K6)-W2*STRESS(K5)
       ENDDO
      ENDDO
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     OTHER STATEV UPDATES
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      STATEV(13)=NS0/DET ! NEW SOLID MATERIAL CONSTANT
      STATEV(14)=RH/DET ! NEW SOLID MATERIAL CONSTANT
C
      RETURN
      END
C
C     TRANSPOSE(A)
C
      SUBROUTINE TRANSPOSE(A,AT)
      INTEGER i , j
      DOUBLE PRECISION A(3,3), AT(3,3)
      Do i = 1 , 3
       DO j = 1 , 3
        AT(j,i) = A(i,j)
       ENDDO
      ENDDO
      RETURN
      END
C
C     DETERMINANT(A)
C
      SUBROUTINE DETERMINANT(A,DET)
      DOUBLE PRECISION A(3,3), DET
      DET=A(1,1)*A(2,2)*A(3,3)-A(1,1)*A(2,3)*A(3,2)
     1 -A(2,1)*A(1,2)*A(3,3)+A(2,1)*A(1,3)*A(3,2)
     2 +A(3,1)*A(1,2)*A(2,3)-A(3,1)*A(1,3)*A(2,2)
      RETURN
      END
C
C     INNER PRODUCT OF TWO MATRICES IN VOIGT-NOTATION
C
      SUBROUTINE VMATMUL(A,B,N,C)
      DOUBLE PRECISION A(3,3), B(3,3), C(N)
      INTEGER i , j
      C(1)=A(1,1)*B(1,1)+A(1,2)*B(2,1)+A(1,3)*B(3,1)
      C(2)=A(2,1)*B(1,2)+A(2,2)*B(2,2)+A(2,3)*B(3,2)
      C(3)=A(3,1)*B(1,3)+A(3,2)*B(2,3)+A(3,3)*B(3,3)
      C(4)=A(1,1)*B(1,2)+A(1,2)*B(2,2)+A(1,3)*B(3,2)
      IF (N.EQ.6) THEN
        C(5)=A(1,1)*B(1,3)+A(1,2)*B(2,3)+A(1,3)*B(3,3)
        C(6)=A(2,1)*B(1,3)+A(2,2)*B(2,3)+A(2,3)*B(3,3)
      ENDIF
      RETURN
      END
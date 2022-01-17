C
C
C
C     THIS CODE IS DEVELOPED BY SEYED SHAYAN SAJJADINIA, A SIMPLIED 
C     VERSION OF HIS PREVIOUS WORK AT https://github.com/shayansss/msc.
C
C
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     SDVINI
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
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
C     16 - 42 --->  INITIAL FIBRILLAR DIRECTIONAL UNIT VECTORS WITHOUT PRESTRESS EFFECTS
C     43 - 69 --->  UPDATED FIBRILLAR DIRECTIONAL UNIT VECTORS WITH PRESTRESS EFFECTS
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
C       FilLoc='C:\temp\OsmoticControl\DATA.txt' !  ONLY FOR PRE-STRESSING
C
      DO i = 2, NSTATV ! INITIALIZTION OF STATE VARIBLES
       STATEV(i)=ZERO
      ENDDO
      DEPTH=-1D0
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
      RETURN
      END
C
C
C
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     UMAT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      DOUBLE PRECISION TRANF(3,3),DET,IDENT(3,3),ALPHA1,ALPHA2,EPS,NS0,
     1 IGAGD,NVEC0(27),RH,W1,W2,W3,W4,W5,W6,STRG(3,3),FV1(3),
     2 HH,NEWV1(3),NSTR(NTENS),STR,LANDA,DELTAV(NTENS),BVEC(NTENS),C,
     3 E1MP,E2MP,K1MP,CSTR,DFGRD(3,3),EP,STRS(NTENS),STATE(NSTATV),GAG,
     4 VV(NTENS),DETC

      INTEGER i,j,k,l,r,m,n,K1,K2,K3,K4,K5,K6,KKK,FF
      PARAMETER (ZERO=0.D0,ONE=1.D0,TWO=2.D0,THREE=3.D0,FOUR=4.D0,
     1 SEVEN=7.D0,SIX=6.D0,HALF=5.D-1,TEN=10D0)
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
      ENDDO
C
      ALPHA1=STATEV(4) ! DEPTH-DEPENDENT GAG CONSTANT
      ALPHA1=0.01D0
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
C
      DO i = 1,27
       NVEC0(i) = STATEV(42+i) ! INITIAL FIBRILLAR DIRECTIONS
      ENDDO
      E1MP=PROPS(1) ! LINEAR MATERIAL CONSTANT OF FIBRILLAR PART
      E1MP=ZERO ! LINEAR MATERIAL CONSTANT OF FIBRILLAR PART
      E2MP=PROPS(2) ! NONLINEAR MATERIAL CONSTANT OF FIBRILLAR PART
      E2MP=ZERO ! NONLINEAR MATERIAL CONSTANT OF FIBRILLAR PART
      CALL TRANSPOSE(DFGRD1,TRANF)
      CALL VMATMUL(DFGRD1,TRANF,NTENS,BVEC) ! BVEC IS THE LEFT CAUCHY-GREEN OR FINGER DEFORMATION TENSOR
      CALL DETERMINANT(DFGRD1,DET) ! CURRENT VOLUME CHANGE FROM THE STRESS-FREE STATE
      DETC=DET
      GM=0.723D0 ! PG NEO-HOOKEAN CONSTANT
      GM=GM*NS0*(ONE-RH) ! CONTRIBUTION OF OTHER CONSTANTS
      W5=GM/DET
      W6=((LOG(DET)/SIX)*(((THREE*NS0/(DET-NS0))
     1 *((DET*LOG(DET)/(DET-NS0))-TWO))-FOUR)+(DET**(TWO/THREE)))*W5
      W5=ZERO
      W6=ZERO
      DO K6=1,NTENS
       NSTR(K6)=-DELTAV(K6)*W6+BVEC(K6)*W5
      ENDDO
      DO K1=1,NTENS
        STATEV(75+K1)=NSTR(K1) ! STRESS IN THE FIBRILLAR PART
      ENDDO
      GAG=ALPHA1*(DETC**(-ALPHA2))
      STATEV(6)=GAG ! S22 STRESS OF GAG PART
      DO K6=1,3
       STRESS(K6)=-GAG
      ENDDO
      DO K6=4,NTENS
       STRESS(K6)=ZERO
      ENDDO
C       write(6,*) "STRESS:", STRESS
       W3=(GM/TWO)*((FOUR*(DET**(TWO/THREE))-FOUR+(THREE*NS0/(DET-NS0))*
     1  (((DET*LOG(DET))/(DET-NS0))-TWO))/(THREE*DET)+((LOG(DET)-ONE)*
     2  NS0*LOG(DET))/((DET-NS0)**TWO))
       CALL TENF(STRESS,NTENS,STRG) ! STRG IS THE TENSOR FORM OF THE STRESS VECTOR
       W2=GAG*(ALPHA2-ONE)
       W3=ZERO
       DO K6=1,NTENS
        K3=IDX(1,K6)
        K4=IDX(2,K6)
        DO K5=1,NTENS
         K1=IDX(1,K5)
         K2=IDX(2,K5)
         DDSDDE(K5,K6)=HALF*(IDENT(K4,K1)*STRG(K3,K2)+IDENT(K3,K2)*
     1    STRG(K1,K4)+IDENT(K3,K1)*STRG(K4,K2)+IDENT(K4,K2)*STRG(K1,K3))
     2    +W6*(IDENT(K1,K3)*IDENT(K2,K4)+IDENT(K1,K4)*IDENT(K2,K3))
     3    -W3*IDENT(K1,K2)*IDENT(K3,K4)+IDENT(K1,K2)*IDENT(K3,K4)*W2
     4    +(IDENT(K1,K4)*IDENT(K2,K3)+IDENT(K1,K3)*IDENT(K2,K4))*GAG
        ENDDO
       ENDDO
C
      STATEV(13)=NS0/DET ! NEW SOLID MATERIAL CONSTANT
      STATEV(14)=RH/DET ! NEW SOLID MATERIAL CONSTANT
      RETURN
      END
C
C
C
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     CUSTOM FUNTIONS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
C
C     MATRIX FORM OF A VECTOR
C
      SUBROUTINE TENF(V,N,M)
      INTEGER N
      DOUBLE PRECISION V(N),M(3,3)
      M(1,1)=V(1)
      M(2,1)=V(4)
      IF (N.EQ.6) THEN
      M(3,1)=V(5)
      ELSE
      M(3,1)=0
      ENDIF
      M(1,2)=V(4)
      M(2,2)=V(2)
      IF (N.EQ.6) THEN
      M(3,2)=V(6)
      M(1,3)=V(5)
      M(2,3)=V(6)
      ELSE
      M(3,2)=0
      M(1,3)=0
      M(2,3)=0
      ENDIF
      M(3,3)=V(3)
      RETURN
      END
C
C     DOUBLE INNER PRODUCT
C
      SUBROUTINE DPROM(A,B,N,C)
      INTEGER i,j,N
      DOUBLE PRECISION A(N),B(N),Amat(3,3),Bmat(3,3),C
      CALL TENF(A,N,Amat)
      CALL TENF(B,N,Bmat)
      C=0.D0
      Do i = 1 , 3
       DO j = 1 , 3
        C=C+Amat(i,j)*Bmat(i,j)
       ENDDO
      ENDDO
      RETURN
      END
C
C     CHANGING INDICES OF TENSORS
C
      INTEGER FUNCTION IDX(i,j)
      INTEGER INDX(2,6)
      INDX(1,1)=1
      INDX(2,1)=1
      INDX(1,2)=2
      INDX(2,2)=2
      INDX(1,3)=3
      INDX(2,3)=3
      INDX(1,4)=1
      INDX(2,4)=2
      INDX(1,5)=1
      INDX(2,5)=3
      INDX(1,6)=2
      INDX(2,6)=3
      IDX=INDX(i,j)
      RETURN
      END
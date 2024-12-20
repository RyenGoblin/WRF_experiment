MODULE MODULE_INTP







CONTAINS



      SUBROUTINE  INTPLIN_HORIZ (PVAL, PI, PJ, PFIELD2, KIX, KJX, KCRS)



















































      IMPLICIT NONE

      REAL    :: PVAL
      REAL    :: PI, PJ
      INTEGER :: KIX, KJX
      REAL, DIMENSION (KIX,KJX) :: PFIELD2
      INTEGER :: KCRS

      INTEGER :: II, IJ
      REAL    :: ZDI, ZDJ, ZDIM, ZDJM







      II = IFIX (PI)
      IJ = IFIX (PJ)


      IF ((1 .LE. II) .AND. (II .LE. KIX-1)  .AND. &
          (1 .LE. IJ) .AND. (IJ .LE. KJX-1)) THEN
















      ZDI  = PI - FLOAT (II)
      ZDJ  = PJ - FLOAT (IJ)

      ZDIM = 1. - ZDI
      ZDJM = 1. - ZDJ





         PVAL =   ZDJM*( ZDIM * PFIELD2 (II,  IJ  )  &
                        +ZDI  * PFIELD2 (II+1,IJ  )) &
                + ZDJ *( ZDIM * PFIELD2 (II,  IJ+1)  &
                        +ZDI  * PFIELD2 (II+1,IJ+1))



      ELSE




         WRITE (0,'(/,A)')            ' Warning in sub. intplin_horiz:'
         WRITE (0,'(A,F7.3,A,I3,A)')  ' yi = ',pi,' outside [1 ',kix,'] or'
         WRITE (0,'(A,F7.3,A,I3,A)')  ' xj = ',pj,' outside [1 ',kjx,']'
         WRITE (0,'(A,/)')            ' No interpolation was performed'

         PVAL = 0. 
      ENDIF





      RETURN
      END SUBROUTINE INTPLIN_HORIZ

      SUBROUTINE INTPLIN_VERTI (PVAL, PZK, PROFILE, KKX)


      IMPLICIT NONE

      REAL,                     INTENT (OUT)   :: PVAL
      REAL,                     INTENT (IN )   :: PZK
      INTEGER,                  INTENT (IN)    :: KKX
      REAL,    DIMENSION (KKX), INTENT (IN)    :: PROFILE

      REAL :: ZF_ABOVE, ZF_BELOW, ZDZ_ABOVE
      INTEGER IK








      IK = IFIX (PZK)




      IF (IK .LT. 1) THEN
          PVAL = PROFILE (1)
          WRITE (0,'(A,F6.3)') ' Point above model lid z = ',PZK
          RETURN
       ENDIF




      IF (IK .GE. KKX) THEN
          PVAL = PROFILE (KKX)
          WRITE (0,'(A,F6.3)') ' Point below model surface z = ',PZK
          RETURN
      ENDIF




      ZF_ABOVE = PROFILE (IK)
      ZF_BELOW = PROFILE (IK+1)








      ZDZ_ABOVE  =   PZK  - REAL (IK)









      PVAL = (ZF_BELOW - ZF_ABOVE) * ZDZ_ABOVE + ZF_ABOVE





      RETURN
      END SUBROUTINE  INTPLIN_VERTI


FUNCTION intplin (x,xx,yy) RESULT (val)

    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl


    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        WRITE (UNIT = 0, FMT = '(A)' ) ' ERROR arrays must have same size'
        STOP 'in intplin.F'
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN    
        val = yy (1)
    ELSE IF (jl .GE. n) THEN    
        val = yy (n)
    ELSE
        val = (xx (jl+1) - x) * yy (jl) + (x - xx (jl)) * yy (jl+1)
        val = val / (xx (jl+1) - xx (jl))
    ENDIF

END FUNCTION intplin

FUNCTION intplog (x,xx,yy) RESULT (val)

    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl


    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        WRITE (UNIT = 0, FMT = '(A)' ) ' ERROR arrays must have same size'
        STOP 'in intplin.F'
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN    
        val = yy (1)
    ELSE IF (jl .GE. n) THEN    
        val = yy (n)
    ELSE
        val = log (xx (jl+1) / x) * yy (jl) + log (x / xx (jl)) * yy (jl+1)
        val = val / log (xx (jl+1) / xx (jl))
    ENDIF

END FUNCTION intplog

FUNCTION locate (x,xx) RESULT (index)

    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx
    REAL                :: x
    INTEGER             :: index

    INTEGER             :: n,jl,jm,ju
    LOGICAL             :: ascnd


    n = size (xx)
    ascnd = (xx (n) >= xx (1))   
    jl = 0                       
    ju = n+1                     

    DO

       IF (ju-jl <= 1) EXIT      

       jm = (ju+jl) / 2.         

       IF (ascnd .EQV. (x >= xx (jm))) THEN
           jl = jm               
       ELSE
           ju = jm               
       ENDIF

    ENDDO

    IF (x .EQ. xx (1)) THEN      
        index = 1
    ELSE IF (x .EQ. xx (n)) THEN
        index = n-1 
    ELSE
        index = jl
    ENDIF
END FUNCTION LOCATE

END MODULE MODULE_INTP

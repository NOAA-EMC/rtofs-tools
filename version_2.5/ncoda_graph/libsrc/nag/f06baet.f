      SUBROUTINE F06BAE(A,B,C,S)
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C     .. Scalar Arguments ..
      REAL              A, B, C, S
C     ..
C
C  Nag  Fortran 77  version of the  SROTG BLAS,  except that c is always
C  returned as non-negative and  b  is overwritten by the tangent of the
C  angle that defines the plane rotation.
C
C  c and s are given as
C
C     c = 1.0/sqrt( 1.0 + t**2 ),   s = c*t   where   t = b/a.
C
C  When  abs( b ) .le. eps*abs( a ),  where  eps is the relative machine
C  precision as  returned by routine  X02AJE,  then  c and s  are always
C  returned as
C
C     c = 1.0  and  s = 0.0
C
C  and when  abs( a ) .le. eps*abs( b ) then c and s are always returned
C  as
C
C     c = 0.0  and  s = sign( t ).
C
C  Note that t is always returned as  b/a, unless this would overflow in
C  which  case the value  sign( t )*flmax  is returned,  where  flmax is
C  the value given by  1/X02AME( ).
C
C  c and s  can be reconstructed from the tangent,  t,  by a call to the
C  Nag basic linear algebra routine F06BCE.
C
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 3-January-1986.
C     Sven Hammarling, Nag Central Office.
C
C
C     .. Parameters ..
      REAL              ONE, ZERO
      PARAMETER         (ONE=1.0E+0,ZERO=0.0E+0)
C     .. Local Scalars ..
      REAL              T
      LOGICAL           FAIL
C     .. External Functions ..
      REAL              F06BLE
      EXTERNAL          F06BLE
C     .. External Subroutines ..
      EXTERNAL          F06BCE
C     ..
C     .. Executable Statements ..
      IF (B.EQ.ZERO) THEN
         C = ONE
         S = ZERO
      ELSE
         T = F06BLE(B,A,FAIL)
         CALL F06BCE(T,C,S)
         A = C*A + S*B
         B = T
      END IF
C
      RETURN
C
C     End of F06BAE. ( SROTGC )
C
      END

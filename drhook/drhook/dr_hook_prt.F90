! (C) Copyright 2014- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

SUBROUTINE DR_HOOK_PRT(KUNIT, CDSTR)
USE PARKIND1  ,ONLY : JPIM
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
CHARACTER(LEN=*), INTENT(IN) :: CDSTR
IF (KUNIT < 0) THEN
  WRITE(*,'(A)') CDSTR
ELSE
  WRITE(KUNIT,'(A)') CDSTR
  CALL FLUSH(KUNIT)
ENDIF
END SUBROUTINE DR_HOOK_PRT

SUBROUTINE DR_HOOK_PRT_CHAR(KUNIT, CD, KLD)
USE PARKIND1  ,ONLY : JPIM
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
INTEGER(KIND=JPIM), INTENT(IN) :: KLD
CHARACTER(LEN=1), INTENT(IN) :: CD(KLD)
IF (KUNIT < 0) THEN
  WRITE(*,'(40A1)') CD
ELSE
  WRITE(*,'(40A1)') CD
  CALL FLUSH(KUNIT)
ENDIF
END SUBROUTINE DR_HOOK_PRT_CHAR

SUBROUTINE DR_HOOK_PRT_LOGICAL(KUNIT, LD, KLD)
USE PARKIND1  ,ONLY : JPIM
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
INTEGER(KIND=JPIM), INTENT(IN) :: KLD
LOGICAL, INTENT(IN) :: LD(KLD)
IF (KUNIT < 0) THEN
  WRITE(*,'(40L2)') LD
ELSE
  WRITE(KUNIT,'(40L2)') LD
  CALL FLUSH(KUNIT)
ENDIF
END SUBROUTINE DR_HOOK_PRT_LOGICAL

SUBROUTINE DR_HOOK_PRT_I4(KUNIT, KD, KLD)
USE PARKIND1  ,ONLY : JPIM
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
INTEGER(KIND=JPIM), INTENT(IN) :: KLD
INTEGER(KIND=JPIM), INTENT(IN) :: KD(KLD)
IF (KUNIT < 0) THEN
  WRITE(*,'(5I12)') KD
ELSE
  WRITE(KUNIT,'(5I12)') KD
  CALL FLUSH(KUNIT)
ENDIF
END SUBROUTINE DR_HOOK_PRT_I4

SUBROUTINE DR_HOOK_PRT_I8(KUNIT, KD, KLD)
USE PARKIND1  ,ONLY : JPIM, JPIB
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
INTEGER(KIND=JPIM), INTENT(IN) :: KLD
INTEGER(KIND=JPIB), INTENT(IN) :: KD(KLD)
IF (KUNIT < 0) THEN
  WRITE(*,'(5I20)') KD
ELSE
  WRITE(KUNIT,'(5I20)') KD
  CALL FLUSH(KUNIT)
ENDIF
END SUBROUTINE DR_HOOK_PRT_I8

SUBROUTINE DR_HOOK_PRT_R4(KUNIT, PD, KLD)
USE PARKIND1  ,ONLY : JPIM, JPIB, JPRM
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
INTEGER(KIND=JPIM), INTENT(IN) :: KLD
REAL(KIND=JPRM), INTENT(IN) :: PD(KLD)
IF (KUNIT < 0) THEN
  WRITE(*,*) PD
ELSE
  WRITE(KUNIT,*) PD
  CALL FLUSH(KUNIT)
ENDIF
END SUBROUTINE DR_HOOK_PRT_R4

SUBROUTINE DR_HOOK_PRT_R8(KUNIT, PD, KLD)
USE PARKIND1  ,ONLY : JPIM, JPIB, JPRB
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
INTEGER(KIND=JPIM), INTENT(IN) :: KLD
REAL(KIND=JPRB), INTENT(IN) :: PD(KLD)
IF (KUNIT < 0) THEN
  WRITE(*,*) PD
ELSE
  WRITE(KUNIT,*) PD
  CALL FLUSH(KUNIT)
ENDIF
END SUBROUTINE DR_HOOK_PRT_R8


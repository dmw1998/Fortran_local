REAL FUNCTION cube_root(x)
    IMPLICIT NONE

    ! Function to calculate the cube root of a positive
    ! real number

    ! Dummy argument declaration
    REAL, INTENT(IN) :: x

    ! Local variable declaration
    REAL :: log_x       ! internal variable or local variable (仅在方程内使用)
    
    ! Calculate cube root by using logs
    log_x = LOG(x)
    cube_root = EXP(log_x/3.0)

END FUNCTION cube_root
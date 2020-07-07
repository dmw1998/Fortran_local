SUBROUTINE roots(x,square_root,cube_root,fourth_root,fifth_root)
    IMPLICIT NONE

    ! Subroutine to calculate various roots of a positive real
    ! number supplied as the first argument, and return it in
    ! the second to fifth arguments

    ! Dimmy argument declarations
    REAL, INTENT(IN) :: x
    REAL, INTENT(OUT) ::square_root,cube_root,fourth_root,fifth_root

    ! Local variable declarations
    REAL :: log_x

    ! Calculate square root by using intrinsic sqrt
    square_root = sqrt(x)

    ! Calculate other roots by using logs
    log_x = log(x)
    cube_root = exp(log_x/3.0)
    fourth_root = exp(log_x/4.0)
    fifth_root = exp(log_x/5.0)

END SUBROUTINE roots

program subroutine_demo
implicit none

! A program to demonstrate the use of the subroutine roots

! Variable declarations
real :: pos_num,root_2,root_3,root_4,root_5

! Get positive number from user
print *,"Please type a positive real number: "
read *,pos_num

! Obtain roots
CALL roots(pos_num,root_2,root_3,root_4,root_5)

! Display number and its roots
print *,"The square root of ",pos_num," is ",root_2
print *,"The cube root of ",pos_num," is ",root_3
print *,"The fourth root of ",pos_num," is ",root_4
print *,"The fifith root of ",pos_num," is ",root_5

end program subroutine_demo
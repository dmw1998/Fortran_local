program exercise_2_2
    implicit none

    REAL :: number
    ! This program contains a number of errors and
    ! is not a good expamle of Fortran 90 at all!
    ! After fixing
    PRINT *,"This &
            &is a silly &
            &program"
    PRINT *,"Type a number"
    READ *, number
    PRINT *,"Thank you. &
            &Your number was ",number

end program exercise_2_2
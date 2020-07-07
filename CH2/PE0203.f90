program test
    implicit none       ! Missed but not detected

    REAL :: number      ! Missed
    ! This program contains four major errors 
    ! and three examples of bad programming style (1st error)
    PRINT *,"Please type a number"      ! 2nd error
    READ *, number      ! 3rd & 4th errors
    PRINT *,"The number you typed was ", number
    
end program test        ! The program name missed
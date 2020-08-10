INTEGER FUNCTION next_int()
    IMPLICIT NONE

    ! This function requests an integer from the keyboard

    ! Get number
    print *,"Please type an integer"
    read *,next_int

END FUNCTION next_int

program next_int_test
implicit none

! This program displays the product of two numbers which are
! typed at the keyboard

! External function declaration
integer, external :: next_int

! Variable declaration
integer :: product

product = next_int()*next_int()

print *,"The product is ",product

end program next_int_test
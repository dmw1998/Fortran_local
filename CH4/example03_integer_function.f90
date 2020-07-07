INTEGER FUNCTION next_int()
    IMPLICIT NONE

    ! This function requests an integer from the keyboard

    ! Get number
    print *,"Please type an integer"
    read *,next_int

END FUNCTION next_int
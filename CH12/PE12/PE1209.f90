module for_integers
implicit none
! This module defines an enumeration type and an operator to assign a appropriate value

    type int_enum
        integer :: month
    end type int_enum

    interface assignment(=)
        module procedure assign_int_enum
    end interface

contains

    subroutine assign_int_enum(a,numb)

        type(int_enum),intent(out) :: a
        integer,intent(in) :: numb

        select case(numb)
        case (1:12)
            a%month = numb
        case default
            a%month = -1
            print *,"Error: cannot assign ",numb
        end select
    end subroutine assign_int_enum

end module for_integers

module for_characters
implicit none
! This module defines an enumeration type and an operator to assign a appropriate value

    type char_enum
        character(len=6) :: seasons 
    end type char_enum

    interface assignment(=)
        module procedure assign_char_enum
    end interface

contains

    subroutine assign_char_enum(s,chara)

        type(char_enum),intent(out) :: s
        character(len=*),intent(in) :: chara

        select case(chara)
        case ("spring","summer","autumn","winter")
            s%seasons = chara
        case default
            s%seasons = " "
            print *,"Error: cannot assign ",chara
        end select
    end subroutine assign_char_enum

end module for_characters

program PE1209_test_module
use for_integers
use for_characters
implicit none

type(int_enum) :: k, b
type(char_enum) :: m, n

k = 3
b = 13

m = "spring"
n = "ppppk"

end program PE1209_test_module
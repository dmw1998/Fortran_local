module MA
implicit none

interface show
    module procedure show_int       
    module procedure show_character 
end interface

contains
    subroutine show_int(n)
        implicit none
        integer, intent(in) :: n
        write(*,'("n =",I3)') n
        return
    end subroutine show_int

    subroutine show_character(str)
        implicit none
        character(len=*), intent(in) :: str
        write(*,'("str = ",A)') str
        return
    end subroutine show_character
end module

program main
use MA
implicit none

call show(1)  ! call show_int(1) since 1 is integer

call show("FORTRAN 95")   ! call show_character("FORTRAN 95")

end program

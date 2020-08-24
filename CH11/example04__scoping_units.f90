subroutine scoping_unit_example
    implicit none

    ! type defination
    type date
        integer :: day
        character(len=3) :: month
        integer :: year
    end type date

    ! Procedure interface body
    interface
        subroutine get_date(day,month,year)
            implicit none
            integer,intent(in) :: day,year
            character(len=*),intent(in) :: month
        end subroutine get_date
    end interface

    ! Local variables
    integer :: day,year
    character(len=10) :: month
    type(date) :: today


end subroutine
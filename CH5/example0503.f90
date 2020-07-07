module geometric_procedures
    use geometric_data
    implicit none

contains
    subroutine line_two_points(line_1,point_1,point_2,status)
        implicit none

        ! Dummy arguments
        type(line), intent(out) ::  line_1
        type(point), intent(in) :: point_1,point_2
        integer :: status

        ! Check to see wether points are coincident
        if (point_1%x==point_2%x .AND. point_1%y==point_2%y) then
            ! Points are coincident -- return error flag
            status = -1
        else
            ! Points are distinct, so calculaaate the coefficients
            ! of the equation representing the line
            line_1%a = point_2%y - point_1%y
            line_1%b = point_2%x - point_1%x
            line_1%c = point_1%y*point_2%x - point_2%y*point_1%x

            ! Set status to indicate success
            status = 0
        end if
        print *,status

    end subroutine line_two_points

end module geometric_procedures

program example0305
use geometric_procedures
implicit none

! A program to test the subroutine line_two_points

! Varibale declarations
type(point) :: p1,p2
type(line) :: p1_to_p2
integer :: status

! Read data
print *,"Please type co-ordinates of first point"
read *,p1
print *,"Please type co-ordinates of second point"
read *,p2

! Call procedure to calculate the equation of the line
call line_two_points(p1_to_p2,p1,p2,status)

end program example0305
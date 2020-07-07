module geometric_data
implicit none

! Type definitions
TYPE point
    REAL :: x, y        ! Cartesian coordinates of the point
END TYPE point

TYPE line
    REAL :: a,b,c       ! coefficients of defining equation
END TYPE line

end module geometric_data

module geometric_procedures
    use geometric_data
    implicit none

contains
    subroutine line_two_points(line_1,point_1,point_2)
        implicit none

        ! Dummy arguments
        type(line), intent(out) ::  line_1
        type(point), intent(in) :: point_1,point_2

        ! Calculate coeficients of equation representing the line
        line_1%a = point_2%y - point_1%y
        line_1%b = point_2%x - point_1%x
        line_1%c = point_1%y*point_2%x - point_2%y*point_1%x
    end subroutine line_two_points

end module geometric_procedures

program geometry
use geometric_procedures
implicit none

! A program to test the subroutine line_two_points

! Varibale declarations
type(point) :: p1,p2
type(line) :: p1_to_p2

! Read data
print *,"Please type co-ordinates of first point"
read *,p1
print *,"Please type co-ordinates of second point"
read *,p2

! Call procedure to calculate the equation of the line
call line_two_points(p1_to_p2,p1,p2)

! Print result
print *,"The equation of the line joining these two points is"
print *,"ax + by + c = 0"
print *,"where a = ", p1_to_p2%a
print *,"      b = ", p1_to_p2%b
print *,"      c = ", p1_to_p2%c

end program geometry
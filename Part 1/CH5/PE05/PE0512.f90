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
    subroutine line_two_points(point_intersection,line_1,line_2)
        implicit none

        ! Dummy arguments
        type(point), intent(out) :: point_intersection 
        type(line), intent(in) :: line_1,line_2

        ! Local variables
        real :: detA

        if (abs(line_1%a*line_2%b - line_2%a*line_1%b) < 10E-15) then
            print *,"Two lines are parallel."
        else
            ! Calculate detA
            detA = 1.0/(line_1%a*line_2%b - line_2%a*line_1%b)

            ! Calculate the coordinate of intersection point
            point_intersection%x = -detA*(line_1%a*line_1%c+line_1%b*line_2%c) 
            point_intersection%y = -detA*(line_2%a*line_1%c+line_2%b*line_2%c)

            print *,"The coordinate of intersection point is (",point_intersection%x, &
                    & ",",point_intersection%y,")."
        end if 
        
    end subroutine line_two_points

end module geometric_procedures

program PE0413
use geometric_procedures
implicit none

type(point) :: point_intersection
type(line) :: line_1,line_2

print *,"Please input the coefficients of line 1."
read *, line_1

print *,"Please input the coefficients of line 2."
read *, line_2

call line_two_points(point_intersection,line_1,line_2)

end program PE0413
module geometry
    implicit none

    ! Type definitions
    type circle
        character(len=12) :: name
        real :: x,y,r       ! coordinates of center, and radius
    end type circle

    type line
        character(len=12) :: name
        real :: a,b,c       ! coefficients of defining equation
    end type line

    type point
        character(len=12) :: name
        real :: x,y         ! Cartesian coordinates of the point
    end type point

    ! Generic procedure definition
    interface gen_line
        module procedure line_two_points
        module procedure line_point_perpto_line
        module procedure line_point_tanto_circle
    end interface

contains

    subroutine line_two_points(line_1,point_1,point_2)
        ! Dummy arguments
        type(line),intent(out) :: line_1
        type(point),intent(in) :: point_1,point_2

        ! Local variable
        real :: small = 10.0*tiny(1.0)

        ! Calculate coefficients of equation of line
        line_1%a = point_2%y - point_1%y
        line_1%b = point_2%x - point_1%x
        line_1%c = point_1%y*point_2%x - point_2%y*point_1%x

        ! Check for coincident points
        if (abs(line_1%a) < small .and. abs(line_1%b) < small) then
            ! Points are coincident - return all coefficients zero
            line_1%a = 0.0
            line_1%b = 0.0
            line_1%c = 0.0
        end if
    end subroutine line_two_points

    subroutine line_point_perpto_line(line_1,point_1,line_2)
        ! Dummy arguments
        type(line),intent(out) :: line_1
        type(point),intent(in) :: point_1
        type(line),intent(in) :: line_2

        if (line_2%a == 0) then
            line_1%a = 1.0
            line_1%b = 0.0
            line_1%c = -point_1%x
        else if (line_2%b == 0) then
            line_1%a = 0.0
            line_1%b = 1.0
            line_1%c = -point_1%y
        else
            line_1%a = line_2%b
            line_1%b = -line_2%a
            line_1%c = line_2%a*point_1%y - line_2%b*point_1%x
        end if

    end subroutine line_point_perpto_line

    subroutine line_point_tanto_circle(line_1,line_2,point_1, &
                                       circle_1,modifier)
        ! Dummy arguments
        type(line),intent(out) :: line_1
        type(line),intent(out),optional :: line_2
        type(point),intent(in) :: point_1
        type(circle),intent(in) :: circle_1
        character(len=6) :: modifier

        ! Local variables
        real :: dis, delta

        dis = (point_1%x-circle_1%x)**2 + (point_1%y-circle_1%y)**2

        if (dis < circle_1%r**2) then
            print *,"There are no tangent line for this point since it is inside the circle."

        else if (dis == circle_1%r**2)  then
            line_1%a = (circle_1%x-point_1%x)/(point_1%y-circle_1%y)
            line_1%b = -1.0
            line_1%c = (-point_1%x*(circle_1%x-point_1%x) + &
                        point_1%y*(point_1%y-circle_1%y))/(point_1%y-circle_1%y)

        else
            if ((circle_1%x-point_1%x)**2-circle_1%r**2 == 0) then
                if ((point_1%y-circle_1%y)**2 - circle_1%r**2 /= 0) then
                    line_2%a = (circle_1%r**2 - &
                                (point_1%y-circle_1%y)**2)/(2*(circle_1%x-point_1%x)*(point_1%y-circle_1%y))
                    line_2%b = -1.0
                    line_2%c = -line_2%a*point_1%x+point_1%y
                    line_1%a = 1.0
                    line_1%b = 0.0
                    line_1%c = -point_1%x
                else
                    line_1%a = 1.0
                    line_1%b = 0.0
                    line_1%c = -point_1%x
                    line_2%a = 0.0
                    line_2%b = 1.0
                    line_2%c = -point_1%y
                end if        

            else 
                if ((point_1%y-circle_1%y)**2 - circle_1%r**2 == 0) then 
                    line_2%a = (-2*(circle_1%x-point_1%x)*(point_1%y-circle_1%y))/((circle_1%x-point_1%x)**2-circle_1%r**2)
                    line_2%b = -1.0
                    line_2%c = -line_2%a*point_1%x + point_1%y
                    line_1%a = 0.0
                    line_1%b = 1.0
                    line_1%c = -point_1%y
                else
                    delta = (-2*(circle_1%x-point_1%x)*(circle_1%y-point_1%y))**2 - &
                            4*((circle_1%x-point_1%x)**2-circle_1%r**2)*((circle_1%y-point_1%y)**2-circle_1%r**2)
                    line_1%a = (2*(circle_1%x-point_1%x)*(circle_1%y-point_1%y) + &
                            sqrt(delta))/(2*((circle_1%x-point_1%x)**2-circle_1%r**2))
                    line_2%a = (2*(circle_1%x-point_1%x)*(circle_1%y-point_1%y) - &
                            sqrt(delta))/(2*((circle_1%x-point_1%x)**2-circle_1%r**2))
                    line_1%c = -line_1%a*point_1%x+point_1%y
                    line_2%c = -line_2%a*point_1%x+point_1%y
                    line_1%b = -1
                    line_2%b = -1
                end if
            end if
        end if

    end subroutine line_point_tanto_circle

end module geometry
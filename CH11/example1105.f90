interface gen_line
    module procedure line_two_points
    module procedure line_point_perpto_line
    module procedure line_point_tanto_circle
end interface

interface gen_line
    
    subroutine line_two_points(line_1,point_1,point_2)
        implicit none
        type(line),intent(in) :: line_1
        type(point),intent(in) :: point_1,point_2
    end subroutine line_two_points

    subroutine line_point_perpto_line(line_1,point_1,line_2)
        implicit none
        type(line),intent(out) :: line_1
        type(point),intent(in) :: point_1
        type(line),intent(in) :: line_2
    end subroutine line_point_perpto_line

    subroutine line_point_tanto_circle(line_1,point_1, &
                                       circle_1,modifier)
        implicit none
        type(line),intent(out) :: line_1
        type(point),intent(in) :: point_1
        type(line),intent(in) :: circle_1
        character(len=6) :: modifier
    end subroutine line_point_tanto_circle
end interface
interface
subroutine demo(a,b,c,d,x)
    implicit none
    real,intent(inout) :: a,b,c,d 
    integer,optional,intent(in) :: x 
end subroutine demo
end interface

interface
real function mean(array)
    implicit none
    integer,dimension(:) :: array
end function
end interface

interface
subroutine input(num_pts,points)
    use geometric_data
    implicit none
    integer,intent(out) :: num_pts
    type(point),dimension(:),intent(out) :: points
end subroutine
end interface
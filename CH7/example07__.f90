subroutine trivial_sub(a,b,c)
    implicit none
    real,dimension(:),intent(out) :: a
    real,dimension(:),intent(in) :: b,c
    a = b+c
end subroutine trivial_sub

function trivial_fun(x,y)
    implicit none
    real,dimension(:),intent(in) :: x,y
    real,dimension(size(x)) :: trivial_fun
    trivial_fun = x+y
end function trivial_fun
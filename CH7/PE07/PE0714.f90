module vec3d_cal
implicit none
save

contains
    function dot_prod(a,b)
    implicit none

    real :: dot_prod
    real,dimension(3),intent(in) :: a,b

    integer :: i 

    do i=1,3
        dot_prod = dot_prod + a(i)*b(i)
    end do

    end function dot_prod

    function vec_prod(a,b)
    implicit none

    real,dimension(3) :: vec_prod
    real,dimension(3),intent(in) :: a,b

    vec_prod(1) = a(2)*b(3) - a(3)*b(2)
    vec_prod(2) = a(3)*b(1) - a(1)*b(3)
    vec_prod(3) = a(1)*b(2) - a(2)*b(1)

    end function vec_prod

    function tri_prod(a,b,c)
    implicit none

    real :: tri_prod
    real,dimension(3),intent(in) :: a,b,c 

    tri_prod = dot_prod(a,vec_prod(b,c))

    end function tri_prod

end module vec3d_cal

program PE0714
use vec3d_cal
implicit none

real,dimension(3) :: a=(/1,2,3/),b=(/2,3,4/),c=(/3,4,5/)
real :: abc,bca,cab

abc = tri_prod(a,b,c)
bca = tri_prod(b,c,a)
cab = tri_prod(c,a,b)

print *, abc,bca,cab

end program PE0714
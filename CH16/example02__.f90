module small
    implicit none
contains
    function even_pointer(a) result(p)
        real, dimension(:), pointer :: a
        real, dimension(:), pointer :: p
        ! The result of an even_pointer is an array
        ! pointer to the even-numbered elements
        ! of the input array a.
        p => a(2 :: 2)      ! p points to an array section
    end function even_pointer
end module small

program pointer_function
use small
implicit none

real, dimension(15), target :: a 
real, dimension(:), pointer :: pa, p, q, r 

pa => a 
p => even_pointer(pa)   ! p points to even elements of a
q => even_pointer(p)    ! q points to even elements of p
r => even_pointer(q)    ! r points to even elements of q

end program pointer_function
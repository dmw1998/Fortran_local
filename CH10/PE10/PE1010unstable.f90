module constant1010
implicit none
integer,parameter :: q = selected_real_kind(P=6)
end module constant1010

program PE1001unstable
use constant1010
implicit none

real(q),dimension(0:10) :: integralsIn
integer :: i, n=10 


integralsIn(0) = 1-exp(-1.0)
do i=1,n
    integralsIn(i) = -exp(-1.0) + i*integralsIn(i-1)
enddo

do i=0,n
    print '(E15.6)', integralsIn(i)
enddo 

end program PE1001unstable
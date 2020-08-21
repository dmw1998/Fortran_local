program PE1001stable
use constant1010
implicit none

real(q),dimension(0:20) :: integralsIn
integer :: i, n=10 


integralsIn(20) = 0
do i=19,0,-1
    integralsIn(i) = (exp(-1.0) + integralsIn(i+1))/(i+1)
enddo

do i=0,10
    print '(E15.6)', integralsIn(i)
enddo 

end program PE1001stable
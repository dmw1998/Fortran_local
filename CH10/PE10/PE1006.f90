program PE1006
implicit none

real(8), external :: fac

real(8) :: calsinx5=0.0, x=0.8
integer :: i 

do i = 1,6
    calsinx5 = calsinx5 + (-1.0)**(i+1.0)*((x)**(2.0*i-1.0) / fac(i))
enddo

print '("sin(",F5.3,") = ", F10.8, " by Taylor series 6 terms.")', x, calsinx5
print '("sin(",F5.3,") = ", F10.8, " by instrinsic function.")', x, sin(x)
print '("The difference is ", F10.8, ", and the accuracy is ", F7.5)', calsinx5 - sin(x), (calsinx5 - sin(x))/sin(x)

calsinx5 = 0.0
do i = 1,14
    calsinx5 = calsinx5 + (-1.0)**(i+1.0)*((x)**(2.0*i-1.0) / fac(i))
enddo

print '("sin(",F5.3,") = ", F10.8, " by Taylor series 14 terms.")', x, calsinx5
print '("sin(",F5.3,") = ", F10.8, " by instrinsic function.")', x, sin(x)
print '("The difference is ", F10.8, ", and the accuracy is ", F7.5)', calsinx5 - sin(x), (calsinx5 - sin(x))/sin(x)

end program PE1006

function fac(n)

integer,intent(in) :: n
integer :: i 
real(8) :: fac

fac = 1.0
do i = 1,n 
    fac = fac*i
enddo

end function fac
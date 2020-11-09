program PE1008
implicit none

real(8),external :: f 

real(8) :: x=2.0, h, df, fx
integer :: i 

fx = f(x)

do i = 1,50 
    h = 10.0**(-i+1.0)
    df = (f(x+h) - fx)/h
    print *, i, h, df, df-1.0
enddo    

end program PE1008

function f(x)

real(8) :: f 
real(8),intent(in) :: x

f = x**2 - 3*x + 2

end function f 

! The best h is the 12th, h = 10^(-11)
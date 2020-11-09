program PE1009d
use constant1009
implicit none

real(kind=q),external :: f 

real(kind=q) :: x=3.0, h, df2=2.0, df1=4.0, fx
integer :: i=0

fx = f(x)

do while (abs(df2-df1) > 10.0**(-5.0))
    df1 = df2
    i = i+1
    h = 10.0**(-i+1.0)
    df2 = (f(x+h) - fx)/h
    print *, i, h, df2

end do 

print '("The Newton quotient for the function at point x=3 is ", F7.5)', df2

end program PE1009d

function f(x)
use constant1009
implicit none

real(kind=q) :: f 
real(kind=q),intent(in) :: x

f = x**3 - 6.0*x**2 + 12.0*x - 5.0

end function f 
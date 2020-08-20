module constant1009
implicit none
integer,parameter :: q = selected_real_kind(P=14)
end module constant1009

program PE1009
use constant1009
implicit none

real(kind=q),external :: f 

real(kind=q) :: x=1.5, h=1, df2=3.0, df1=4.0, fx
integer :: i=0

fx = f(x)

do while (abs(df2-df1) > 10.0**(-14.0))
    df1 = df2
    i = i+1
    h = 10.0**(-i+1.0)
    df2 = (f(x+h) - fx)/h
    print *, i, h, df2

end do 

print '("The Newton quotient fot the function at point x=1.5 is ", F16.14)', df2

end program PE1009

function f(x)
use constant1009
implicit none

real(kind=q) :: f 
real(kind=q),intent(in) :: x

f = x**2 - 3*x + 2

end function f 
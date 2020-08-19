program PE1005a
implicit none

real,external :: f 

real(kind=8) :: left, right, mid 
real(kind=8) :: xleft,xright
integer :: i

i = 0
left = -4.5
mid = left + 0.1
do while (f(mid)<f(left))
    i = i + 1
    print *, i, mid, f(mid)
    left = mid
    mid = left + 0.1
enddo 

print '(F10.5, 5X, E15.7)',left, f(left)

i = 0
left = 2.5
mid = left + 0.1
do while (f(mid)<f(left))
    i = i + 1
    print *, i, mid, f(mid)
    left = mid
    mid = left + 0.1
enddo 

print '(F10.5, 5X, E15.7)',left, f(left)

end program PE1005a

function f(x)

real(kind=8),intent(in) :: x

f = x**4.0 + 2.0*x**3.0 - 23.0*x**2.0 - 24.0*x + 144.0

end function f 
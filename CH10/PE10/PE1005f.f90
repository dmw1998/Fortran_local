program PE1005a
implicit none

real,external :: f 

real(kind=8) :: left, right, mid 
real(kind=8) :: xleft,xright
integer :: i

i = 1
left = -13.5
right = -10
mid = left + abs(left - right)/2.0
do while (abs(f(mid)) > 10.0**(-5.0))
    if (f(mid)*f(left) > 0) then
        left = mid 
    else
        right = mid
    endif
    print *,i,f(left),f(right)
    mid = left + abs(left - right)/2.0 
    i = i+1
enddo

print '(F10.5, 5X, E15.7)',mid, f(mid)

left = 2
mid = left + 0.000001
do while (f(mid)>f(left))
    left = mid
    mid = left + 0.000001
enddo 

print *," "
print '(F10.5, 5X, E15.7)',left, f(left)
print *," "

i = 1
left = 10
right = 15.5
mid = left + abs(left - right)/2.0
do while (abs(f(mid)) > 10.0**(-5.0))
    if (f(mid)*f(left) > 0) then
        left = mid 
    else
        right = mid
    endif
    print *,i,f(left),f(right)
    mid = left + abs(left - right)/2.0 
    i = i+1
enddo

print '(F10.5, 5X, E15.7)',mid, f(mid)


end program PE1005a

function f(x)

real(kind=8),intent(in) :: x

f = 9*x**4.0 - 42.0*x**3.0 - 1040.0*x**2.0 + 5082.0*x - 5929.0

end function f 
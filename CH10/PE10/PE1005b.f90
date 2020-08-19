program PE1005b
implicit none

real,external :: f 

real(kind=8) :: left, right, mid 
real(kind=8) :: xleft,xright
integer :: i

i = 1
left = -1
right = 0
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

i = 1
left = 1
right = 1.55
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

i = 1
left = 1.55
right = 2
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


end program PE1005b

function f(x)

real(kind=8),intent(in) :: x

f = 20.0*x**3.0 - 52.0*x**2.0 + 17.0*x + 24.0

end function f 
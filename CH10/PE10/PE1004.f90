program PE1004
implicit none

real,external :: f 

real(kind=8) :: left, right, mid
integer :: i

i = 1
left = -0.4
right = -0.3
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

print *, mid, f(mid)

i = 1
left = 1.5
right = 1.6
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
 
print *, mid, f(mid)

i = 1
left = 1.6
right = 1.7
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
 
print *, mid, f(mid)

end program PE1004


function f(x)

real(kind=8),intent(in) :: x

f = 63.0*x**3.0 - 183.0*x**2.0 + 97.0*x + 55.0

end function f 

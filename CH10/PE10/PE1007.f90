program PE1007
implicit none

real(8),external :: equfora

real(kind=8) :: left=2.52, right=2.53, mid
integer :: i = 0

if (equfora(left)*equfora(right) > 0) then
    print *, equfora(left), equfora(right)
    stop
endif

mid = left + abs(left - right)/2.0
do while (abs(equfora(mid)) > 10.0**(-5.0))
    i = i+1
    if (equfora(mid)*equfora(right) > 0) then
        right = mid 
    else
        left = mid
    endif
    print *,i,equfora(left),equfora(right)
    mid = left + abs(left - right)/2.0 

    if (i > 150) then
        exit
    endif
enddo

print '(F10.8, 5X, E15.8)',mid, equfora(mid)


end program PE1007


function equfora(a)

real(8) :: equfora
real(8),intent(in) :: a

real(8),dimension(11) :: x = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.4, 0.9, 1.0]
real(8),dimension(11) :: y = [1.07, 1.40, 1.56, 2.30, 2.92, 3.52, 4.57, 6.00, 7.33, 9.69, 12.04]
integer :: i 

equfora = sum(x*(y-exp(a*x))*exp(a*x))

end function equfora
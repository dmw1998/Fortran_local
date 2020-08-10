program PE0612
implicit none

integer :: a1 = 0, a2 = 1, a3, output
integer :: i = 0
real :: ratio,diff

do
    a3 = a1 + a2
    output = a1
    a1 = a2
    a2 = a3
    ratio = (1.0*a1)/(1.0*a2)
    diff = ratio - (sqrt(5.0)-1.0)/2.0
    if (abs(diff) < 1E-6) then
        exit
    end if   
    i = i+1
end do

print *,"The ratio between",i,"th term",output,"and",i+1,"th term",a1,&
        "is close enough to the Golden Ratio with difference less than 10^(-6)."

end program PE0612
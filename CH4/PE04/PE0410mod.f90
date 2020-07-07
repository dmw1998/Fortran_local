module ten_numbers
implicit none
save
real :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
end module ten_numbers

subroutine arith_mean
    use ten_numbers
    implicit none
    print *,"The arithmetic mean of these number is ",(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)/10.0    
end subroutine arith_mean

subroutine geo_mean
    use ten_numbers
    implicit none
    print *,"The geometric mean of these number is ",(x1*x2*x3*x4*x5*x6*x7*x8*x9*x10)**(1/10)   
end subroutine geo_mean

program PE0410
use ten_numbers
implicit none

print *,"Please input the ten positive numbers."
read *, x1,x2,x3,x4,x5,x6,x7,x8,x9,x10

call arith_mean
call geo_mean

end program PE0410
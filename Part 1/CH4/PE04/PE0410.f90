subroutine cal_arith_mean(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,mean)
    real,intent(in)  :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
    real,intent(out) ::  mean

    mean = (x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)/10.0
    
end subroutine cal_arith_mean

subroutine cal_geo_mean(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,mean)
    real,intent(in)  :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
    real,intent(out) ::  mean

    mean = (x1*x2*x3*x4*x5*x6*x7*x8*x9*x10)**(1/10)
    
end subroutine cal_geo_mean

program PE0410
implicit none

real :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
real :: arith_mean, geo_mean

print *,"Please input the ten positive numbers."
read *, x1,x2,x3,x4,x5,x6,x7,x8,x9,x10

call cal_arith_mean(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,arith_mean)
call cal_geo_mean(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,geo_mean)

print *,"The arithmetic mean of these number is ",arith_mean
print *,"The geometric mean of these number is ",geo_mean

end program PE0410
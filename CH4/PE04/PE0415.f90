subroutine radian2degree(radian,degree)
real,intent(in)  :: radian
real ::  degree
    ! Convert radians to degrees

    degree = radian/3.14159265*180.0        ! pi = 3.14159265
    
end subroutine radian2degree

subroutine degree2times(degree,time)
real,intent(in)  :: degree
real ::  time
    ! Convert degree to timess

    time = degree/360.0*24.0
    
end subroutine degree2times

subroutine calculation(right_ascension,declination,beta,lambda)
real :: right_ascension, declination
real,intent(in) :: beta, lambda

    real,parameter :: epsilon = 0.4091

    right_ascension = atan2(sin(lambda)*cos(epsilon)-tan(beta)*sin(epsilon),cos(lambda))
    declination = asin(sin(beta)*cos(epsilon)+cos(beta)*sin(epsilon)*sin(lambda))
        
end subroutine calculation

program PE0415
implicit none

! beta: ecliptic latitude   lambda: ecliptic longitude
! alpha: right ascension    delta: declination

real :: right_ascension,declination,beta,lambda
real :: alpha,delta,degree

print *,"Please input the ecliptic latitude and longitude."
read *, beta, lambda

call calculation(right_ascension,declination,beta,lambda)
call radian2degree(right_ascension,degree)
call degree2times(degree,alpha)
call radian2degree(declination,delta)

print *,"The right ascension in hour and the declination in degree are ", &
        & alpha, delta

end program PE0415
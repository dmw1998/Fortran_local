subroutine cal(t,position,velocity,acceleration)
implicit none

! This subroutine calculates the position, velocity and acceleration
! of a body undergoing simple harmonic motion.

    real,intent(in)  :: t
    real :: position,velocity,acceleration

    real, parameter :: n = 3.14159265, epsilon = 0, a = 2.5

    position = a*sin(n*t+epsilon)
    velocity = n*a*cos(n*t+epsilon)
    acceleration = -a*n**2*sin(n*t+epsilon)

end subroutine cal

program PE0412
implicit none

real :: t,position,velocity,acceleration

print *,"Please input the time in second."
read *, t

call cal(t,position,velocity,acceleration)

print *,"The position, velocity and acceleration are ",position,velocity,acceleration

end program PE0412
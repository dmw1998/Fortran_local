program PE0314
implicit none

real, parameter :: g = 9.81
real :: s, t, u         ! s = 1/2 at^2 + ut (a = g)

print *,"Please input the initial velocity in m/s and the time of flight in s."
read *, u, t

s = 0.5*g*t**2 + u*t

print *, "The body fell from ",s,"m height."

end program PE0314
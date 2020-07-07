function escape_velocity(M,R)
implicit none

real, intent(in) :: M,R
real :: escape_velocity

real, parameter :: G = 6.673E-11

escape_velocity = (2.0*G*M)**(1/2)/R 

end function escape_velocity

program EP0414
implicit none

real, external :: escape_velocity

real, parameter :: mass_Earth = 6.0E24, radius_Earth = 6.4E6
real, parameter :: mass_Moon = 7.4E22, radius_Moon = 1.7E6
real, parameter :: mass_Jupiter = 1.9E27, radius_Jupiter = 7.1E7
real :: velocity_Earth, velocity_Moon, velocity_Jupiter

velocity_Earth = escape_velocity(mass_Earth,radius_Earth)
velocity_Moon = escape_velocity(mass_Moon,radius_Moon)
velocity_Jupiter = escape_velocity(mass_Jupiter,radius_Jupiter)

print *,"Plant   ","   escape velocity"
print *,"Earth   ",velocity_Earth
print *,"Moon    ",velocity_Moon
print *,"Jupiter ",velocity_Jupiter

end program EP0414
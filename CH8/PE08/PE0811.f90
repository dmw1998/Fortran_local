program PE0811
implicit none

type angles
integer :: degrees,minutes,seconds
end type angles

type(angles) :: angle
real :: deg_resl,rad_resl
real,parameter :: pi=3.14159236

print *,"Please input an angle xxx(degrees) yy(minutes) zz(seconds)."
read '(I3,1X,2(I2,1X))', angle%degrees,angle%minutes,angle%seconds

deg_resl = angle%degrees + 1.0/60.0*angle%minutes + 1.0/3600.0*angle%seconds
rad_resl = deg_resl/180.0*pi

print *,deg_resl,rad_resl

print '("The angle ",I3,"degrees ",I2,"minutes ",I2,"seconds is ",F8.4,"degrees or ",F4.2,"radians.")', &
    angle%degrees,angle%minutes,angle%seconds,deg_resl,rad_resl

end program PE0811

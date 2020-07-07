program PE0310
implicit none

real, parameter :: mortar = 0.5         ! units: inch
real, parameter :: brick_long = 9.0, brick_wide = 4.5, brick_high = 3        ! units: inch
real :: wall_high_ft, wall_high_in, wall_long_ft, wall_long_in
integer :: num_long, num_high, num_brick

print *,"Please input the height x ft y in of the wall with the form of 'x,y'."
read *, wall_high_ft, wall_high_in

print *,"Please input the length x ft y in of the wall with the form of 'x,y'."
read *, wall_long_ft, wall_long_in

num_high = (wall_high_ft*12.0 + wall_high_in)/(brick_high+mortar) + 1.0
num_long = (wall_long_ft*12.0 + wall_long_in)/(brick_long+mortar) + 1.0
num_brick = num_high*num_long

print *, num_high, num_long

print *,"We need at least ",num_brick," bricks."

end program PE0310
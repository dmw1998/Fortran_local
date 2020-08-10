program list_directed_input_example
implicit none

integer :: int_1, int_2, int_3
real :: real_1, real_2, real_3

! Initialize all variables to zero
int_1 = 0; int_2 = 0; int_3 = 0
real_1 = 0.0; real_2 = 0.0; real_3 = 0.0

! Read data
read *, int_1, real_1, int_2, real_2, int_3, real_3

! Print new values
print *, int_1, real_1, int_2, real_2, int_3, real_3

end program list_directed_input_example
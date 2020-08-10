program sawp
implicit none

real :: var_1, var_2, c

print *,"Please input two numbers you want to sawp"
read *, var_1, var_2

c = var_1
var_1 = var_2
var_2 = c

print *, var_1, var_2

end program sawp
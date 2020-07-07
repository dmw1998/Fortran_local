program swap
implicit none

real :: var_1 = 111.111, var_2 = 222.222, c     ! We need a third varible

! Exchange value
c = var_1       ! Save the value of var_1 on c
var_1 = var_2       ! Let var_1 = var_2, then they are 222.222
var_2 = c       ! Put the original var_1, which is c now, on var_2

! Print swapped value
print *, var_1, var_2

end program swap
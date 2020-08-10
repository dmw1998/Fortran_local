program example07__
implicit none

subroutine sub1
use global_data
implicit none

global_1 = pi       ! both accessed form module
print *,global_1    ! prints 3.14159
call sub2           ! global_1 now has the value 2.5
print *,global_1    ! prints 2.5
end subroutine sub1

subroutine sub2
use global_data
implicit none
global_1 = 2.5
end subroutine sub2

end program example07__
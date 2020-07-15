program PE0710
implicit none

type rider
    character(len=22) :: names
    integer :: race_num
    character :: race
    real :: time
end type rider

integer :: num_riders, i 
type(rider),dimension(:),allocatable :: riders
real :: ave_time

print *,"Please input the total number of riders."
read *, num_riders

allocate(riders(num_riders))
print *,"Please input the information of the riders"
print *,"in the order of name, race number and time."

do i=1,num_riders
    read *, riders(i)%names, riders(i)%race_num, riders(i)%time
end do 

ave_time = sum(riders%time)/num_riders

do i=1,num_riders
    if (riders(i)%time < ave_time) then
        riders(i)%race = "A"
    else
        riders(i)%race = "B"
    end if 
end do

print *, riders

end program PE0710
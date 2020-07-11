program PE0703
implicit none

integer,dimension(10) :: simple
integer :: i,j = 0

do i=1,10
    print *,"Please input a one for pass or a zero for fail."
    read *, simple(i)

    if (simple(i) == 1) then
        j = j + 1
    end if
    print *,"The percentage of the volunteers who past is",j*0.1

end do

end program PE0703
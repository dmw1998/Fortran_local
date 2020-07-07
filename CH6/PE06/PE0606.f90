program PE0606
implicit none

real, parameter :: load=2000, d2=2
real :: effort, d1
integer :: max_dis, min_dis, m 

print *,"Please input the maximun and minimum of the (integer) length of the fulcrum in metres."
read *, max_dis, min_dis

do m = min_dis, max_dis, 2
    d1 = m
    effort = load*d2/d1
    print *, effort,"kg effort will be required under length ",m,"metres."

enddo

end program PE0606
program test_15_1_3
implicit none

integer,dimension(0:6) :: power_of_two
integer :: i, j 

power_of_two = (/ (2**i, i=0,6) /)
do i = 0,6
    print '(I10,I10.6,B10.6,O10.6,Z10.6,G10.6)',   &
            (power_of_two(i), j=1,6)
end do

end program test_15_1_3
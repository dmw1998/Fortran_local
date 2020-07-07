program PE0604
implicit none

integer, parameter :: num_char = 128
integer :: i

do i = 0,num_char-1
    print *,i,char(i)
enddo

end program PE0604
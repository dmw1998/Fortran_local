program PE0602
implicit none

integer :: count,i 

print *,"Please input the count you want to start countdown."
read *, count

do i = count,1,-1
    ! Countdown to 1
    print *, i
    
enddo

print *,"Blast Off!"

end program PE0602
program multiplication_tables
implicit none

! A program to print multiplication tables form 2 to 12 times

! Variable declarations
integer :: i,j

! Outer loop defines which 'times table'
do i = 2,12
    print *," "
    print *,i," times table"
    do j = 1,i
        print *,i," times ",j," is ",i*j       
    end do  
end do

end program multiplication_tables
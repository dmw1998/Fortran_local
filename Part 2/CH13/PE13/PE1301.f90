program PE1301
implicit none

integer,dimension(4,5) :: array
integer :: i,j

do i = 1,4
    do j = 1,5
        array(i,j) = 10*i + j
    enddo
enddo

! Print in the regular form
do i = 1,4
    print *, array(i,:)
enddo

print *, " "

! Print in the form rotated 90 degree
do j = 1,5
    print *, array(:,j)
enddo

end program PE1301
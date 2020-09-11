module revese
implicit none
contains
    function revese_array(array)

        integer,dimension(:),intent(in) :: array
        integer,dimension(size(array)) :: revese_array

        integer :: i 
        integer,dimension(size(array)) :: subarr 

        do i = 1,size(array)
            subarr(i) = size(array)-i+1
        end do

        revese_array = array(subarr)
        
    end function revese_array
end module revese

program PE1307
use revese
implicit none

integer :: i, j, row
integer,dimension(6,7) :: array =       &
    reshape((/ ((10*i+j, i=1,6), j=1,7) /),(/ 6,7 /))

print *,"Which row is to be revesed (in range 1-7)?"
read *,row

select case(row)
case (:0)
    row = 1
    print *, "Out of range - row 1 will be reversed"
case (8:)
    row = 7
    print *, "Out of range - row 7 will be reversed"
end select

array(row,:) = revese_array(array(row,:))

print '(7I6)', ((array(i,j), j=1,7), i=1,6)

end program PE1307
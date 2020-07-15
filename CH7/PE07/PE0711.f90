module bubble_sort_m
contains
    subroutine bubble_sort(arr)
    implicit none

    character(len=*), dimension(:) ,intent(inout):: arr

    integer :: i,j
    character(len=len(arr(i))) :: chara

    do i=size(arr)-1,1,-1
        do j=1,i
            if (arr(j) > arr(j+1)) then
                chara = arr(j)
                arr(j) = arr(j+1)
                arr(j+1) = chara
            end if 
        end do
    end do 

    end subroutine bubble_sort

end module bubble_sort_m

program PE0711
use bubble_sort_m
implicit none

character(len=12), dimension(7):: array
integer :: i 

print *,"Please input the elements in the array."
do i=1,7
    read *,array(i)
end do 

call bubble_sort(array)

print *, array

end program PE0711
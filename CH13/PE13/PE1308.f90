module left_circular_shift
implicit none
contains
    function left_circular_shift_rank1(array,n)

        real,dimension(:),intent(in) :: array
        integer,intent(in) :: n
        real,dimension(size(array)) :: left_circular_shift_rank1

        integer,dimension(size(array)) :: subarr
        integer :: i, j=1

        do i = 1,size(array)-n
            subarr(i) = n+i
        end do

        do i = size(array)-n+1,size(array)
            subarr(i) = j
            j = j+1
        end do

        left_circular_shift_rank1 = array(subarr)
    end function left_circular_shift_rank1
end module left_circular_shift

program PE1308
use left_circular_shift
implicit none

integer :: i, n=6
real,dimension(13) :: array =           &
    reshape((/ (5*i+0.01*i, i=2,14) /), (/13/))
real,dimension(13) :: arr1,arr2
logical :: x = .true.

arr1 = left_circular_shift_rank1(array,n)
arr2 = cshift(array,n)

do i=1,size(array)
    if (arr1(i) /= arr2(i)) then
        x = .false.
        return
    end if
end do

if (x) then
    print *,"Agree"
else
    print *,"Disagree"
end if 

end program PE1308

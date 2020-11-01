program bubble_sort_program
    implicit none

    integer :: n        ! Size of the data set
    integer, dimension(:), allocatable :: array
    integer :: i 

    print *, "How many numbers are there?"
    read *, n

    allocate(array(n))

    print *, "Please input the numbers."
    read *, (array(i), i=1,n)

    call bubble_sort(n,array)

contains

subroutine bubble_sort(n,array)

    integer :: n, array(n)
    
    integer :: i, j, temp

    do i = n-1,1,-1
        do j = 1,i 
            if (array(j) > array(j+1)) then
                temp = array(j)
                array(j) = array(j+1)
                array(j+1) = temp
            end if
        end do
    end do

end subroutine bubble_sort

end program bubble_sort_program
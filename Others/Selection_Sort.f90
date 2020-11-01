program selection_sort_program
    implicit none

    integer :: n        ! Size of the data set
    integer, dimension(:), allocatable :: array
    integer :: i 

    print *, "How many numbers are there?"
    read *, n

    allocate(array(n))

    print *, "Please input the numbers."
    read *, (array(i), i=1,n)

    call selection_sort(n,array)

contains

subroutine selection_sort(n,array)

    integer :: n, array(n)

    integer :: i, j, min

    do i = 1,n 
        min = array(i)
        do j = i,n 
            if (min > array(j)) then
                min = array(j)
                array(j) = array(i)
                array(i) = min
            end if
        end do
    end do

end subroutine selection_sort
    
end program selection_sort_program
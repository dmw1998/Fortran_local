program shell_sort_program
    implicit none

    integer :: n        ! Size of the data set
    integer, dimension(:), allocatable :: array
    integer :: i 

    print *, "How many numbers are there?"
    read *, n

    allocate(array(n))

    print *, "Please input the numbers."
    read *, (array(i), i=1,n)

    call shell_sort(n,array)

contains

subroutine shell_sort(n,array)

    integer :: n, array(n)

    integer :: i, j, k, temp

    k = n/2

    do while (k > 0)
        do i = k+1,n 
            do while (j >0)

                ! If array(j) > array(j+k), then
                ! interchange their values and
                ! compare array(j-k) and array(j)

                if (array(j) > array(j+k)) then
                    temp = array(j)
                    array(j) = array(j+k)
                    array(j+k) = temp
                    j = j-k 
                else
                    exit
                end if

            end do
        end do

        k = k/2

    end do

end subroutine shell_sort

end program shell_sort_program
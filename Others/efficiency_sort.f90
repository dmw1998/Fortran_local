module sort_methods
    implicit none

    ! This module contains four common sorting methods.
    ! They are quick sort, shell sort, bubble sort and selection sort.
    
contains

recursive subroutine quick_sort(n,array,s,e)

    integer :: n, array(n)      ! array and its size
    integer :: s, e             ! start and end position

    integer :: l, r             ! left and right value
    integer :: k                ! key value = array(s)
    integer :: temp

    ! Given initial values for l and r
    l = s                       ! l start from the begining
    r = e + 1                   ! r start from the end

    ! Only when r > l it can be started
    if (r > l) return

    k = array(s)                ! Set the key value

    do
        ! Find array(l) < k
        do
            l = l+1
            if (array(l) > k .or. l >= e) exit
        end do

        ! Find array(r) > k
        do 
            r = r-1
            if (array(r) < k .or. r <= s) exit
        end do

        ! End loop if r <= l
        if (r <= l) exit

        ! Interchange array(l) and array(r)
        temp = array(r)
        array(l) = array(r)
        array(r) = temp

    end do

    ! Interchange array(s) and array(r)
    temp = array(s)
    array(s) = array(r)
    array(r) = temp

    ! quick sort the sequence befor r
    call quick_sort(n,array,s,r-1)

    ! quick sort the sequence after r
    call quick_sort(n,array,r+1,e)

end subroutine quick_sort

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
    
end module sort_methods

program test_efficiency
    use sort_methods
    implicit none

    integer, parameter :: n = 15000       ! Set data set has 15000 numbers
    integer, dimension(n) :: array
    real :: start_time, end_time, count
    real, dimension(n) :: temp
    integer :: i 

    call random_seed()
    
    count = 0
    do i = 1,100000
        call random_number(temp)
        array = int(temp*7)
        call cpu_time(start_time)
        call quick_sort(n,array,1,n)
        call cpu_time(end_time)
        count = count + (end_time - start_time)
    end do

    print '("Applying 10000 times of quick sort method costs"s,E14.6," seconds.")', count

    count = 0
    do i = 1,100
        call random_number(temp)
        array = int(temp*7)
        call cpu_time(start_time)
        call shell_sort(n,array)
        call cpu_time(end_time)
        count = count + (end_time - start_time)
    end do

    print '("Applying 100 times of shell sort method costs",E16.6,"seconds.")', count
    
    count = 0
    do i = 1,10
        call random_number(temp)
        array = int(temp*7)
        call cpu_time(start_time)
        call bubble_sort(n,array)
        call cpu_time(end_time)
        count = count + (end_time - start_time)
    end do

    print '("Applying 10 times of bubble sort method costs",E16.6,"seconds.")', count
    
    count = 0
    do i = 1,10
        call random_number(temp)
        array = int(temp*7)
        call cpu_time(start_time)
        call selection_sort(n,array)
        call cpu_time(end_time)
        count = count + (end_time - start_time)
    end do

    print '("Applying 10 times of selection sort method costs",E16.6,"seconds.")', count
    
    
end program test_efficiency
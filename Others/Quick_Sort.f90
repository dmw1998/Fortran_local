program quick_sort_program
    implicit none

    integer :: n        ! Size of the data set
    integer, dimension(:), allocatable :: array
    integer :: i 

    print *, "How many numbers are there?"
    read *, n

    allocate(array(n))

    print *, "Please input the numbers."
    read *, (array(i), i=1,n)

    call quick_sort(n,array,1,n)

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

    
end program quick_sort_program
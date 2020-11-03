program binary_searching
    implicit none
    
    ! The binary search is only for the ordering sequence

    integer, parameter :: n = 10
    integer, dimension(n) :: array
    integer :: key, index

    print *, "Which data do you want to find?"
    read *, key

    index = binary_search(key,n,array)

contains

integer function binary_search(key,n,array)

    integer :: key, n, array(n)

    integer :: l, r, m      ! The begining, end and mid-index of each subsequence

    l = 1 ; r = n 
    m = (l+r)/2

    ! key is not in the sequence(array)
    if (key < array(l) .or. key < array(r)) then
        binary_search = 0 
        return
    end if

    do while (l <= r)
        ! key is in the left-hand side subsequence
        if (key < array(m)) then
            l = m+1
            m = (l+r)/2
        ! key is in the right-hand side subsequence
        else if (key > array(m)) then
            r = m-1
            m = (l+r)/2
        else if (key == array(m)) then
            binary_search = m 
            return
        end if
    end do

    binary_search = 0

end function binary_search
    
end program binary_searching
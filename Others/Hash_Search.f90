program hash_searching
    implicit none

    ! This program search a data in the set
    ! {21, 53, 71, 19, 61, 81, 3, 17, 44, 93}

    integer, parameter :: n = 10
    integer, dimension(n) :: array = &
        (/ 21, 53, 71, 19, 61, 81, 3, 17, 44, 93 /)
    integer, dimension(1:100) :: hash_table
    integer :: key, i 

    print *, "Which data do you want to find?"
    read *, key

    if (key < 0 .or. key > 100) then
        print *, "Not found."
        stop
    end if

    ! Create a hash table
    hash_table = 0
    do i = 1,n 
        hash_table(array(i)) = i 
    end do

    ! Found data in the hash table
    if (hash_table(key) /= 0) then
        print *, hash_table(key), " ", key
    else
        print *, "Not found."
    end if
    
end program hash_searching
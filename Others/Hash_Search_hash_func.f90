program hash_searching
    implicit none

    integer, parameter :: n =10     ! Size of data set
    integer, dimension(n) :: array = &
        (/ 21, 53, 71, 19, 61, 81, 3, 17, 44, 93 /)
    integer, dimension(n) :: hash_table
    integer :: key, i, index, hash 

    hash(key) = key/10 + 1          ! hash function

    hash_table = 0
    do i = 1,n 
        index = hash(array(i))
        do
            if (hash_table(index) == 0) then
                hash_table(index) = i 
                exit
            else
                index = index+1
                if (index > n) index = 1
            end if
        end do
    end do

    print *, "Which data do you want to find?"
    read *, key

    index = hash(key)
    do i = 1,n 
        if (hash_table(index) == 0) then
            print *, "Not found."
            exit
        else if (array(hash_table(index)) == key) then
            print *, hash_table(index), " ", key
            exit
        else
            index = index+1
            if (index > n) index = 1
        end if
    end do

    if (i > n) print *, "Not found."
    
end program hash_searching
program sequential_searching
    implicit none

    integer, parameter :: n = 10
    integer, dimension(n) :: array
    integer :: key, index

    print *, "Which data do you want to find?"
    read *, key

    index = sequential_search(key,n,array)

contains

integer function sequential_search(key,n,array)

    integer :: key, n, array(n)
    
    integer :: i 

    do i = 1,n 
        if (array(i) == key) then
            sequential_search = i 
            return
        end if
    end do

end function sequential_search
    
end program sequential_searching
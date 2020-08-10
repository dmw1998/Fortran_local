subroutine alpha_sort(name)
implicit none 

! A subroutine to sort the content of the array name into alphabetic order
! THIS SUBROUTINE MUST HAVE AN EXPLICIT INTERFACE WHERE CALLED

! Dummy argument
character(len=*), dimension(:), intent(inout) :: name

! Local variables
character(len=len(name)) :: first,temp
integer :: number, index, i,j

! Set number to the number of names to be sorted
number = size(name)

! Loop to sort number-1 names into order
do i=1,number-1

    ! Initialize earliest so far to be the first in this pass
    first = name(i)
    index = i 

    ! Search remaining (unsorted items) for earliest one
    do j=i+1,number
        if (name(j)<first) then         ! An earlier one has been
            first = name(j)             ! found, so save it
            index = j                   ! and its position
        end if
    end do 

    if (index /= i) then                ! An earlier name was found
        temp = name(i)                  ! so exchange it with the
        name(i) = name(index)           ! 'head' of the list
        name(index) = temp
    end if 
    
end do

end subroutine alpha_sort
function max_array(array_1,array_2)
    implicit none

    ! This function returns the maximum of two arrays on an element by element basis

    ! Dummy arguments
    real,dimension(:) :: array_1,array_2

    ! Result variable
    real,dimension(size(array_1)) :: max_array

    ! Use the elemental instrinsic MAX to compare elements
    max_array = max(array_1,array_2)

end function max_array
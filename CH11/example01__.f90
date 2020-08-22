interface
    subroutine alpha_sort(name)
        implicit none
        character(len=*), dimension(:), intent(inout) :: name
    end subroutine alpha_sort
end interface
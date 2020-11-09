subroutine allocatable_space
    use work_space
    implicit none

    ! This subroutine allocates the array work at a size
    ! determined by the user during execution

    ! Local variable
    integer :: i, error

    ! Ask for required size for array
    do i = 1,3
        print *,"Please give maximumsize of file"
        read *, work_size

        ! Allocate array
        allocate(work(work_size+1), stat=error)
        if (error == 0) exit 

        ! Error dutring allocation - try again (max of 2 times)
        print *,"Space required not possible - try again"
    end do 

    ! Check to see if array was (finally) allocated
    if (.not. allocated(work)) then
    ! No llocation - even after three tries
        print *,"Three attempts to allocate without success!"
        stop
    end if

    ! Work array sucessfully allocated
end subroutine allocatable_space
module work_space
    implicit none
    save
    integer :: work_size
    real,allocatable,dimension(:) :: work
end module work_space

program flexible
    implicit none

    ! Allocate space for the array work
    call allocatable_space

    ! Carry out calculations using the  array work
    call calculate

end program flexible

subroutine allocatable_space
    use work_space
    implicit none

    ! This subroutine allocates the array work at a size
    ! determined by the user during execution

    ! Local variable
    integer :: error

    ! Ask for required size for array
    print *."Please give maximumsize of file"
    read *, work_size

    ! Allocate array
    allocate(work(work_size+1), stat=error)
    if (error /= 0) then
        ! Error dutring allocation - terminate processing
        print *,"Space required not possible"
        stop
    end if

    ! Work array sucessfully allocated
end subroutine allocatable_space

subroutine calculate
    use work_space
    implicit none

    ! Local variables
    integer :: i, n, min_p, max_p, open_error, io_stat
    real :: min, max 
    character(len=20) :: file_name

    ! Get name of data file
    print *,"Please give name of data file"
    read '(A)',file_name

    ! Open data file
    open(unit=7, file=file_name,status="old", &
         action="read", iostat=open_error)
    if (open_error /= 0) then
        print *,"Error during file opening"
        stop
    end if

    ! Read data until end of file
    do i = 1,work_size
        read (unit=7, fm=*, iostat=io_stat) work(i)
        ! Check for end of file
        if (io_stat < 0) exit
    end do

    ! Save number of numbers read
    n = i-1

    ! Find maximum and minimum values
    call minmax(n,min,max,min_p,max_p)

    ! Print details of minimum and maximum numbers
    print '(1X,"Minimum value is ",F15.4,    &
            " and occurs at position ",I10/  &
            1X,"Maximum value is ",F15.4,    &
            " and occurs at position ",I10)' &
            min,min_p,max,max_p
    
    ! Calculate number that are less than the mean
    call num_less_than_mean(n)

    ! Deallocate work array
    deallocate(work)

end subroutine calculate

subroutine minmax(n,minimum,maximum,min_pos,max_pos)
    use work_space
    implicit none

    ! This subroutine calculates the largest and smallest
    ! element values in work(1) to work(n)

    ! Dummy arguments
    integer,intent(in) :: n 
    real,intent(out) :: minimum, maximum
    integer,intent(out) :: min_pos, max_pos

    ! Local variable
    integer :: i 

    ! Establish initial values
    minimum = work(1)
    maximum = work(1)
    min_pos = 1
    max_pos = 1

    ! Loop to find maximum and minimum values
    do i = 2,n 
        if (work(i) < minimum) then 
            minimum = work(i)
            min_pos = i 
        else if (work(i) > maximum) then
            maximum = work(i)
            max_pos = i 
        end if
    end do

end subroutine minmax

subroutine nuk_less_than_mean(n)
    use work_space
    implicit none

    ! This subroutine calculates and prints the number of 
    ! elements of work(1) to work(n) that are less than
    ! the mean of all the numbers

    ! Dummy argument
    integer,intent(in) :: n 

    ! Local variables
    integer :: i, less = 0
    real :: sum = 0.0, mean

    ! Calaultae mean
    do i = 1,n
        sum = sum + work(i)
    end do
    mean = sum/n 

    ! Count number less than mean
    do i = 1,n
        if (work(i) < mean) less = less+1
    end do

    ! Print number below mean
    print '(1X,"There are ",I10,"numbers less than the &
            &mean of all numbers in the file")', less

end subroutine num_less_than_mean
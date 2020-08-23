real function mean(array,min_value,max_value)
    implicit none

    ! This function calculates the mean of the elements of array,
    ! ignoring any elements outside the range specified by the two 
    ! optional arguments

    ! Dummy arguments
    real,dimension(:),intent(in) :: array
    real,intent(in),optional :: min_value,max_value

    ! Local variables
    logical :: minimum_check,maximun_check
    integer :: i,count=0
    real :: sum_elems = 0.0

    ! Establish whether any limits are supplied
    minimum_check = present(min_value)
    maximun_check = present(max_value)

    ! Take different actions depending on whether any limits are set
    select case (minimum_check .or. maximun_check)
    case(.false.)
        ! No limits - use whole array processing
        sum_elems = sum(array)
        count = size(array)

    case (.true.)
        ! One or both limits specified - examine each element
        do i=lbound(array,1),ubound(array,1)
            ! Ignore element if below minimum value - if specified
            if (minimum_check .and. array(i)<min_value) cycle
            ! Ignore element if above maximum value - if specified
            if (maximun_check .and. array(i)>max_value) cycle

            ! Include this element in the calculation
            sum_elems = sum_elems + array(i)
            count = count + 1
        end do
    end select

    ! Calculate mean
    If (count>0) then
        mean = sum_elems/count
    else 
        print *,"No items in specified range - zero returned"
        mean = 0.0
    end if
    
end function mean 
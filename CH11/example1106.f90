program material_sort
implicit none

! This program reads a set of material names and densities
! and lists them in either alphabetical other or in order
! of their denities, either increasing or decreasing

! Declarations
integer,parameter :: max_length=20,max_number=100
character(len=max_length),dimension(max_number+1) :: material
real,dimension(max_number+1) :: density
integer :: number,i 
character :: sort_type

! Read data
call input

! Ask what type of sort is requied
do
    print *,"How do you wish this data to be sorted?"
    print *,"Type A for alphabetic order"
    print *,"Type D for order of decreasing density"
    print *,"Type I for order of increasing density"
    read *, sort_type
    select case (sort_type)
    case ("A","D","I")
        ! Valid reply - prepare to sort data
        exit
    case default
        ! Invalid reply - try again
        print *,"You must type A, D or I. Please try again"
    end select
end do

! Use approproate sort procedure to sort data
select case (sort_type)
case ("A")
    call alpha_sort
case ("D")
    call numeric_sort(up=.false.)
case ("I")
    call numeric_sort
end select

! List sorted data
print '("The materials and their densities are:"//  &
        (A15,F10.4/))', (material(i),density(i),i=1,number)

contains

subroutine input
    ! This subroutine reads the data into the array material
    ! and density, and stores the number of data item in
    ! number

    ! Local variable
    integer :: count

    print *,"Please type up to ",max_number," sets of data"
    print *,"Each set must consist of the name of the material"
    print *,"fllowed by its density"
    print *,"The final set must be followed by a zero"

    ! Loop to read data
    do count=1,max_number+1
        read *,material(count),density(count)
        if (density(count)==0.0) exit
    end do

    if (count>max_number) then 
        ! count>max_number, print warning
        print *,"More than ",max_number, &
                " data sets have been entered"
        print *,"Only the frist ",max_number," will be used"
        number = max_number
    else
        ! Set number to number of data sets read
        number = count-1
    end if
end subroutine input

subroutine alpha_sort
     ! This subroutine sorts the contents of the character array
     ! material into alphabetic order and also sorts the array
     ! density into the same order

     ! Local variables
     character(len=max_length) :: first,temp_name
     integer :: index,temp_num,i,j

     ! Loop to sort number-1 material names into order
     do i=1,number-1

        ! Initial lize earliest so far to be the first in 
        ! This pass
        first = material(i)
        index = i 

        ! Search remaining (unsorted item) for earliest one
        do j=i+1,number
            if (material(j) < first) then
                first = material(j)
                index = j
            end if
        end do

        ! Swap both material names and densities if necessary
        if (index /= i) then
            temp_name = material(i)
            material(i) = material(index)
            material(index) = temp_name
            temp_num = density(i)
            density(i) = density(index)
            density(index) = temp_num
        end if 
    end do 
end subroutine alpha_sort

subroutine numeric_sort(up)
    ! This subroutine sorts the contents of the real array
    ! density into numeric order and also sort the character
    ! array material into the same order. If the optional
    ! argument up has the value .FALSE. then density is sorted
    ! into decreaing order; otherwise it is sorted into 
    ! increasing order.

    ! Dummy argument
    logical,optional :: up

    ! Local variables
    character(len=max_length) :: temp_name
    integer :: index,first,temp_num,i,j

    ! Set up to .TURE. if not present as an actual argument
    if (.not.present(up)) up=.true.

    ! Loop to sort number-1 densities into order
    do i=1,number-1

        ! Initialize earliest so far to be the first in this pass
        first = density(i)
        index = i 

        ! Search remaining (unsorted items) for earliest one
        do j=i+1,number
            select case (up)
            case default
                ! Sorting in increasing order
                if (density (j) < first) then 
                    first = density(j)
                    index = j
                end if
            case (.false.)
                ! Sorting in decreasing order
                if (density(j) > first) then
                    first = density(j)
                    index = j
                end if
            end select
        end do

        ! Swap both densities and material names if necessary
        if (index /= i) then
            temp_num = density(i)
            density(i) = density(index)
            density(index) = temp_num
            temp_name = material(i)
            material(i) = material(index)
            material(index) = temp_name
        end if
    end do
end subroutine numeric_sort

end program material_sort
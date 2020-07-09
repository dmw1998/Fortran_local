module golf_details
implicit none
save
    type competitor
        integer :: index,score
    end type competitor

    type golfer
        character(len=15) :: first_name, last_name
        integer :: handicap,last_rounds(10)
    end type golfer

end module golf_details


function leaders(members,scores)
use golf_details
implicit none

    ! A function to determine the leading golfers in a golf club

    ! Result and argument declarations
    type(competitor),dimension(6) :: leaders
    type(golfer),dimension(:),intent(in) :: members
    integer,dimension(:),intent(in) :: scores

    ! Local variables
    integer,dimension(size(members)) :: total,member_index
    integer :: i, number
    number = size(members)

    ! Update golfers records and create array of aggreate
    ! scores for the last five rounds, allowing for handicaps
    call update(menbers,scores,number,total)

    ! Set up initial index array
    member_index = (/ (i,i=1,number) /)

    ! Sort index array to members and total into ascending 
    ! order of totals
    call sort(total,member_index,number)

    ! Indexes sorted, so return first six
    do i=1,6
        leaders(i)%index = member_index(i)
        ! Round average scores
        leaders(i)%score = real(total(member_index(i)))/5.0 + 0.5
    end do

end function leaders

subroutine update(members,scores,n,total)
    use golf_details
    implicit none

    ! This  subroutine updates the records of a set of golfers,
    ! and then creates an array of aggregate scores for their
    ! last five rounds, allowing for their individual handicaps

    ! Dummy arguments
    type(golfer),dimension(n),intent(in) :: members
    integer, dimension(n),intent(in) :: scores
    integer, dimension(in) :: n 
    integer,dimension(n),intent(out) :: total

    ! Local variable
    integer :: i 

    ! Move most recent 9 scores into last_rounds 2-10
    do i=9,1,-1
        members%last_rounds(i+1) = members%last_rounds(i)
    end do

    ! Insert latest scores into last_rounds(1)
    members%last_rounds(1) = scores

    ! Calculate aggregate scores allowing for handicap
    total = members%last_rounds(1) + &
            members%last_rounds(2) + &
            members%last_rounds(3) + &
            members%last_rounds(4) + &
            members%last_rounds(5) - 5*members%handicap

end subroutine update

subroutine sort(total,index,n)
    use golf_details
    implicit none

    ! This subroutine sorts an index array to the array total

    ! Dummy arguments
    integer,dimension(n),intent(inn) :: total
    integer,dimension(n),intent(inout) :: index
    integer,intent(in) :: n

    ! Local variables
    integer :: i,j,temp,first,i_first

    ! Sort index array into ascending order of aggregates scores
    do i=1,n-1

        ! Initialize lowset so far to be the first in this iteration
        first = total(index(i))
        i_first = i 

        ! Search remaining  unsorted items for lowest one
        do j=i+1,n
            if (total(index(j)) < first) then       ! A lower total has
                first = total(index(i))             ! been found, so
                i_first = j                         ! save it and index
            end if
        end do

        ! Exchange indexes if necessary
        if (i_first /= i) then 
            temp = index(i)
            index(i) = index(i_first)
            index(i_first) = temp
        end if 
        
    end do 

end subroutine sort

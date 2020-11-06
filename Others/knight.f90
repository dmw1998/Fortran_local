module stack_utility
    implicit none
    private

    integer, parameter :: top = 50
    integer, save :: current = 0
    integer, save :: stack(top)

    public :: push, pop 
    
contains

! Put data into stack
subroutine push(value)

    integer :: value

    if (current > top) then
        print *, "Stack full."
        return
    end if

    current = current+1
    stack(current) = value

end subroutine push

! Take data from stack
integer function pop(value)

    integer :: value

    if (current <= 0) then
        pop = 1
        return
    end if

    value = stack(current)
    current = current-1
    pop = 0

end function pop
    
end module stack_utility

program knight
    use stack_utility
    implicit none

    integer, parameter :: n = 5         ! Size of board
    integer :: board(5,5)               ! Record the board
    integer, parameter :: total = n*n   ! # of grids
    integer :: x_move(8), y_move(8)     ! Each knight has 8 choice
    integer :: x_pos, y_pos             ! Position on the board
    integer :: x_new, y_new             ! Next step
    integer :: move                     ! total movement
    integer :: step                     ! finished movement
    integer :: sol                      ! # of solutions
    integer :: error

    data x_move /1, 2, 2, 1, -1, -2,-2,-1/
    data y_move /2, 1,-1,-2, -2, -1, 1, 2/
    data board / total*0 /              ! Initialize
    data sol /0/

    ! Start from the center of board
    x_pos = (n+1)/2
    y_pos = (n+1)/2
    step = 1
    board(x_pos,y_pos) = step           ! The first step

    move = 1
    do
        do while (move <= 8)            ! Only 8 choice
            ! next step
            x_new = x_pos + x_move(move)
            y_new = y_pos + y_move(move)
            ! Check if it is still on the board
            if (x_new < 1 .or. x_new > n) then
                move = move+1
                cycle
            end if
            if (y_new < 1 .or. y_new > n) then
                move = move+1
                cycle
            end if

            ! Check if the next position is available
            if (board(x_new,y_new) == 0) then
                x_pos = x_new
                y_pos = y_new
                step = step+1
                board(x_pos,y_pos) = step
                call push(move)         ! push into stack
                move = 1
            else
                move = move+1           ! Change another way
            end if
        end do

        if (step == total) then
            sol = sol+1
            print '(I3," solution(s) exists. And they are")', sol 
            call show_board(board,n)
        end if

        ! Take back one step
        step = step-1
        ! step <= 0, there is no way to go back
        if (step <= 0) exit
        board(x_pos,y_pos) = 0
        error = pop(move)               ! Drop the last data from stack
        if (error /= 0) then
            print *,"Stack empty"
            stop
        end if

        ! Back one step
        x_pos = x_pos - x_move(move)
        y_pos = y_pos - y_move(move)

        ! Another way
        move = move +1

    end do

    print '(I3," solution(s) exists. And they are")', sol

contains

subroutine show_board(board,n)
    implicit none

    integer :: n, board(n,n)

    integer :: i
    character*(20) :: for = '(??(1X,I3))'
    
    write (for(2:3), '(I2)') n 
    do i = n,1,-1
        write(*,fmt=for) board(:,i)
    end do

end subroutine show_board
    
end program knight
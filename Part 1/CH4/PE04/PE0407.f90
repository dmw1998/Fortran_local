module type_brick
    implicit none
    type brick
    real :: height, length, price
    end type brick
end module type_brick

function numb(size_wall,size_brick)
    implicit none

    real, intent(in) :: size_wall,size_brick
    integer :: numb

    numb = size_wall/(size_brick+0.5)

end function numb

function cost(num, price)
    implicit none

    integer,intent(in)  :: num
    real,intent(in) :: price
    real ::  cost

    cost = num*price
        
end function cost

subroutine bricks(brick_i,wall_height, wall_length,num_brick,cost_brick)
    use type_brick
    implicit none

    ! Declare external function
    integer, external :: numb
    real, external :: cost

    type(brick), intent(in) :: brick_i
    real, intent(in) :: wall_height, wall_length
    integer, intent(out) :: num_brick 
    real, intent(out) :: cost_brick

    ! Calculate the number and cost of bricks
    num_brick = (numb(wall_height,brick_i%height)+1)*(numb(wall_length,brick_i%length)+1)
    cost_brick = cost(num_brick,brick_i%price)

    print *,num_brick,"bricks will be required and they cost $",cost_brick
 
end subroutine bricks

subroutine mortar(brick_i,mortar_i,wall_height, wall_length,num_mortar,cost_mortar)
    use type_brick
    implicit none

    ! Declare external function
    integer, external :: numb
    real, external :: cost

    type(brick), intent(in) :: brick_i
    real, intent(in) :: mortar_i, wall_height, wall_length
    integer, intent(out) :: num_mortar
    real, intent(out) :: cost_mortar

    ! Calculate the volume and cost of mortar (the wide is 4.5 by PE0310)
    num_mortar = 0.5*4.5*brick_i%height*(numb(wall_height,brick_i%height)) &
                + 0.5*4.5*brick_i%length*(numb(wall_length,brick_i%length))
    cost_mortar = cost(num_mortar,mortar_i)

    print *,num_mortar,"cube inch mortar will be required and they cost $",cost_mortar
 
end subroutine mortar

program PE0407
    use type_brick
    implicit none

    type(brick) :: brick_i
    real :: mortar_i
    real :: wall_height, wall_length, cost_brick, cost_mortar, cost_wall
    integer :: num_brick, num_mortar

    ! Collect data
    print *,"Please input the height, length and price of the brick."
    read *, brick_i
    print *,"Please input the price of the mortar."
    read *, mortar_i
    print *,"Please input the height and the length of the wall"
    read *, wall_height, wall_length

    call bricks(brick_i,wall_height, wall_length,num_brick,cost_brick)
    call mortar(brick_i,mortar_i,wall_height, wall_length,num_mortar,cost_mortar)
    cost_wall = cost_brick + cost_mortar
    print *,"The wall totally costs $",cost_wall

end program PE0407
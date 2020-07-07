program circle
    implicit none

    ! This program claculates the equation of a circle passing through three points

    ! Variable declarations
    REAL :: x1, y1, x2, y2, x3, y3, a, b, r
    
    ! Step 1
    PRINT *,"Please type the coordinates of three points"
    PRINT *,"in the order x1, y1, x2, y2, x3, y3"
    READ *,x1, y1, x2, y2, x3, y3   ! Read the three points

    ! Step 2
    CALL claculate_circle(x1, y1, x2, y2, x3, y3,a ,b, r)

    ! Step 3
    PRINT *,"The centre of the circle through these points is (",a,",",b,")"
    PRINT *,"Its radius is ",r

end program circle
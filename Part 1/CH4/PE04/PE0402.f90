function dis_bw_2pts(p1,p2)
    use point2d
    implicit none

    type(coord), intent(in) :: p1,p2
    real :: dis_bw_2pts

    ! Calculate the distance d between the two points
    dis_bw_2pts = sqrt((p2%x-p1%x)**2+(p2%y-p1%y)**2)

end function dis_bw_2pts

subroutine distances(p1,p2,d1,d2,d)
    ! This subroutine calculate the distences of two points from the origin and each other
    use point2d
    implicit none

    ! Declare external function
    real, external :: dis_bw_2pts

    ! Dummy declarations
    type(coord), intent(in) :: p1,p2
    real, intent(out) :: d1,d2,d

    ! Local dummy
    type(coord) :: p0

    p0%x = 0
    p0%y = 0

    ! Calculate and print the distance d1 of point 1 from the origin
    d1 = dis_bw_2pts(p0,p1)
    print *,"The distance d1 of point 1 from the origin is ",d1

    ! Calculate and print the distance d1 of point 2 from the origin
    d2 = dis_bw_2pts(p0,p2)
    print *,"The distance d2 of point 2 from the origin is ",d2

    ! Calculate and print the distance d between the two points
    d = dis_bw_2pts(p1,p2)
    print *,"The distance d between the two points is ",d

end subroutine distances

program PE0402
use point2d
implicit none
! Test the subroutine

! Variable declarations
type(coord) :: p1,p2
real :: d1,d2,d

! Read points
print *,"Please input the first piont."
read *, p1
print *,"Please input the second point."
read *, p2

! Calculate the distence
call distances(p1,p2,d1,d2,d)

end program PE0402
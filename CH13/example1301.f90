module convexity
    implicit none

    ! Derived type definition
    type point
        real :: x,y
    end type point

contains

    subroutine convex_polygon(polygon,convex)
        implicit none

        ! This subroutine determines wether a polygon is convex

        ! Dummy arguments
        type(point),dimension(:),intent(in) :: polygon
        logical,intent(out) :: convex

        ! Local variables
        real :: anti = 0.0
        integer :: i, n_vertices

        ! Set initial value for convex and obtain number of vertices
        convex = .true.
        n_vertices = size(polygon,1)    ! n_vertices is the number of vertices

        ! Get direction of rotation at first vertex
        if (orientation(polygon,1) > 0.0) then
            anti = 1.0
        else
            anti = -1.0
        end if

        ! Check direction of rotation at first vertex
        do i = 2,n_vertices
            if (anti*orientation(polygon,i) < 0.0) then
                ! Return immediately a different orientation occurs
                convex = .false.
                exit
            end if 
        end do

    end subroutine convex_polygon

    real function orientation(p,vertex)
        implicit none

        ! This function returns the direction of angular
        ! rotation at a specified vertex of a polygon
        ! positive if counterclockwise, negative if clockwise

        ! Dummy arguments
        type(point),dimension(:),intent(in) :: p 
        integer,intent(in) :: vertex

        ! Local variable
        integer :: n 

        n = size(p,1)       ! n is the number of vertices

        ! Calculate orientation at this vertex
        if (vertex == n-1) then
            orientation = (p(n)%x - p(n-1)%x) * (p(1)%y - p(n)%y) &
                        - (p(n)%y - p(n-1)%y) * (P(1)%x - p(n)%x)
        else if (vertex == n) then
            orientation = (p(1)%x - p(n)%x) * (p(2)%y - p(1)%y) &
                        - (p(1)%y - p(n)%y) * (P(2)%x - p(1)%x)
        else
            orientation = (p(vertex+1)%x - p(vertex)%x) &
                        * (p(vertex+2)%y - p(vertex+1)%y) &
                        - (p(vertex+1)%y - p(vertex)%y) &
                        * (P(vertex+2)%x - p(vertex+1)%x)
        end if

    end function orientation

end module convexity

program PE1301
use convexity
implicit none

! This program uses the module convexity to establish
! whether a set of points make a set of points make a convex polygon

integer,parameter :: number_of_points = 6
type(point),dimension(number_of_points) :: polygon,polygon1,polygon2
integer :: i 
logical :: convex,convex1,convex2

real,dimension(2*number_of_points) :: dataset1, dataset2

! Ask for six points
! do i = 1,number_of_points
!     print '(1X,"Given  vertex number ",I2)', i 
!     read *, polygon(i)%x, polygon(i)%y
! end do

dataset1 = (/ 0,0,1,0,2,1,2,2,1,3,-1,1 /)
dataset2 = (/ 1,1,3,1,2,2,3,3,1,3,0,2 /)

do i = 1,number_of_points
    polygon1(i)%x = dataset1(2*i-1)
    polygon1(i)%y = dataset1(2*i)
    polygon2(i)%x = dataset2(2*i-1)
    polygon2(i)%y = dataset2(2*i)
end do 

! Establish the polygon's convexity
! call convex_polygon(polygon,convex)
! if (convex) then
!     print *, "Polygon is convex."
! else
!     print *, "Polygon is not convex."
! end if

call convex_polygon(polygon1,convex1)
if (convex1) then
    print *, "Polygon1 is convex."
else
    print *, "Polygon1 is not convex."
end if

call convex_polygon(polygon2,convex2)
if (convex2) then
    print *, "Polygon2 is convex."
else
    print *, "Polygon2 is not convex."
end if

end program PE1301
subroutine outer(a,b,c)
    implicit none
    real,intent(inout) :: a,b,c     ! Dummy arguments
    real :: aa, bb, cc              ! Local variables
    ! Executable statement follow


contains
    subroutine inner(x,y,z)
        ! Note that implicit none is not allowed here
        ! as the implicit none is in the host is still in effect
        real,intent(inout) :: x,y,z     ! Dummy arguments
        real :: a,bb,xx,yy,zz           ! Local variables

        xx = x+y                        ! Assigns to xx in inner
        aa = y+z                        ! Assigns to aa in outer
        bb = z+x                        ! Assigns to bb in inner


    end subroutine inner
end subroutine outer
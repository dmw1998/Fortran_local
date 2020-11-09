module Universal_Constants
    implicit none

    real,parameter :: pi=3.1415926536

end module Universal_Constants

real function trig_fun_degrees(trig_fun,degrees,minutes,seconds)
    use Universal_Constants
    implicit none
    ! This function is a general trigonometry procedure for
    ! angles in degrees, minutes and seconds.

    ! Dummy arguments
    real, external :: trig_fun
    integer,intent(in) :: degrees, minutes, seconds

    ! Local variable
    real :: angle 

    ! Convert angle to radians
    angle =  (degrees + minutes/60.0 + seconds/3600.0)*pi/180.0

    ! Use supplied intrinsic to calculate required function
    trig_fun_degrees = trig_fun(angle)

end function trig_fun_degrees

program test_for_trig_fun_degrees
implicit none

    ! This program is a test program for trig_fun_degrees

    ! Declarations
    real, intrinsic :: sin, cos, tan
    real, external :: trig_fun_degrees
    integer :: degrees,mins,secs 
    character :: answer

    ! Loop to ask for an angle
    do 
        print *,"Please give an angle in degrees, minutes&
                & and seconds"
        print *,"without any fractional parts"
        print *,"Degrees: "
        read *, degrees
        print *,"Minutes (0-59): "
        read *, mins
        print *,"Seconds(0-59): "
        read *, secs

        ! Calculate and display its sin, cosine and tangent
        print *,"Its sine is ", &
                trig_fun_degrees(sin,degrees,mins,secs)
        print *,"Its cosine is ", &
                trig_fun_degrees(cos,degrees,mins,secs)
        print *,"Its tangent is ", &
                trig_fun_degrees(tan,degrees,mins,secs)

        ! Ask if another test is required
        print *,"Another one? (Y/N) "
        read *,answer
        if (answer/="Y" .and. answer/="y") exit

        ! If answer was Y or y then repeat the loop
    end do 

end program test_for_trig_fun_degrees
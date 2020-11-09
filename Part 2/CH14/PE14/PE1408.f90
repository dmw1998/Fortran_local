module polar_form
implicit none

    type polar
        real :: r, theta
    end type polar

contains

    real function find_r(z)

        complex,intent(in) :: z 

        find_r = sqrt(real(z)**2 + aimag(z)**2)

    end function find_r

    real function find_theta(z)

        complex,intent(in) :: z

        find_theta = atan(aimag(z)/real(z))

    end function find_theta

    subroutine find_polar(z,polar_z)

        complex,intent(in) :: z
        type(polar),intent(out) :: polar_z

        polar_z%r = find_r(z)
        polar_z%theta = find_theta(z)

    end subroutine find_polar

end module polar_form


program PE1408
use polar_form
implicit none

complex :: z=(1,1), w=(1,3)
type(polar) :: polar_z, polar_w, polar_z_times_w, polar_z_div_w, &
               polar_z_add_w, polar_z_minu_w, polar_2z, &
               polar_z_sq,polar_sqrz

call find_polar(z,polar_z)
call find_polar(w,polar_w)
call find_polar(z*w,polar_z_times_w)
call find_polar(z/w,polar_z_div_w)
call find_polar(z+w,polar_z_add_w)
call find_polar(z-w,polar_z_minu_w)
call find_polar(z**2,polar_z_sq)
call find_polar(sqrt(z),polar_sqrz)

print *," z = (", polar_z%r, ",", polar_z%theta," )"
print *," w = (", polar_w%r, ",", polar_w%theta," )"
print *," z*w = (", polar_z_times_w%r, ",", polar_z_times_w%theta," )"
print *," z/w = (", polar_z_div_w%r, ",", polar_z_div_w%theta," )"
print *," z+w = (", polar_z_add_w%r, ",", polar_z_add_w%theta," )"
print *," z-w = (", polar_z_minu_w%r, ",", polar_z_minu_w%theta," )"
print *," z**2 = (", polar_z_sq%r, ",", polar_z_sq%theta," )"
print *," sqrt(z) = (", polar_sqrz%r, ",", polar_sqrz%theta," )"

end program PE1408
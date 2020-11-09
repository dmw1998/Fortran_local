module complex_arithmetic
implicit none
private
public complex_number,operator(+),operator(-), &
       operator(*),operator(/)

    ! This module defines a complex number derived tyoe and
    ! extends the four intrinsic operators +,-,* and / to
    ! have complex operands

    type complex_number
        real :: real_part,imag_part
    end type complex_number

    interface operator(+)
        module procedure c_add
    end interface

    interface operator(-)
        module procedure c_sub
    end interface

    interface operator(*)
        module procedure c_mult
    end interface

    interface operator(/)
        module procedure c_div
    end interface

contains

    function c_add(z1,z2)

        ! Function result and arguments
        type(complex_number) :: c_add
        type(complex_number),intent(in) :: z1,z2

        ! Calculate function result
        c_add%real_part = z1%real_part + z2%real_part
        c_add%imag_part = z1%imag_part + z2%imag_part
    end function c_add

    function c_sub(z1,z2)

        ! Function result and arguments
        type(complex_number) :: c_sub
        type(complex_number),intent(in) :: z1,z2

        ! Calculate function result
        c_sub%real_part = z1%real_part - z2%real_part
        c_sub%imag_part = z1%imag_part - z2%imag_part
    end function c_sub

    function c_mult(z1,z2)

        ! Function result and arguments
        type(complex_number) :: c_mult
        type(complex_number),intent(in) :: z1,z2

        ! Calculate function result
        c_mult%real_part = z1%real_part*z2%real_part - &
                           z1%imag_part*z2%imag_part
        c_mult%imag_part = z1%real_part*z2%imag_part + &
                           z1%imag_part*z2%real_part
    end function c_mult

    function c_div(z1,z2)

        ! Function result and arguments
        type(complex_number) :: c_div
        type(complex_number),intent(in) :: z1,z2

        ! Local variable to save calculating denominator twice
        real :: denom

        ! Calculate function result
        denom = z2%real_part**2 + z2%imag_part**2
        c_div%real_part = (z1%real_part*z2%real_part + &
                           z1%imag_part*z2%imag_part) / denom
        c_div%imag_part = (z2%real_part*z1%imag_part + &
                           z1%real_part*z2%imag_part) / denom
    end function c_div

end module complex_arithmetic
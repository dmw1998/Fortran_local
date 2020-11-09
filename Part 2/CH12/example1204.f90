module rational_numbers
    implicit none
    private
    public :: rational, operator(+), operator(-), operator(*), &
              operator(/), operator(**), assignment(=),      &
              rational_convert, real, integer

! This module implements rational numbers as an additional numeric type

    ! Type definition
    type rational
        private
        integer :: num,denom
    end type rational

    ! Extended intrinsic operator specifications
    interface operator(+)
        module procedure real_real_plus_rat
        module procedure real_rat_plus_real
        module procedure rat_real_plus_rat
        module procedure rat_rat_plus_real
        module procedure int_int_plus_rat
        module procedure int_rat_plus_int
        module procedure rat_int_plus_rat
        module procedure rat_rat_plus_int
        module procedure rat_rat_plus_rat
    end interface

    interface operator(-)
        module procedure real_real_minus_rat
        module procedure real_rat_minus_real
        module procedure rat_real_minus_rat
        module procedure rat_rat_minus_real
        module procedure int_int_minus_rat
        module procedure int_rat_minus_int
        module procedure rat_int_minus_rat
        module procedure rat_rat_minus_int
        module procedure rat_rat_minus_rat
    end interface

    interface operator(*)
        module procedure real_real_times_rat
        module procedure real_rat_times_real
        module procedure rat_real_times_rat
        module procedure rat_rat_times_real
        module procedure int_int_times_rat
        module procedure int_rat_times_int
        module procedure rat_int_times_rat
        module procedure rat_rat_times_int
        module procedure rat_rat_times_rat
    end interface

    interface operator(/)
        module procedure real_real_div_rat
        module procedure real_rat_div_real
        module procedure rat_real_div_rat
        module procedure rat_rat_div_real
        module procedure int_int_div_rat
        module procedure int_rat_div_int
        module procedure rat_int_div_rat
        module procedure rat_rat_div_int
        module procedure rat_rat_div_rat
    end interface

    interface operator(**)
        module procedure real_real_sqr_rat
        module procedure real_rat_sqr_real
        module procedure rat_real_sqr_rat
        module procedure rat_rat_sqr_real
        module procedure int_int_sqr_rat
        module procedure int_rat_sqr_int
        module procedure rat_int_sqr_rat
        module procedure rat_rat_sqr_int
        module procedure rat_rat_sqr_rat
    end interface

    ! Extended assignment
    interface assignment(=)
        module procedure real_equals_rat
        module procedure rat_equals_real
        module procedure int_equals_rat
        module procedure rat_equals_int
        module procedure rat_equals_rat
    end interface

    ! Generic type conversion functions
    interface rational_convert
        module procedure real_to_rat
        module procedure int_to_rat
    end interface

    interface real
        module procedure rat_to_real
    end interface

    interface integer
        module procedure rat_to_int
    end interface

contains

    type(rational) function rat_rat_plus_int(rat_num,int_num)
    ! Adds an integer to a rational number to give a rational result

        ! Dummy arguments
        type(rational),intent(in) :: rat_num
        integer,intent(in) :: int_num

        ! Calculate result
        rat_rat_plus_int%num = rat_num%num + int_num*rat_num%denom
        rat_rat_plus_int%denom = rat_num%denom
    end function rat_rat_plus_int

    type(rational) function rat_int_plus_rat(int_num,rat_num)
    ! Adds a rational number to an integer to give a rational result

        ! Dummy arguments
        integer,intent(in) :: rat_num
        type(rational),intent(in) :: rat_num

        ! Calculate result
        rat_int_plus_rat%num = int_num*rat_num%denom + rat_num%num
        rat_int_plus_rat%denom = rat_num%denom
    end function rat_int_plus_rat



end module rational_numbers
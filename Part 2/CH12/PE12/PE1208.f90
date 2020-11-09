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
        integer :: num,denom        ! ?
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

    type(rational) function rat_real_plus_rat(rat_num,real_num)
    ! Adds a rational number to an real number to give a rational reasult

        ! Dummy arguments
        type(rational),intent(in) :: rat_num
        real,intent(in) :: real_num

        ! Local argument
        real :: real_num2,real_result

        ! Convert rational number to real for calculating
        real_num2 = rat_to_real(rat_num)

        real_result = real_num + real_num2

        ! Convert real number result to rational number
        rat_real_plus_rat = real_to_rat(real_result)
    end function rat_real_plus_rat

    real function rat_rat_plus_real()

    end function rat_rat_plus_real

    type(rational) function int_int_plus_rat()

    end function int_int_plus_rat

    integer function int_rat_plus_int()

    end function int_rat_plus_int

    type(rational) function rat_int_plus_rat()

    end function rat_int_plus_rat

    integer function rat_rat_plus_int()

    end function rat_rat_plus_int

    type(rational) function rat_rat_plus_rat()

    end function rat_rat_plus_rat

    type(rational) function real_real_minus_rat()

    end function real_real_minus_rat

    real function real_rat_minus_real()

    end function real_rat_minus_real

    type(rational) function rat_real_minus_rat()

    end function rat_real_minus_rat

    real function rat_rat_minus_real()

    end function rat_rat_minus_real

    type(rational) function int_int_minus_rat()

    end function int_int_minus_rat

    integer function int_rat_minus_int()

    end function int_rat_minus_int

    type(rational) function rat_int_minus_rat()

    end function rat_int_minus_rat

    integer function rat_rat_minus_int()

    end function rat_rat_minus_int

    type(rational) function rat_rat_minus_rat()

    end function rat_rat_minus_rat

    type(rational) function real_real_times_rat()

    end function real_real_times_rat

    real function real_rat_times_real()

    end function real_rat_times_real

    type(rational) function rat_real_times_rat()

    end function rat_real_times_rat

    real function rat_rat_times_real()

    end function rat_rat_times_real

    type(rational) function int_int_times_rat()

    end function int_int_times_rat

    integer function int_rat_times_int()

    end function int_rat_times_int

    type(rational) function rat_int_times_rat()

    end function rat_int_times_rat

    integer function rat_rat_times_int()

    end function rat_rat_times_int

    type(rational) function rat_rat_times_rat()

    end function rat_rat_times_rat

    type(rational) function real_real_div_rat()

    end function real_real_div_rat

    real function real_rat_div_real()

    end function real_rat_div_real

    type(rational) function rat_real_div_rat()

    end function rat_real_div_rat

    real function rat_rat_div_real()

    end function rat_rat_div_real

    type(rational) function int_int_div_rat()

    end function int_int_div_rat

    integer function int_rat_div_int()

    end function int_rat_div_int

    type(rational) function rat_int_div_rat()

    end function rat_int_div_rat

    integer function rat_rat_div_int()

    end function rat_rat_div_int

    type(rational) function rat_rat_div_rat()

    end function rat_rat_div_rat

    type(rational) function real_to_rat(real_num)
    ! This function convert real number to rational number

        ! Dummy argument
        real,intent(in) :: real_num

        ! Local argument
        character(len=:) :: real_char
        character(len=:) :: num_char
        character(len=:) :: denom_char

        write(real_char,'(F20)') real_num

        i_split = index(real_char,".")

        num_char = real_char(1,i_split-1)
        denom_char = real_char(i_split+1,len(real_char))

        read(num_char,'(I10)') real_to_rat%num
        read(denom_char,'(I10)') real_to_rat%denom
    end function real_to_rat

    type(rational) function int_to_rat(int_num)
    ! This function convert a integer to a rational number

        ! Dummy argument
        integer,intent(in) :: int_num

        int_to_rat%num = int_num
        int_to_rat%denom = 0
    end function int_to_rat

    real function rat_to_real(rat_num)
    ! This function convert rational number to real number

        type(rational),intent(in) :: rat_num

        rat_to_real = rat_num%num / rat_num%denom
    end function rat_to_real

    integer function rat_to_int(rat_num)
    ! This functino convert rational number to integer

        type(rational),intent(in) :: rat_num

        if (denom /= 0) then
            print *,"This rational number cannot convert to be an integer."
        else
            rat_to_int = rat_num%num
        end if
    end function rat_to_int

    
    


end module rational_numbers
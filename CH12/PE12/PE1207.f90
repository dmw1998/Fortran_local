module rational_numbers
    implicit none

    type number
        integer,private :: p, q         ! p/q
    end type number

contains

    type(number) function create_rational_numbers(a,b)

        integer,intent(in) :: a,b 

        create_rational_numbers%p = a
        create_rational_numbers%q = b
        
    end function create_rational_numbers

    integer function hcf(numb)

        ! Dummy argument
        type(number),intent(in) :: numb

        ! Local argument
        integer :: i 

        if (numb%p >= numb%q) then
            do i = numb%q,1,-1
                if (mod(numb%p,i) == 0 .and. mod(numb%q,i) == 0) then
                    hcf = i 
                    return
                end if
            end do 
        else 
            do i = numb%p,1,-1
                if (mod(numb%q,i) == 0 .and. mod(numb%p,i) == 0) then
                    hcf = i 
                    return
                end if
            end do
        end if
    end function hcf

    type(number) function simplify_frac(numb)

        ! Dummy argument
        type(number),intent(in) :: numb

        ! Local argument
        integer :: hcf_of_pq

        hcf_of_pq = hcf(numb)

        simplify_frac%p = numb%p / hcf_of_pq
        simplify_frac%q = numb%q / hcf_of_pq
    end function simplify_frac

    integer function give_numerator(numb)

        ! Dummy argument
        type(number),intent(in) :: numb

        give_numerator = numb%p
    end function give_numerator

    integer function give_denominator(numb)

        ! Dummy argument
        type(number),intent(in) :: numb

        give_denominator = numb%q
    end function give_denominator

end module rational_numbers

program PE1207
use rational_numbers
implicit none

    type(number) :: a, b

    a = create_rational_numbers(5,3)
    b = create_rational_numbers(60,84)

    a = simplify_frac(a)
    b = simplify_frac(b)

    print *, "The numerator of a is ", give_numerator(a)
    print *, "The denominator of a is ", give_denominator(a)

    print *, "The numerator of b is ", give_numerator(b)
    print *, "The denominator of b is ", give_denominator(b)

end program PE1207
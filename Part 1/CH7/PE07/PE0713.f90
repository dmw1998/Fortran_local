module XOR
    implicit none

    ! XOR calculator

    contains
    subroutine sub_xor(log1,log2,xor_resl)
    implicit none

    logical,intent(in) :: log1,log2
    logical,intent(out) :: xor_resl

    xor_resl = (log1 .or. log2) .and. (.not.(log1 .and. log2))

    end subroutine sub_xor

end module XOR

module add_2_binary
    use XOR
    implicit none

    ! Add two binary numbers

    contains
    subroutine add_bin1_bin2(binary1,binary2,sum_resl)
    implicit none

    logical,dimension(8),intent(in) :: binary1, binary2
    logical,dimension(8),intent(out) :: sum_resl

    integer :: i 
    logical,dimension(8) :: m
    logical :: co1,co2,ci=.false.        ! co stands for carry output, ci stands for carry input 
    
    do i=8,1,-1
        co1 = .false.
        co2 = .false.
        if (binary1(i) .and. binary2(i)) then
            co1 = .true.
        end if
        call sub_xor(binary1(i),binary2(i),m(i))

        if (ci) then
            if (m(i) .and. ci) then 
                co2 = .true.
            end if
        end if
        call sub_xor(m(i),ci,sum_resl(i)) 
        ci = co1 .or. co2
    end do

    end subroutine add_bin1_bin2

end module add_2_binary

module integer_logical
    implicit none

    ! Convert an integer to a logical

    contains
    subroutine int2log(int_input,log_output)
    integer,dimension(:),intent(in)  :: int_input
    logical,dimension(size(int_input)),intent(out) ::  log_output
    
    integer :: i 

    do i=1,size(int_input)
        if (int_input(i) == 0) then
            log_output(i) = .false.
        else
            log_output(i) = .true.
        end if
    end do
        
    end subroutine int2log

    subroutine log2int(log_input,int_output)
    logical,dimension(:),intent(in)  :: log_input
    integer,dimension(size(log_input)),intent(out) ::  int_output
    
    integer :: i 

    do i=1,size(log_input)
        if (log_input(i)) then
            int_output(i) = 1
        else
            int_output(i) = 0
        end if
    end do
        
    end subroutine log2int

end module integer_logical

program PE0713
use add_2_binary
use integer_logical
implicit none

integer,dimension(8) :: bin1 = (/1,0,1,1,0,0,1,1/)
integer,dimension(8) :: bin2 = (/0,0,1,1,1,0,1,0/),sum_int

logical,dimension(8) :: binary1, binary2, sum_log

call int2log(bin1,binary1)
call int2log(bin2,binary2)

call add_bin1_bin2(binary1,binary2,sum_log)

call log2int(sum_log,sum_int)

print *, sum_int

end program PE0713
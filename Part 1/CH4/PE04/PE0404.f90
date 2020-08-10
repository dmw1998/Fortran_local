module four_integer
    implicit none
    integer ::x1,x2,x3,x4
end module four_integer

subroutine input3
    use four_integer
    implicit none
    print *,"Please input three integers"
    read *, x1,x2,x3  
end subroutine input3

subroutine cal_sum
    use four_integer
    implicit none
    x4 = x1+x2+x3    
end subroutine cal_sum

subroutine output_sum
    use four_integer
    implicit none
    print *,"The sum of these three number is ",x4
end subroutine output_sum

program PE0404
    use four_integer
    implicit none

    call input3
    call cal_sum
    call output_sum

end program PE0404
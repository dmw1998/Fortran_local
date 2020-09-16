program cal_factorials
implicit none

! two byte integer
integer(kind = 2) :: shorter_factorial = 1

! four byte integer
integer(kind = 4) :: short_factorial = 1

! eight byte integer
integer(kind = 8) :: long_factorial = 1

! sixteen byte integer
integer(kind = 16) :: longer_factorial = 1

integer :: n2=0,n4=0,n8=0,n16=0,i = 1

! two byte integer factorial
    do while (shorter_factorial > 0)
        n2 = n2+1
        shorter_factorial = shorter_factorial*i 
        i = i+1
    end do 

    shorter_factorial = 1
    n2 = n2-1

    do i = 1,n2
        shorter_factorial = shorter_factorial*i
    end do

    print '("For two byte integer, the largest possible factorial is ",I2,"! =",I5)', n2, shorter_factorial

! four byte integer factorial
    i = 1
    do while (short_factorial > 0)
        n4 = n4+1
        short_factorial = short_factorial*i 
        i = i+1
    end do 

    short_factorial = 1
    n4 = n4-1

    do i = 1,n4
        short_factorial = short_factorial*i
    end do

    print '("For four byte integer, the largest possible factorial is ",I2,"! = ",I10)', n4, short_factorial

! eight byte integer factorial
    i = 1
    do while (long_factorial > 0)
        n8 = n8+1
        long_factorial = long_factorial*i 
        i = i+1
    end do 

    long_factorial = 1
    n8 = n8-1

    do i = 1,n8
        long_factorial = long_factorial*i
    end do

    print '("For eight byte integer, the largest possible factorial is ",I2,"! = ",I19)', n8, long_factorial

! sixteen byte integer factorial
    i = 1
    do while (longer_factorial > 0)
        n16 = n16+1
        longer_factorial = longer_factorial*i 
        i = i+1
    end do 

    longer_factorial = 1
    n16 = n16-1

    do i = 1,n16
        longer_factorial = longer_factorial*i
    end do

    print '("For sixteen byte integer, the largest possible factorial is ",I2,"! = ",I37)', n16, longer_factorial

end program cal_factorials
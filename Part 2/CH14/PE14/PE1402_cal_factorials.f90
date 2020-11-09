program cal_factorials
implicit none

! four byte real
real(kind = 4) :: shorter_factorial = 1

! eight byte real
real(kind = 8) :: short_factorial = 1

! ten byte real
real(kind = 10) :: long_factorial = 1

! sixteen byte real
real(kind = 16) :: longer_factorial = 1

integer :: n4=0,n8=0,n10=0,n16=0,i = 1

! four byte real factorial
    do while (shorter_factorial < 1.0E10)
        n4 = n4+1
        shorter_factorial = shorter_factorial*i 
        i = i+1
    end do 

    shorter_factorial = 1
    n4 = n4-1

    do i = 1,n4
        shorter_factorial = shorter_factorial*i
    end do

    print '("For four byte real, the largest possible factorial ",I2,"! is")', n4
    print *, shorter_factorial

! eight byte real factorial
    i = 1
    do while (short_factorial < 1.0E38)
        n8 = n8+1
        short_factorial = short_factorial*i 
        i = i+1
    end do 

    short_factorial = 1
    n8 = n8-1

    do i = 1,n8
        short_factorial = short_factorial*i
    end do

    print '("For eight byte real, the largest possible factorial ",I2,"! is")', n8
    print *, short_factorial

! ten byte real factorial
    i = 1
    do while (long_factorial < 1.0E22)
        n10 = n10+1
        long_factorial = long_factorial*i 
        i = i+1
    end do 

    long_factorial = 1
    n10 = n10-1

    do i = 1,n10
        long_factorial = long_factorial*i
    end do

    print '("For ten byte real, the largest possible factorial ",I2,"! is")', n10
    print *, long_factorial

! sixteen byte real factorial
    i = 1
    do while (longer_factorial < 1.0E22)
        n16 = n16+1
        longer_factorial = longer_factorial*i 
        i = i+1
    end do 

    longer_factorial = 1
    n16 = n16-1

    do i = 1,n16
        longer_factorial = longer_factorial*i
    end do

    print '("For sixteen byte real, the largest possible factorial ",I2,"! is")', n16
    print *, longer_factorial

end program cal_factorials
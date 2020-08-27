real function tax(income)
    implicit none

    real, intent(in) :: income

    ! u is the integer table of income limits 
    ! r is the integer table of tax rates
    ! These two tables are given as an array dummy variables

    integer,dimension(3) :: u = [600, 1500, 10000], r = [2, 6, 20]
    integer :: i 

    if (income < 0) then
        tax = 0
    else
        do i = size(u),1,-1
            if (income > u(i)) then
                tax = income * r(i) * 0.01
            end if
        end do
    end if

end function tax

program PE1103
implicit none

real, external :: tax

real :: income,taxv

print *,"Please input the income you want to calculate."
read *, income

print '("The tax is ",F9.3)', tax(income)

end program PE1103
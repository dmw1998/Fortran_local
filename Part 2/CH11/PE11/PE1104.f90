real function tax(income)
    implicit none

    real, intent(in) :: income

    ! u is the integer table of income limits 
    ! r is the integer table of tax rates
    ! These two tables are given as an array dummy variables

    integer,dimension(4) :: u = [0, 600, 1500, 10000], r = [0, 2, 6, 20]
    integer :: i, index = 0
    real :: taxx = 0.0

    if (income < 0) then
        taxx = 0.0

    else
        do i = size(u),1,-1
            if (income > u(i)) then
                index = i
                exit
            end if
        end do

        do i = 1,index-1
            taxx = taxx + (u(i+1) - u(i))*r(i)*0.01
        end do 

        taxx = taxx + (income - u(index))*r(index)*0.01

    end if

    tax = taxx

end function tax

program PE1104
implicit none

real, external :: tax

real :: income,taxv

print *,"Please input the income you want to calculate."
read *, income

taxv = tax(income)

print '("The tax is ",F9.3)', taxv

end program PE1104
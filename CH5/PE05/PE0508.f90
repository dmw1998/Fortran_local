program PE0508
implicit none

character(len=2) :: currency1, currency2
real :: amount1, amount2

print *,"Please in put the amount and the currency you want to convert."
print *,"You can input UK( pound) or US( dollar) for the currency."
read *, amount1, currency1

print *,"Please in put the currency you want to convert in."
read *, currency2

select case (currency1)
case ("UK")
    select case (currency2)
    case ("US")
        amount2 = amount1*1.52
    case ("De")
        amount2 = amount1*2.45
    case ("Fr")
        amount2 = amount1*8.60
    case ("Be")
        amount2 = amount1*52.65
    case default
        print *,"No target currency."
    end select
case ("US")
    select case (currency2)
    case ("Ja")
        amount2 = amount1*103.95
    case ("Sw")
        amount2 = amount1*1.40
    case ("Ca")
        amount2 = amount1*1.31
    case default
        print *,"No target currency."
    end select
end select

print *,"It is ",amount2

end program PE0508
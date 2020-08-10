module card_info
    implicit none
    save

    real :: outstanding_last_month, outstanding_now
    real :: interest_rate, interest
    real :: received_payment, total_payment
    real :: total_spent
    real :: amount_can_pay

end module card_info

subroutine collect_info(outstanding_last_month,received_payment,total_spent,interest_rate)
    implicit none

    real, intent(out) :: outstanding_last_month, received_payment, total_spent, interest_rate

    print *,"Please input the amount outstanding."
    read *, outstanding_last_month
    print *,"Please input the payment since the last statement"
    read *, received_payment
    print *,"Please input the total spent with the card since the last statement"
    read *, total_spent
    print *,"Please input the interest rate."
    read *, interest_rate

end subroutine collect_info

subroutine payments(total_payment, received_payment)
    implicit none

    real, intent(inout) :: total_payment, received_payment

    total_payment = total_payment + received_payment
    print *,"Your total payment is ",total_payment   
end subroutine payments

subroutine outstanding(interest,outstanding_last_month,interest_rate,outstanding_now,received_payment,total_spent)
    implicit none

    real, intent(inout) :: interest, outstanding_last_month, interest_rate, outstanding_now
    real, intent(inout) :: received_payment, total_spent

    interest = outstanding_last_month * interest_rate
    outstanding_now = outstanding_last_month + interest - received_payment + total_spent
    print *,"Your amount outstanding from last month is ",outstanding_last_month
    print *,"Your outstanding now is ",outstanding_now
end subroutine outstanding

subroutine can_pay(amount_can_pay,outstanding_now)
    implicit none
    real :: amount_can_pay, outstanding_now
    amount_can_pay = 0.05*outstanding_now
    print *,"Your can pay at least ",amount_can_pay   
end subroutine can_pay

program PE0405
    use card_info
    implicit none

    call collect_info(outstanding_last_month,received_payment,total_spent,interest_rate)
    call outstanding(interest,outstanding_last_month,interest_rate,outstanding_now,received_payment,total_spent)
    call payments(total_payment, received_payment)
    print *," Your total spent is",total_spent
    call can_pay(amount_can_pay,outstanding_now)

end program PE0405
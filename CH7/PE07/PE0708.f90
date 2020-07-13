program PE0708
implicit none

integer :: num,code,num_food=0,num_print=0,num_other=0
real :: price,food_cost=0,print_cost=0,other_cost=0

print *,"Please input the number, code and price of the goods."
print *,"Input '0 0 0' to end whenever you want to."

do 
    read *, num, code, price
    select case(code)
    case(0)
        exit
    case(1)
        num_food = num_food + num
        food_cost = food_cost + num*price
    case(2:4)
        num_print = num_print + num
        print_cost = print_cost + num*price
    case(9)
        num_other = num_other + num
        other_cost = other_cost + num*price
    end select
end do 

print *,num_food,"food purcheses cost",food_cost,". The State tax is",0.03*food_cost, &
        ". So the total cost is",1.03*food_cost
print *,num_print,"printed materials purcheses cost",print_cost,"The City sales tax is",0.05*print_cost, &
        ". So the total cost is",1.05*print_cost
print *,num_other,"other purcheses cost",other_cost,". The City sales tax is",0.05*other_cost, &
        ". The State tax is",0.03*other_cost,". So the total cost is",1.08*other_cost
print *,"So the total cost of the goods is",food_cost+print_cost+other_cost
print *,"The total City tax is",0.05*(print_cost+other_cost)
print *,"The total State tax is",0.03*(food_cost+other_cost)

end program PE0708
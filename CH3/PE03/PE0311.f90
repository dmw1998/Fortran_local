program PE0311
implicit none

integer :: total_pounds, total_pennies
integer :: pound20, pound10, pound5, pound1 
integer :: penny50, penny20, penny10, penny5, penny2, penny1
integer :: notes, coins

print *,"Please input the total amount ￡x yp in the form of 'x,y'"
read *, total_pounds, total_pennies

total_pennies = total_pounds*100 + total_pennies - 540      ! Turn the total amount into pennies and withdraw ￡5 and 40p
pound20 = total_pennies/(20*100)
pound10 = (total_pennies-pound20*20*100)/(10*100)
pound5 =  (total_pennies-pound20*20*100-pound10*10*100)/(5*100)
pound1 =  (total_pennies-pound20*20*100-pound10*10*100-pound5*5*100)/(100)
total_pennies = total_pennies-pound20*20*100-pound10*10*100-pound5*5*100-pound1*100
penny50 = total_pennies/50
penny20 = (total_pennies-penny50*50)/20
penny10 = (total_pennies-penny50*50-penny20*20)/10
penny5 = (total_pennies-penny50*50-penny20*20-penny10*10)/5
penny2 = (total_pennies-penny50*50-penny20*20-penny10*10-penny5*5)/2
penny1 = total_pennies-penny50*50-penny20*20-penny10*10-penny5*5-penny2*2

pound5 = pound5 + 1         ! ￡5
penny20 = penny20 + 2       ! 40p = 20p*2

notes = pound20+pound10+pound5
coins = pound1+penny50+penny20+penny10+penny5+penny2

print *,"The numbers of ￡20, ￡10 and ￡5 are ",pound20, pound10, " and ", pound5, &
        & ", in total ",notes, " notes required."
print *,"The numbers of ￡1, penny50, penny20, penny10, penny5, penny2 and penny1 are ", &
        & pound1, penny50, penny20, penny10, penny5, penny2," and ",penny1, &
        & ",in total ",coins," coins required."

end program PE0311
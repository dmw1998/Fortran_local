program PE0309
implicit none

type individual
character(len=20) :: last_name, first_name, sex, nation
character(3) :: age
character(len=22) :: occupation
character(len=11) :: phone_number
end type individual

type address
character(len=88) :: province, city, district, street
character(3) :: street_number
character(88) :: building
end type address

type(individual) :: individual_x
type(address) :: address_x

print *,"Please input your information in the order below"
print *,"name, sex, nation, age, occupation and phone number"
read *, individual_x
print *,"province, city, district, street(with number, separate by space), building name"
read *, address_x

print *,"My name is ",trim(individual_x%last_name)," ",trim(individual_x%first_name),"."
print *,"I am a ",trim(individual_x%age),"-year-old",trim(individual_x%sex)," ",trim(individual_x%occupation),"."
print *,"I live at ",trim(address_x%province)," Province ",trim(address_x%city)," city ", &
        & trim(address_x%district)," district ",trim(address_x%street)," ",trim(address_x%street_number), &
        & ",",trim(address_x%building)," building, and my phone number is ",individual_x%phone_number,"."

end program PE0309
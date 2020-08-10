program PE0313
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

type family
type(individual) :: father, mother, son, daughter
type(address) :: address_x
end type family

type(family) :: family_x

print *,"Please input the information of the father in the order below"
print *,"name, sex, nation, age, occupation and phone number"
read *, family_x%father

print *,"Please input the information of the mother in the order below"
print *,"name, sex, nation, age, occupation and phone number"
read *, family_x%mother

print *,"Please input the information of the son in the order below"
print *,"name, sex, nation, age, occupation and phone number"
read *, family_x%son

print *,"Please input the information of the daughter in the order below"
print *,"name, sex, nation, age, occupation and phone number"
read *, family_x%daughter

print *,"Please input the address of the family in the order below"
print *,"province, city, district, street(with number, separate by space), building name"
read *, family_x%address_x

print *,"The ",trim(family_x%father%last_name),"family live in",trim(family_x%address_x%province)," Province ", &
        & trim(family_x%address_x%city)," city ",trim(family_x%address_x%district)," district ", &
        & trim(family_x%address_x%street)," ",trim(family_x%address_x%street_number), &
        & ",",trim(family_x%address_x%building)," building."
print *,family_x%father%first_name," is ", family_x%father%age,"."
print *,"His wife ",family_x%mother%first_name," is ",family_x%mother%age
print *,"There duaghter ",family_x%daughter%first_name," is ",family_x%daughter%age," and their son ", &
        & family_x%son%first_name," is ",family_x%son%age,"."

end program PE0313
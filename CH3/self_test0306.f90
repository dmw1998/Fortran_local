type person
  character(len=20) :: first_name, last_name
  type(address) :: address
end type person

type(person) :: indivual

print *,"Please input your name and address in the order"
print *,"first name, last name, province, city"
pprint *,"district, street, number, building"
read *, indivual
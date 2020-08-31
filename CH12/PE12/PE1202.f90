module person_relation
    type person
        character(len=48) :: name
        character(len=12) :: sex        ! female, male etc.
        character(len=8) :: birthday             ! yyyymmdd
    end type person

    type family
        type(person) :: members
        character(len=20) :: mother,father,daughter,son
        character(len=20) :: husband,wife
    end type family

end module person_relation

program PE1202
use person_relation
implicit none

integer :: num1,num2
character(len=20) :: who, relation,printout

Lucy%members%name = "Lucy Jones"
Lucy%members%sex = "female"
Lucy%members%birthday = "19720911"
Lucy%daughter = "May Smith"
Lucy%husband = "Frances Smith"

Frances%members%name = "Frances Smith"
Frances%members%sex = "male"
Frances%members%birthday = "19730331"
Frances%daughter = "May Smith"
Frances%wife = "Lucy Jones"

May%members%name = "May Smith"
May%members%sex = "female"
May%members%birthday = "19990930"
May%father = "Frances Smith"
May%mother = "Lucy Jones"

print *,"Who do you want to ask about from the following list?"
print *,"1. Lucy Jones"
print *,"2. Frances Smith"
print *,"3. May Smith"
print *,"Type number of person:"
read *,num1

select case(num1)
case (1)
    who = "Lucy Jones"
case (2)
    who = "Frances Smith"
case (3)
    who = "May Smith"
end select

print *,"What do you want to know from the following list?"
print *,"1. Mother"
print *,"2. Father"
print *,"3. Daughter"
print *,"4. Husband"
print *,"5. Wife"
print *,"Type number of the item:"
read *,num2

select case(num2)
case (1)
    relation = "mother"
    if (num1 == 3) then
        printout = " is "//May%mother
    else
        printout = " does not exit"
    end if
case (2)
    relation = "father"
    if (num1 == 3) then
        printout = " is "//May%father
    else
        printout = " does not exit"
    end if
case (3)
    relation = "daughter"
    if (num1 == 1) then
        printout = " is "//Lucy%daughter
    else if (num1 == 2) then
        printout = " is "//Frances%daughter
    else
        printout = " does not exit"
    end if
case (4)
    relation = "husband"
    if (num1 == 1) then
        printout = " is "//Lucy%husband
    else
        printout = " does not exit"
    end if
case (5)
    relation = "wife"
    if (num1 == 2) then
        printout = " is "//Frances%wife
    else
        printout = " does not exit"
    end if
end select

print *," "
print *, trim(who),"'s ",trim(relation),trim(printout),"."

end program PE1202
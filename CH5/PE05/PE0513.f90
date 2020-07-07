module type_person
implicit none
save
    type person
        character(len=20) :: first_name, last_name
        integer :: age
        character :: sex
    end type person

end module type_person

subroutine rel(person_1,person_2,relation)
use type_person
implicit none

    type(person), intent(in) :: person_1,person_2
    integer, intent(out) :: relation

    if (person_1%last_name == person_2%last_name) then
        if ((person_1%age - person_2%age) >= 20) then
            if (person_1%sex == "M") then
                if (person_2%sex == "M") then
                    relation = 2
                else
                    relation = 3
                end if
            else
                if (person_2%sex == "M") then
                    relation = 4
                else
                    relation = 5
                end if
            end if
        else if ((person_2%age - person_1%age) >= 20) then
            if (person_1%sex == "M") then
                if (person_2%sex == "M") then
                    relation = -2
                else
                    relation = -4
                end if
            else
                if (person_2%sex == "M") then
                    relation = -3
                else
                    relation = -5
                end if
            end if
        else 
            if (person_1%age > 20 .and. person_2%age > 20) then
                if (person_1%sex == "M") then
                    relation = 1
                else
                    relation = -1
                end if
            else
                if (person_1%age > person_2%age) then
                    if (person_1%sex == "M") then
                        if (person_2%sex == "M") then
                            relation = 6
                        else
                            relation = 8
                        end if
                    else
                        if (person_2%sex == "M") then
                            relation = -8
                        else
                            relation = 7
                        end if
                    end if
                else
                    if (person_1%sex == "M") then
                        if (person_2%sex == "M") then
                            relation = -6
                        else
                            relation = -8
                        end if
                    else
                        if (person_2%sex == "M") then
                            relation = 8
                        else
                            relation = -7
                        end if
                    end if
                end if
            end if
        end if
    else
        relation = 0
    end if

end subroutine rel

program PE0513
use type_person
implicit none

type(person) :: person_1,person_2
integer :: relation

print *,"Please input the information of two persons"
print *,"in the order of first_name, last_name, age and sex(F or M)"
read *, person_1, person_2

call rel(person_1,person_2,relation)

select case(relation)
case (1)
    print *,trim(person_1%first_name)," ",trim(person_1%last_name)," is the &
            &husband of ",trim(person_2%first_name)," ",trim(person_2%last_name)
case (-1)
    print *,trim(person_2%first_name)," ",trim(person_2%last_name)," is the &
            &wife of ",trim(person_1%first_name)," ",trim(person_1%last_name)
case (2,3)
    print *,trim(person_1%first_name)," ",trim(person_1%last_name)," is the &
            &father of ",trim(person_2%first_name)," ",trim(person_2%last_name)
case (4,5)
    print *,trim(person_1%first_name)," ",trim(person_1%last_name)," is the &
            &mother of ",trim(person_2%first_name)," ",trim(person_2%last_name)
case (-2,-4)
    print *,trim(person_2%first_name)," ",trim(person_2%last_name)," is the &
            &son of ",trim(person_1%first_name)," ",trim(person_1%last_name)
case (-3,-5)
    print *,trim(person_2%first_name)," ",trim(person_2%last_name)," is the &
            &daughter of ",trim(person_1%first_name)," ",trim(person_1%last_name)
case (6,-6,8)
    print *,trim(person_1%first_name)," ",trim(person_1%last_name)," is the &
            &brother of ",trim(person_2%first_name)," ",trim(person_2%last_name)
case (7,-7,-8)
    print *,trim(person_2%first_name)," ",trim(person_2%last_name)," is the &
            &sister of ",trim(person_1%first_name)," ",trim(person_1%last_name)
case default
    print *,"They are unrelated."
end select

end program PE0513
subroutine trans
implicit none

character(len=22) :: name
real :: cup

    read *, name, cup

    select case (name)
    case ("flour")
        print *,cup," cup(s)"," ",trim(name)," is ",4*cup," oz in British."
    case ("butter")
        print *,cup," cup(s)"," ",trim(name)," is ",8*cup," oz in British."
    case ("sugar")
        print *,cup," cup(s)"," ",trim(name)," is ",6*cup," oz in British."
    case ("confectioner's sugar")
        print *,cup," cup(s)"," ",trim(name)," is ",4*cup," oz in British."
    case ("milk")
        print *,cup," cup(s)"," ",trim(name)," is ",8*cup," fl oz or ", &
                0.4*cup," pints in British."
    case ("exit")
        stop
    case default
        print *,cup," cup(s)"," ",trim(name)," is ",cup," cup(s) in British."
    end select
    
end subroutine trans


program PE0607
implicit none

print *,"This program can help you convert the US units into the British units."
print *,'You can keep input one kind of the ingredients and its number of cups. &
            &And you can type "exit 0" when you finish.'

do
    call trans
    
enddo

end program PE0607
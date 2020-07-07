program examination_statistics
implicit none

! This program calculates some simple examination statistics

! Constant and variable declarations
character, parameter :: male="M",female="F"
integer, parameter :: max_pupils=100
integer :: student,mark,num_pupils,num_boys=0,num_girls=0, &
           total_marks,marks_boys=0,marks_girls=0
character :: code

! Read at most max_pupils sets of data
print *,"Type up to ",max_pupils," exam results."
print *,"Each result must consist fo the student number, &
        &the mark, and a code"
print *,"The code is F for a female student and M for a male"
print *,"Data should be ended by a zero student number and &
        &mark, followed by any code other than M or F"

do num_pupils = 1,max_pupils
    ! Read next mark and code
    read *, student,mark,code

    ! Select appropriate action
    select case (code)

    ! Female pupil
    case ("F")
        num_girls = num_girls+1
        marks_girls = marks_girls+mark

    ! Male pupil
    case ("M")
        num_boys = num_boys+1
        marks_boys = marks_boys+mark

    ! End of data
    case ("x")
        exit
    ! Invalid code
    case default
        print *,"Invalid code - please re-enter data"
        cycle
    end select    
enddo

! Adjust num_pupils to correct number
num_pupils = num_pupils-1

! Calculate total marks
total_marks = marks_boys + marks_girls

! Calculate and print averages
if (num_pupils == 0) then
    print *,"There was no data!"
else
    ! Deal with no terminator case
    if (num_pupils == max_pupils) then
        print *,max_pupils,"sets of data read without a &
                &terminating record"
        print *,"Reaults are based on these puppils only"
    end if
    print *,"There are ",num_pupils," pupils. Their average &
            &mark is ",real(total_marks)/num_pupils
    if (num_girls > 0) then
        print *,"There are ",num_girls," girls. Their average &
                &mark is ",real(marks_girls)/num_girls
    else
        print *,"There are no girls in the class"
    end if
    if (num_boys > 0) then
        print *,"There are ",num_boys," boys. Their average &
                &mark is ",real(marks_boys)/num_boys
    else
        print *,"There are no boys in the class"
    end if
end if

end program examination_statistics
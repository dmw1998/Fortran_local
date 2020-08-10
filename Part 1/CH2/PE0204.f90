program final_test      ! No blank in name
!
implicit none       ! Wrong form. No need ::

REAL :: var_1, var_2, var_3, var_4      ! Wrong forms

PRINT *,"Please type four numbers &
        &separated by commas"       ! Missed *, upper line and missed & under line
READ *, var_1, var_2, var_3, var_4      ! Missed *, and wrong varible forms

! Now print the number to check that they were
! input correctly
PRINT *,"The numbers you typed were:"       ! Missed "
PRINT *, var_1, var_2, var_3, var_4     ! Varible forms
PRINT *,"That's all for now. &
        & How many errors did you find?"      ! Wrong ""

end program final_test
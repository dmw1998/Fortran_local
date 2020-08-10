program PE0306
implicit none

! Read a six word sentence, one word a time, into six variables

character(len=20) ::  w1, w2, w3, w4, w5, w6

print *,"Please input your sentence"
read *, w1, w2, w3, w4, w5, w6

! Print the sentence formed by concatenating the six variables

print *,TRIM(w1)//TRIM(w2)//TRIM(w3)//TRIM(w4)//TRIM(w5)//TRIM(w6)

end program PE0306
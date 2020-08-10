program PE0307
implicit none

character(len=880) :: sentence
integer :: num1,num2,num3,num4,num5,num6
character :: w1,w2,w3,w4,w5,w6

print *,"Please input your six word sentence"
read *, sentence

num1 = index(sentence," ")-1
w1 = sentence(:num1)
sentence = sentence(num1+2:)

num2 = index(sentence," ")-1
w2 = sentence(:num2)
sentence = sentence(num2+2:)


print *,w1,w2

end program PE0307
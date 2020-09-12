program PE1310
implicit none

character(10) :: filename = "data.txt"
integer,dimension(:,:),allocatable :: foxes, badgers, squirrels
character(10),dimension(:,:),allocatable :: highest_pop, lowest_pop
integer :: m, n, i, j, stat = 0
integer :: sum_foxes, sum_badgers, sum_squirrels
integer,dimension(3) :: hightest_foxes,hightest_badgers,hightest_squirrels
integer,dimension(3) :: lowest_foxes,lowest_badgers,lowest_squirrels

! Read data from a file and save into corresponding arrays
open (15,file=filename,status='old',form='formated')
read(15,*),m 
read(15,*),n 

allocate(foxes(m,n),badgers(m,n),squirrels(m,n))

read(15,120) i,j 
120 format '(I2,X1,I2)' i, j

read(15,150,iostat=stat) foxes(i,j),badgers(i,j),squirrels(i,j)
150 format '(X5,I2,X1,I2,X7,I2,X7,I2)'

close(15)

! Find sum of each matrix
sum_foxes = sum(foxes)
sum_badgers = sum(badgers)
sum_squirrels = sum(squirrels)

! Find region(s) with the hightest and lowest population of each type od animal
hightest_foxes(3) = maxval(foxes)
hightest_badgers(3) = maxval(badgers)
hightest_squirrels(3) = maxval(squirrels)

lowtest_foxes(3) = minval(foxes)
lowtest_badgers(3) = minval(badgers)
lowtest_squirrels(3) = minval(squirrels)

do i = 1,m
    do = j = 1,n 
        if (foxes(i,j) == hightest_foxes(3)) then
            hightest_foxes(1) = i
            hightest_foxes(2) = j
        end if 
        if (badgers(i,j) == hightest_badgers(3)) then
            hightest_badgers(1) = i
            hightest_badgers(2) = j
        end if 
        if (squirrels(i,j) == hightest_squirrels(3)) then
            hightest_squirrels(1) = i
            hightest_squirrels(2) = j
        end if 

        if (foxes(i,j) == lowtest_foxes(3)) then
            lowtest_foxes(1) = i
            lowtest_foxes(2) = j
        end if 
        if (badgers(i,j) == lowtest_badgers(3)) then
            lowtest_badgers(1) = i
            lowtest_badgers(2) = j
        end if 
        if (squirrels(i,j) == lowtest_squirrels(3)) then
            lowtest_squirrels(1) = i
            lowtest_squirrels(2) = j
        end if 
    end do
end do

! Find highest and lowest population of each region
allocate(highest_pop(m,n),lowest_pop(m,n))

do i = 1,m 
    do j = 1,n 
        if (foxes(i,j)>badgers(i,j) .and. foxes(i,j)>squirrels(i,j)) then
            highest_pop(i,j) = 'foxes'
            if (badgers(i,j) > squirrels(i,j)) then
                lowest_pop(i,j) = 'squirrels'
            else
                lowest_pop(i,j) = 'badgers'
            end if 
        elseif (badgers(i,j)>squirrels(i,j) .and. badgers(i,j)>squirrels(i,j)) then
            highest_pop(i,j) = 'badgers'
            if (squirrels(i,j) > foxes(i,j)) then
                lowest_pop(i,j) = 'foxes'
            else
                lowest_pop(i,j) = 'squirrels'
            end if
        elseif (squirrels(i,j)>foxes(i,j) .and. squirrels(i,j)>badgers(i,j)) then
            highest_pop(i,j) = 'squirrels'
            if (foxes(i,j) > badgers(i,j)) then
                lowest_pop(i,j) = 'badgers'
            else
                lowest_pop(i,j) = 'foxes'
            end if
        end if
    end do
end do

end program PE1310
module vectors
    implicit none

    ! Maximum length for vactors
    integer,parameter :: max_length = 10 

    ! Derived type definition
    type vector
        private
        integer :: length
        real,dimension(max_length) :: elements
    end type vector

contains

    type(vector) function creat_vector(array,n)
    ! This function creates a vector from the first n elements of an array

        ! Dummy aarguments
        integer,intent(in) :: n
        real,dimension(n),intent(in) :: array

        ! Validity check
        if (n > max_length) then
            ! Too long - print warning and set length to zero
            print '(" Error: Vector of length",I5," requested"/ &
                    " Maximum permitted is ",I3)', n, max_length
            creat_vector%length = 0
        else
            ! OK - copy first n elements of array to vector
            creat_vector%length = n
            creat_vector%elements(1:n) = array(1:n)
        end if
    end function creat_vector

    integer function vector_size(v)
    ! This function returns the size of a vetor

        ! Dummy argument
        type(vector),intent(in) :: v 

        vector_size = v%length
    end function vector_size

    function vector_array(v)
    ! This function returns the elements of a vector as an array

         ! Dummy argument and function result
         type(vector),intent(in) :: v 
         real,dimension(v%length) :: vector_array(v%length)

         vector_array(1:v%length) = v%elements(1:v%length)
    end function vector_array

    real function scalar_product(v1,v2)
    ! This function returns the scalar product of two vectors

        ! Dummy arguments
        type(vector),intent(in) :: v1, v2

        ! Local variables
        real :: dot_product = 0.0
        integer :: i 

        ! Validity check
        if (v1%length /= v2%length) then
            ! Vectors have different lengths
            print '(" Error Vectors are of different lengths", &
                    I4," and ",I4 / " Zero result returned")', &
                    v1%length, v2%length
            scalar_product = 0.0
        else
            ! OK - calculate dot product
            do i = 1,v1%length
                dot_product = dot_product +     &
                              v1%elements(i)*v2%elements(i)
            end do
            scalar_product = dot_product
        end if
    end function scalar_product

end module vectors
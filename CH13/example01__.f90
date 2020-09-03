program vectors_and_matrices
implicit none
integer,dimension(2,3) :: matrix_a
integer,dimension(3,2) :: matrix_b
integer,dimension(2,2) :: matrix_ab
integer,dimension(2) :: vector_c = (/1,2/)
integer,dimension(3) :: vector_bc

! Initialize matrix_a
matrix_a(1,1) = 1       ! matrix_a is the matrix:
matrix_a(1,2) = 2
matrix_a(1,3) = 3       ! [1 2 3]
matrix_a(2,1) = 2       ! [2 3 4]
matrix_a(2,2) = 3
matrix_a(2,3) = 4

! Set matrix_b as the transpose of matrix_a
matrix_b = transpose(matrix_a)
! matrix_b is now the matrix:  [1 2]
!                              [2 3]
!                              [3 4]

! Calculate matrix products
matrix_ab = matmul(matrix_a,matrix_b)
! matrix_ab is now the matrix:  [14 20]
!                               [20 29]

vector_bc = matmul(matrix_b,vector_c)
! vector_bc is now the vactor:  [5 8 11]

print *, matrix_a
print *, matrix_b
print *, matrix_ab
print *, vector_bc

end program vectors_and_matrices
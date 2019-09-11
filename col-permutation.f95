program packed_matrix
    type packed 
        integer, allocatable :: len_col, col_start, row_index
        real, allocatable :: value
    end type


!     matr(1,:) = (/  1.,  2.,  0.,  0.,  5./)
! 	matr(2,:) = (/  0.,  0., -3.,  4.,  0./)
! 	matr(3,:) = (/  0., -2.,  0.,  0., -5./)
! 	matr(4,:) = (/ -1.,  0.,  0., -4.,  0./)
! 	matr(5,:) = (/  0.,  3.,  0.,  0.,  6./)
end program

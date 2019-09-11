program packed_matrix
    implicit none
    type col_packed 
        integer, allocatable :: len_col(:), col_start(:), row_index(:)
        real, allocatable :: value(:)
    end type
    
    real :: A(5,5)
    type(col_packed) :: x
    A(1,:) = (/  1.,  2.,  0.,  0.,  5./)
	A(2,:) = (/  0.,  0., -3.,  4.,  0./)
	A(3,:) = (/  0., -2.,  0.,  0., -5./)
	A(4,:) = (/ -1.,  0.,  0., -4.,  0./)
	A(5,:) = (/  0.,  3.,  0.,  0.,  6./)
	x = packing(A)
	print*, x%value
	print*, x%col_start
	
    contains
    
    function packing(A)
        implicit none
        real :: A(:,:)
        type(col_packed) :: packing
        integer :: i, j, k, n, m, l, r
        n = size(A(:,1))
        allocate(packing%len_col(n), packing%col_start(n+1))
        packing%col_start(1) = 1
        do j = 1, n
            k = 0
            do i = 1, n
                if(A(i,j) /= 0.) then
                    k = k+1
                end if
                packing%len_col(j) = k
                packing%col_start(j+1) = packing%len_col(j) + packing%col_start(j)
            end do
        end do
        m = packing%col_start(n+1)
        allocate(packing%row_index(m-1), packing%value(m-1))
        k = 0
        do j = 1, n
             do i = 1, n
                if(A(i,j) /= 0.) then
                    k = k+1
                    packing%row_index(k) = i
                    packing%value(k) = A(i,j)
                end if
             end do
        end do 
    end function
    
    function permutation(A,j1,j2)
        implicit none
        type(col_packed) :: permutation, A
        integer :: j1, j2, i, j, k, m, n
        real, allocatable :: auxiliar(:)
        
    end function
    
end program

  

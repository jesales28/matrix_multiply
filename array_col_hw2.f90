! Driver    Julia Sales
! Navigator Anthony Overton
! Group 44-3, 10/17/2019
! HW2
! array_col_hw2
! change n to big and small numbers to see run times
!
! Compilation Lines:
! make -f Makefile4
!   optimization with -O3 -fdefault-real-8 -fdefault-double-8
! make -f Makefile5
!   optimization with -O0 -fdefault-real-8 -fdefault-double-8
! make -f Makefile6
!   Debugging flags:  -g3 -fdefault-real-8 -fdefault-double-8 
!   -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g 
!   -fcheck=all -fbacktrace


program array_col_hw2
    implicit none
    integer, parameter :: n = 4097
    integer, parameter :: r = n, c = n
    real (kind=8), dimension(r,c) :: A
    real (kind=8), dimension(r,c) :: D
    real (kind=8), dimension(r,c) :: C1
    real (kind=8), dimension(r,c) :: C2
    integer :: i, j, k
    integer :: count = 0

    ! First do-loop
    ! Initialize Matrix A and D
    do j=1, n
        do i=1, n
            A(i,j) = i+j
            if(i == j) then 
                D(i,j) = i+j
            else
                D(i,j) = 0
            endif
             !print *, ((A(i,j)))
             !print *, ((D(i,j)))
        end do
    end do

    ! Second do-loop
    ! Multiply the two matrices
    do j=1, n
        do i=1, n
            k = i+1
            C1(i,j) = C1(i,j) + A(j,k-1) * D(k-1,i)
            C2(i,j) = C2(i,j) + D(i,k-1) * A(k-1,j)
            !print *, ((C1(j,i)))
            !print *, ((C2(i,j)))
        end do
    end do 

    ! Third do-loop
    ! Compare C1 and C2 to see how many non zeros there are
    do j=1, n
        do i=1, n
            if((C1(j,i)-C2(i,j)) /= 0) then
                count = count+1
            endif
        end do
    end do
    !print *, (count)

    end program array_col_hw2
    
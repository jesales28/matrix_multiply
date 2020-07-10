! Driver    Julia Sales
! Navigator Anthony Overton
! Group 44-3, 10/17/2019
! HW2
! array_row_hw2
! change n to big and small numbers to see run times
!
! Compilation Lines:
! make -f Makefile1
!   optimization with -O3 -fdefault-real-8 -fdefault-double-8
! make -f Makefile2
!   optimization with -O0 -fdefault-real-8 -fdefault-double-8
! make -f Makefile3
!   Debugging flags:  -g3 -fdefault-real-8 -fdefault-double-8 
!   -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g 
!   -fcheck=all -fbacktrace

program array_row_hw2
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
    do i=1, n
        do j=1, n
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
    do i=1, n
        do j=1, n
            k = j+1
            C1(i,j) = C1(i,j) + A(i,k-1) * D(k-1,j)
            C2(i,j) = C2(i,j) + D(k-1,j) * A(i,k-1)
            !print *, ((C1(i,j)))
            !print *, ((C2(i,j)))
        end do
    end do 

    ! Third do-loop
    ! Compare C1 and C2 to see how many non zeros there are
    do i=1, n
        do j=1, n
            if((C1(i,j)-C2(j,i)) /= 0) then
                count = count+1
            endif
        end do
    end do
    !print *, (count)

    end program array_row_hw2
    
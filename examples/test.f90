! #############################################################################
! This example uses the `dict` object to insert and retrieve key value pairs
! using the C++ std::map .
!
! Compilation command:
! gfortran -g -Wall -Wextra -Werror -std=f2008 -lstdc++ test.f90 cmap.o fmap.o -o test
!
! #############################################################################
program test
    use iso_c_binding
    use fmap
    implicit none
    double precision:: a(3)
    double precision:: b(3)
    double precision:: c(3)
    double precision:: f(3)
    type(dict):: D

    a = [1.d0,1.d0,1.d0]
    b = [2.d0,2.d0,2.d0]
    c = [3.d0,3.d0,3.d0]
    write(*,*)'Check if dictionary exists: ',D%exists()
    write(*,*)'Call init ...'
    call D%init()
    write(*,*)'Check if dictionary exists: ',D%exists()
    write(*,*)
    write(*,*)'Check if dictionary is empty: ',D%empty()
    write(*,*)'Add a vector to it ...'
    f = [1.d0,5.d0,3.d0]
    call D%add('mytag',f)
    write(*,*)'Check if dictionary is empty: ',D%empty()
    write(*,*)
    write(*,*)'Retrieve vector from map ...'
    write(*,*)D%get('mytag')
    write(*,*)
    write(*,*)'Change the vector in Fortran ...'
    f(2) = 15.d0
    write(*,*)'Retrieve same vector from map ...'
    write(*,*)D%get('mytag')
    write(*,*)
    call D%add('var_a',a)
    call D%add('var_b',b)
    call D%add('var_c',c)
    write(*,*)'Get var by fake index on map ...'
    write(*,*)D%getVarName(2)
    write(*,*)
    write(*,*)'Deallocating dictionary ...'
    call D%destroy()
    write(*,*)'Check if dictionary exists: ',D%exists()




end program test
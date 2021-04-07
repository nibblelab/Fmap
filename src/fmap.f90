! #############################################################################
! This module handles the communication between C++ and Fortran in a 
! object named `dict`.
! This objects contains a C++ pointer to a std::map. The methods in `dict`
! pass this pointer to C++ and call std::map methods to obtain the desired
! results.
!
! Compilation command:
! gfortran -c -g -Wall -Wextra -Werror -std=f2008 fmap.f90
!
! #############################################################################
module fmap
    use iso_c_binding
    
    implicit none

    ! ------------------------
    ! Data Structure to Map in C++
    ! This has to mach `typedef struct vec` in cmap.cpp
    type, bind(c):: c_vec
        type(c_ptr):: data
        integer(c_int):: size
    end type

    type, bind(c):: c_string
        type(c_ptr):: str
        integer(c_int):: size
    end type
    ! ------------------------
    
    ! ------------------------
    ! Dictionary access in Fortran
    ! This class save the map pointer in Fortran
    ! and use methods to opperate it in C++
    type:: dict
        type(c_ptr), private:: map = c_null_ptr
        contains
            procedure:: init
            procedure:: add
            procedure:: get
            procedure:: getVarName
            procedure:: empty
            procedure:: exists
            procedure:: destroy
    end type
    ! ------------------------

    ! ------------------------
    ! C++ functions to be accessed in Fortran
    ! like using `extern` in C++
    interface
        subroutine mapInit(map) bind(c,name="mapInit")
            use iso_c_binding
            implicit none
            !Entrada:
            type(c_ptr):: map
        end subroutine mapInit

        subroutine mapAdd(map,name,n,val) bind(c,name="mapAdd")
            use iso_c_binding
            implicit none
            !Entrada:
            type(c_ptr), value:: map
            character(len=1,kind=c_char):: name(*)
            integer(c_int), value:: n
            real(c_double), dimension(n):: val
        end subroutine mapAdd

        function mapGet(map,name) result(val) bind(c,name="mapGet")
            use iso_c_binding
            import c_vec
            implicit none
            !Entrada:
            type(c_ptr), value:: map
            character(len=1,kind=c_char):: name(*)
            !Saida:
            type(c_vec):: val
        end function mapGet

        function mapGetVarNameByIndex(map,i) result(val) bind(c,name="mapGetVarNameByIndex")
            use iso_c_binding
            import c_string
            implicit none
            !Entrada:
            type(c_ptr), value:: map
            integer(c_int), value:: i
            type(c_string):: val
        end function mapGetVarNameByIndex

        function mapIsEmpty(map) result(isit) bind(c,name="mapIsEmpty")
            use iso_c_binding
            implicit none
            !Entrada:
            type(c_ptr), value:: map
            !Saida:
            logical(c_bool):: isit
        end function mapIsEmpty

        subroutine mapClear(map) bind(c,name="mapClear")
            use iso_c_binding
            implicit none
            !Entrada:
            type(c_ptr), value:: map
        end subroutine mapClear

        subroutine mapDestroy(map) bind(c,name="mapDestroy")
            use iso_c_binding
            implicit none
            !Entrada:
            type(c_ptr), value:: map
        end subroutine mapDestroy
    end interface
    ! ------------------------

    !#####################################
    !            Module Usage:
    !#####################################
    private
    !The following subroutines/variables are the only
    !ones accessible outside this module
    public:: dict
    !-------------------------------------

contains

! -----------------------------------------------------------------------------
! Initialize mapping 
subroutine init(this)
    implicit none
    !Entrada:
    class(dict), intent(inout):: this
    !Local:
    
    call mapInit(this%map)
    
end subroutine init
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
! Check if mapping is empty
function empty(this) result(isit)
    implicit none
    !Entrada:
    class(dict), intent(inout):: this
    !Saida:
    logical(c_bool):: isit
    !Local:
    
    isit = mapIsEmpty(this%map)
    
end function empty
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
! Check if mapping was created in C++
function exists(this) result(doesit)
    implicit none
    !Entrada:
    class(dict), intent(inout):: this
    !Saida:
    logical:: doesit
    !Local:
    
    doesit = c_associated(this%map)
    
end function exists
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
! Insert an key and vector pair in mapping
subroutine add(this,key,val)
    implicit none
    !Entrada:
    class(dict), intent(inout):: this
    character(len=*,kind=c_char), intent(in):: key
    real(c_double), intent(in):: val(:)
    !Local:

    call mapAdd(this%map,key//c_null_char,size(val),val)

end subroutine add
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
! Get a vector from a key in mapping 
function get(this,key) result(val)
    implicit none
    !Entrada:
    class(dict), intent(inout):: this
    character(len=*), intent(in):: key
    !Saida:
    real(c_double), pointer:: val(:)
    !Local:
    type(c_vec):: result

    result = mapGet(this%map,key//c_null_char)

    call c_f_pointer(result%data, val, shape=[result%size])

end function get
! -----------------------------------------------------------------------------



! ------------------------------------------------------------------------------
! Insipired from amrex_string_c_to_f

function string_c_to_f (cstr) result(fstr)
    character(kind=c_char), intent(in) :: cstr(:)
    character(len=size(cstr)-1) :: fstr
    integer :: i, n
    n = size(cstr)-1   ! skip the null character
    fstr = ""
    do i = 1, n
       if (cstr(i) == c_null_char) exit
       fstr(i:i) = cstr(i)
    enddo
  end function string_c_to_f

! -----------------------------------------------------------------------------
! Get a key from a fake index in mapping 
function getVarName(this,indx) result(val)
    implicit none
    !Entrada:
    class(dict), intent(inout):: this
    integer(c_int), intent(in):: indx
    !Saida:
    character(len=:), allocatable:: val
    !Local:
    type(c_string):: result
    character(kind=c_char), pointer :: cc(:)

    result = mapGetVarNameByIndex(this%map,indx)
    allocate(character(len=result%size-1)::val)
    call c_f_pointer(result%str, cc, [result%size])
    val = string_c_to_f(cc)
end function getVarName
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
! Clear the dictionary and free the pointer
subroutine destroy(this)
    implicit none
    !Entrada:
    class(dict), intent(inout):: this
    !Local:
    
    call mapClear(this%map)
    call mapDestroy(this%map)
    this%map = c_null_ptr
    
end subroutine destroy
! -----------------------------------------------------------------------------

end module fmap
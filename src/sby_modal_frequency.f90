!===============================================================================
! sby_modal_frequency.f90
!===============================================================================
!
! Native core for modal-frequency statistics by encoded column.
!
! R converts each supported column to compact positive integer codes. This
! Fortran routine receives only those integer codes and computes, for every
! column, the modal code, modal count, and modal ratio. Filtering is performed in
! R so the same native kernel can be reused by different public functions.
!
!===============================================================================

module sby_modal_frequency_core_mod
  use iso_c_binding
  implicit none

  integer(c_int), parameter :: INTSXP  = 13
  integer(c_int), parameter :: REALSXP = 14
  integer(c_int), parameter :: VECSXP  = 19

contains

  pure subroutine fill_zero_int(x, n)
    integer(c_int), intent(inout) :: x(:)
    integer(c_int), intent(in)    :: n
    integer(c_int)                :: i

    do i = 1, n
      x(i) = 0_c_int
    end do
  end subroutine fill_zero_int

  pure subroutine column_mode_from_codes(codes, n, max_code, counts, mode_code, mode_count)
    integer(c_int), intent(in)    :: codes(:)
    integer(c_int), intent(in)    :: n
    integer(c_int), intent(in)    :: max_code
    integer(c_int), intent(inout) :: counts(:)
    integer(c_int), intent(out)   :: mode_code
    integer(c_int), intent(out)   :: mode_count

    integer(c_int) :: i
    integer(c_int) :: code

    call fill_zero_int(counts, max_code)

    do i = 1, n
      code = codes(i)
      if (code >= 1_c_int .and. code <= max_code) then
        counts(code) = counts(code) + 1_c_int
      end if
    end do

    mode_code  = 1_c_int
    mode_count = counts(1)

    do code = 2_c_int, max_code
      if (counts(code) > mode_count) then
        mode_count = counts(code)
        mode_code  = code
      end if
    end do
  end subroutine column_mode_from_codes

end module sby_modal_frequency_core_mod

function sby_modal_frequency_codes_fortran(codes_list_sexp, max_codes_sexp) &
    result(result_sexp) bind(C, name="sby_modal_frequency_codes_fortran")

  use iso_c_binding
  use omp_lib
  use sby_modal_frequency_core_mod
  implicit none

  interface
    function Rf_length(x) bind(C, name="Rf_length") result(n)
      use iso_c_binding
      type(c_ptr), value :: x
      integer(c_int)     :: n
    end function Rf_length

    function VECTOR_ELT(x, i) bind(C, name="VECTOR_ELT") result(elt)
      use iso_c_binding
      type(c_ptr), value    :: x
      integer(c_int), value :: i
      type(c_ptr)           :: elt
    end function VECTOR_ELT

    subroutine SET_VECTOR_ELT(x, i, value) bind(C, name="SET_VECTOR_ELT")
      use iso_c_binding
      type(c_ptr), value    :: x
      integer(c_int), value :: i
      type(c_ptr), value    :: value
    end subroutine SET_VECTOR_ELT

    function INTEGER(x) bind(C, name="INTEGER") result(p)
      use iso_c_binding
      type(c_ptr), value :: x
      type(c_ptr)        :: p
    end function INTEGER

    function R_REAL(x) bind(C, name="REAL") result(p)
      use iso_c_binding
      type(c_ptr), value :: x
      type(c_ptr)        :: p
    end function R_REAL

    function Rf_allocVector(sexp_type, n) bind(C, name="Rf_allocVector") result(p)
      use iso_c_binding
      integer(c_int), value :: sexp_type
      integer(c_int), value :: n
      type(c_ptr)           :: p
    end function Rf_allocVector

    function Rf_protect(x) bind(C, name="Rf_protect") result(p)
      use iso_c_binding
      type(c_ptr), value :: x
      type(c_ptr)        :: p
    end function Rf_protect

    subroutine Rf_unprotect(n) bind(C, name="Rf_unprotect")
      use iso_c_binding
      integer(c_int), value :: n
    end subroutine Rf_unprotect
  end interface

  type(c_ptr), value :: codes_list_sexp
  type(c_ptr), value :: max_codes_sexp
  type(c_ptr)        :: result_sexp

  type(c_ptr) :: col_sexp
  type(c_ptr) :: out_column_sexp
  type(c_ptr) :: out_ratio_sexp
  type(c_ptr) :: out_code_sexp
  type(c_ptr) :: out_count_sexp

  type(c_ptr), allocatable :: column_ptr(:)
  integer(c_int), allocatable :: column_n(:)
  integer(c_int), pointer :: codes(:)
  integer(c_int), pointer :: max_codes(:)
  integer(c_int), pointer :: out_column(:)
  integer(c_int), pointer :: out_code(:)
  integer(c_int), pointer :: out_count(:)
  integer(c_int), allocatable :: counts(:)
  real(c_double), pointer :: out_ratio(:)

  integer(c_int) :: ncols
  integer(c_int) :: total_size
  integer(c_int) :: n
  integer(c_int) :: j
  integer(c_int) :: max_code
  integer(c_int) :: mode_code
  integer(c_int) :: mode_count
  real(c_double) :: ratio
  integer(c_int), parameter :: openmp_min_columns = 4_c_int
  integer(c_int), parameter :: openmp_min_cells = 20000_c_int

  ncols = Rf_length(codes_list_sexp)
  total_size = 0_c_int

  result_sexp     = Rf_protect(Rf_allocVector(VECSXP, 4_c_int))
  out_column_sexp = Rf_protect(Rf_allocVector(INTSXP,  ncols))
  out_ratio_sexp  = Rf_protect(Rf_allocVector(REALSXP, ncols))
  out_code_sexp   = Rf_protect(Rf_allocVector(INTSXP,  ncols))
  out_count_sexp  = Rf_protect(Rf_allocVector(INTSXP,  ncols))

  call c_f_pointer(INTEGER(out_column_sexp), out_column, [ncols])
  call c_f_pointer(R_REAL(out_ratio_sexp),   out_ratio,  [ncols])
  call c_f_pointer(INTEGER(out_code_sexp),   out_code,   [ncols])
  call c_f_pointer(INTEGER(out_count_sexp),  out_count,  [ncols])

  if (ncols > 0_c_int) then
    call c_f_pointer(INTEGER(max_codes_sexp), max_codes, [ncols])

    allocate(column_ptr(ncols))
    allocate(column_n(ncols))

    do j = 1_c_int, ncols
      col_sexp      = VECTOR_ELT(codes_list_sexp, j - 1_c_int)
      column_n(j)   = Rf_length(col_sexp)
      total_size    = total_size + column_n(j)
      column_ptr(j) = INTEGER(col_sexp)
      out_column(j) = j
      out_ratio(j)  = 0.0_c_double
      out_code(j)   = 0_c_int
      out_count(j)  = 0_c_int
    end do

!$omp parallel default(none) &
!$omp shared(ncols, total_size, column_ptr, column_n, max_codes, out_ratio, out_code, out_count) &
!$omp private(j, n, max_code, codes, counts, mode_code, mode_count, ratio) &
!$omp if(ncols >= openmp_min_columns .and. total_size >= openmp_min_cells)
!$omp do schedule(static)
    do j = 1_c_int, ncols
      n        = column_n(j)
      max_code = max_codes(j)

      if (n > 0_c_int .and. max_code > 0_c_int) then
        call c_f_pointer(column_ptr(j), codes, [n])

        if (.not. allocated(counts)) then
          allocate(counts(max_code))
        else if (size(counts) < max_code) then
          deallocate(counts)
          allocate(counts(max_code))
        end if

        call column_mode_from_codes(codes, n, max_code, counts, mode_code, mode_count)
        ratio = real(mode_count, c_double) / real(n, c_double)

        out_ratio(j) = ratio
        out_code(j)  = mode_code
        out_count(j) = mode_count
      end if
    end do
!$omp end do

    if (allocated(counts)) deallocate(counts)
!$omp end parallel

    deallocate(column_ptr)
    deallocate(column_n)
  end if

  call SET_VECTOR_ELT(result_sexp, 0_c_int, out_column_sexp)
  call SET_VECTOR_ELT(result_sexp, 1_c_int, out_ratio_sexp)
  call SET_VECTOR_ELT(result_sexp, 2_c_int, out_code_sexp)
  call SET_VECTOR_ELT(result_sexp, 3_c_int, out_count_sexp)

  call Rf_unprotect(5_c_int)
end function sby_modal_frequency_codes_fortran

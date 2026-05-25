!===============================================================================
! sby_correlation_pearson.f90
!===============================================================================
!
! Native core for absolute Pearson correlation by column pair.
!
! R passes a double matrix in column-major order. This routine returns a dense
! p-by-p absolute correlation matrix as a numeric vector. The diagonal is zero.
! Non-finite values are handled pairwise by using only rows that are finite in
! both columns.
!
!===============================================================================

module sby_correlation_pearson_core_mod
  use iso_c_binding
  use, intrinsic :: ieee_arithmetic
  implicit none

  integer(c_int), parameter :: REALSXP = 14

contains

  pure logical function is_valid_double(x)
    real(c_double), intent(in) :: x
    is_valid_double = ieee_is_finite(x)
  end function is_valid_double

  pure subroutine column_mean_ss(mat, n, col, mean, ss)
    real(c_double), intent(in)  :: mat(:)
    integer(c_int), intent(in)  :: n
    integer(c_int), intent(in)  :: col
    real(c_double), intent(out) :: mean
    real(c_double), intent(out) :: ss

    integer(c_int) :: row
    integer(c_int) :: offset
    real(c_double) :: x
    real(c_double) :: delta

    mean = 0.0_c_double
    ss = 0.0_c_double
    offset = (col - 1_c_int) * n

    do row = 1_c_int, n
      x = mat(offset + row)
      delta = x - mean
      mean = mean + delta / real(row, c_double)
      ss = ss + delta * (x - mean)
    end do
  end subroutine column_mean_ss

  pure subroutine pearson_abs_complete_pair(mat, n, col_i, col_j, means, ss_cols, corr)
    real(c_double), intent(in)  :: mat(:)
    integer(c_int), intent(in)  :: n
    integer(c_int), intent(in)  :: col_i
    integer(c_int), intent(in)  :: col_j
    real(c_double), intent(in)  :: means(:)
    real(c_double), intent(in)  :: ss_cols(:)
    real(c_double), intent(out) :: corr

    integer(c_int) :: row
    integer(c_int) :: offset_i
    integer(c_int) :: offset_j
    real(c_double) :: cov_acc
    real(c_double) :: denom

    cov_acc = 0.0_c_double
    offset_i = (col_i - 1_c_int) * n
    offset_j = (col_j - 1_c_int) * n

!$omp simd reduction(+:cov_acc)
    do row = 1_c_int, n
      cov_acc = cov_acc + (mat(offset_i + row) - means(col_i)) * (mat(offset_j + row) - means(col_j))
    end do

    denom = sqrt(ss_cols(col_i) * ss_cols(col_j))
    if (n < 2_c_int .or. denom <= 0.0_c_double .or. .not. ieee_is_finite(denom)) then
      corr = 0.0_c_double
    else
      corr = abs(cov_acc / denom)
      if (.not. ieee_is_finite(corr)) corr = 0.0_c_double
      if (corr > 1.0_c_double) corr = 1.0_c_double
    end if
  end subroutine pearson_abs_complete_pair

  pure subroutine pearson_abs_pairwise_pair(mat, n, col_i, col_j, corr)
    real(c_double), intent(in)  :: mat(:)
    integer(c_int), intent(in)  :: n
    integer(c_int), intent(in)  :: col_i
    integer(c_int), intent(in)  :: col_j
    real(c_double), intent(out) :: corr

    integer(c_int) :: row
    integer(c_int) :: n_valid
    integer(c_int) :: offset_i
    integer(c_int) :: offset_j
    real(c_double) :: xi
    real(c_double) :: xj
    real(c_double) :: mean_i
    real(c_double) :: mean_j
    real(c_double) :: delta_i
    real(c_double) :: delta_j
    real(c_double) :: cov_acc
    real(c_double) :: ss_i
    real(c_double) :: ss_j
    real(c_double) :: denom

    n_valid = 0_c_int
    mean_i = 0.0_c_double
    mean_j = 0.0_c_double
    cov_acc = 0.0_c_double
    ss_i = 0.0_c_double
    ss_j = 0.0_c_double

    offset_i = (col_i - 1_c_int) * n
    offset_j = (col_j - 1_c_int) * n

    do row = 1_c_int, n
      xi = mat(offset_i + row)
      xj = mat(offset_j + row)

      if (is_valid_double(xi) .and. is_valid_double(xj)) then
        n_valid = n_valid + 1_c_int
        delta_i = xi - mean_i
        delta_j = xj - mean_j
        mean_i = mean_i + delta_i / real(n_valid, c_double)
        mean_j = mean_j + delta_j / real(n_valid, c_double)
        ss_i = ss_i + delta_i * (xi - mean_i)
        ss_j = ss_j + delta_j * (xj - mean_j)
        cov_acc = cov_acc + delta_i * (xj - mean_j)
      end if
    end do

    denom = sqrt(ss_i * ss_j)
    if (n_valid < 2_c_int .or. denom <= 0.0_c_double .or. .not. ieee_is_finite(denom)) then
      corr = 0.0_c_double
    else
      corr = abs(cov_acc / denom)
      if (.not. ieee_is_finite(corr)) corr = 0.0_c_double
      if (corr > 1.0_c_double) corr = 1.0_c_double
    end if
  end subroutine pearson_abs_pairwise_pair

end module sby_correlation_pearson_core_mod

function sby_correlation_pearson_matrix_fortran(matrix_sexp, n_rows_sexp, n_cols_sexp) &
    result(result_sexp) bind(C, name="sby_correlation_pearson_matrix_fortran")

  use iso_c_binding
  use omp_lib
  use sby_correlation_pearson_core_mod
  implicit none

  interface
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

  type(c_ptr), value :: matrix_sexp
  type(c_ptr), value :: n_rows_sexp
  type(c_ptr), value :: n_cols_sexp
  type(c_ptr)        :: result_sexp

  integer(c_int), pointer :: n_rows_ptr(:)
  integer(c_int), pointer :: n_cols_ptr(:)
  real(c_double), pointer :: mat(:)
  real(c_double), pointer :: out(:)
  real(c_double), allocatable :: means(:)
  real(c_double), allocatable :: ss_cols(:)

  integer(c_int) :: n
  integer(c_int) :: p
  integer(c_int) :: i
  integer(c_int) :: j
  integer(c_int) :: idx_ij
  integer(c_int) :: idx_ji
  integer(c_int) :: n_pairs
  real(c_double) :: corr
  logical :: all_valid
  logical :: use_openmp
  integer(c_int), parameter :: openmp_min_columns = 4_c_int
  integer(c_int), parameter :: openmp_min_pair_rows = 50000_c_int

  call c_f_pointer(INTEGER(n_rows_sexp), n_rows_ptr, [1])
  call c_f_pointer(INTEGER(n_cols_sexp), n_cols_ptr, [1])

  n = n_rows_ptr(1)
  p = n_cols_ptr(1)
  n_pairs = (p * (p - 1_c_int)) / 2_c_int
  use_openmp = (p >= openmp_min_columns .and. n * n_pairs >= openmp_min_pair_rows)

  call c_f_pointer(R_REAL(matrix_sexp), mat, [n * p])

  result_sexp = Rf_protect(Rf_allocVector(REALSXP, p * p))
  call c_f_pointer(R_REAL(result_sexp), out, [p * p])

  all_valid = .true.
  do i = 1_c_int, n * p
    if (.not. is_valid_double(mat(i))) then
      all_valid = .false.
      exit
    end if
  end do

  if (all_valid) then
    allocate(means(p), ss_cols(p))

!$omp parallel do default(none) private(j) shared(n, p, mat, means, ss_cols) schedule(static) if(use_openmp)
    do j = 1_c_int, p
      call column_mean_ss(mat, n, j, means(j), ss_cols(j))
    end do
!$omp end parallel do

!$omp parallel do default(none) private(i, j, idx_ij, idx_ji, corr) &
!$omp shared(n, p, mat, out, means, ss_cols) schedule(dynamic,32) if(use_openmp)
    do j = 1_c_int, p
      out((j - 1_c_int) * p + j) = 0.0_c_double
      do i = j + 1_c_int, p
        call pearson_abs_complete_pair(mat, n, i, j, means, ss_cols, corr)
        idx_ij = (j - 1_c_int) * p + i
        idx_ji = (i - 1_c_int) * p + j
        out(idx_ij) = corr
        out(idx_ji) = corr
      end do
    end do
!$omp end parallel do

    deallocate(means, ss_cols)
  else
!$omp parallel do default(none) private(i, j, idx_ij, idx_ji, corr) &
!$omp shared(n, p, mat, out) schedule(dynamic,32) if(use_openmp)
    do j = 1_c_int, p
      out((j - 1_c_int) * p + j) = 0.0_c_double
      do i = j + 1_c_int, p
        call pearson_abs_pairwise_pair(mat, n, i, j, corr)
        idx_ij = (j - 1_c_int) * p + i
        idx_ji = (i - 1_c_int) * p + j
        out(idx_ij) = corr
        out(idx_ji) = corr
      end do
    end do
!$omp end parallel do
  end if

  call Rf_unprotect(1_c_int)
end function sby_correlation_pearson_matrix_fortran

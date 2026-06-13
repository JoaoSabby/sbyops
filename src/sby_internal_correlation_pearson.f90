!===============================================================================
! sby_internal_correlation_pearson.f90
!===============================================================================
!
! Native core for absolute Pearson correlation by column pair and Jolliffe pruning.
!
! R passes a double matrix in column-major order.
!
! all_valid path  : converts to REAL*4 internally, calls oneMKL ssyrk for X'X,
!                   normalises and prunes in single precision.
!                   Doubles AVX-512 register density vs REAL*8.
!
! pairwise path   : data contains NA/Inf; stays in REAL*8, pair-by-pair loop.
!
!===============================================================================

module sby_internal_correlation_pearson_core_mod
  use iso_c_binding
  use, intrinsic :: ieee_arithmetic
  implicit none

  integer(c_int), parameter :: REALSXP = 14
  integer(c_int), parameter :: LGLSXP  = 10

contains

  pure logical function sby_internal_is_valid_double(x)
    real(c_double), intent(in) :: x
    sby_internal_is_valid_double = ieee_is_finite(x)
  end function sby_internal_is_valid_double

  ! Pairwise (NA-safe) Pearson in REAL*8 — unchanged
  pure subroutine sby_internal_pearson_abs_pairwise_pair(mat, n, col_i, col_j, corr)
    real(c_double), intent(in)  :: mat(:)
    integer(c_int), intent(in)  :: n
    integer(c_int), intent(in)  :: col_i
    integer(c_int), intent(in)  :: col_j
    real(c_double), intent(out) :: corr

    integer(c_int) :: row, n_valid, offset_i, offset_j
    real(c_double) :: xi, xj, mean_i, mean_j
    real(c_double) :: delta_i, delta_j, cov_acc, ss_i, ss_j, denom

    n_valid = 0_c_int
    mean_i  = 0.0_c_double;  mean_j  = 0.0_c_double
    cov_acc = 0.0_c_double
    ss_i    = 0.0_c_double;  ss_j    = 0.0_c_double
    offset_i = (col_i - 1_c_int) * n
    offset_j = (col_j - 1_c_int) * n

    do row = 1_c_int, n
      xi = mat(offset_i + row)
      xj = mat(offset_j + row)
      if (sby_internal_is_valid_double(xi) .and. sby_internal_is_valid_double(xj)) then
        n_valid = n_valid + 1_c_int
        delta_i = xi - mean_i;  delta_j = xj - mean_j
        mean_i  = mean_i + delta_i / real(n_valid, c_double)
        mean_j  = mean_j + delta_j / real(n_valid, c_double)
        ss_i    = ss_i + delta_i * (xi - mean_i)
        ss_j    = ss_j + delta_j * (xj - mean_j)
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
  end subroutine sby_internal_pearson_abs_pairwise_pair

end module sby_internal_correlation_pearson_core_mod


function sby_internal_correlation_pearson_matrix_fortran(matrix_sexp, n_rows_sexp, n_cols_sexp, threshold_sexp) &
    result(result_sexp) bind(C, name="sby_internal_correlation_pearson_matrix_fortran")

  use iso_c_binding
  use omp_lib
  use sby_internal_correlation_pearson_core_mod
  implicit none

  ! oneMKL ssyrk interface (single precision)
  ! SSYRK computes C := alpha * A'*A + beta*C  (UPLO='U', TRANS='T')
  interface
    subroutine ssyrk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc) &
        bind(C, name="ssyrk_")
      use iso_c_binding
      character(c_char), intent(in) :: uplo, trans
      integer(c_int),    intent(in) :: n, k, lda, ldc
      real(c_float),     intent(in) :: alpha, beta
      real(c_float),     intent(in) :: a(*)
      real(c_float),  intent(inout) :: c(*)
    end subroutine ssyrk
  end interface

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

    function LOGICAL(x) bind(C, name="LOGICAL") result(p)
      use iso_c_binding
      type(c_ptr), value :: x
      type(c_ptr)        :: p
    end function LOGICAL
  end interface

  type(c_ptr), value :: matrix_sexp, n_rows_sexp, n_cols_sexp, threshold_sexp
  type(c_ptr)        :: result_sexp

  integer(c_int), pointer :: n_rows_ptr(:), n_cols_ptr(:)
  real(c_double), pointer :: mat(:)
  real(c_double), pointer :: threshold_ptr(:)
  integer(c_int), pointer :: out_logical(:)

  integer(c_int) :: n, p, i, j, k
  real(c_double) :: threshold
  logical        :: all_valid

  ! ---- REAL*4 working arrays (all_valid path) ----
  real(c_float),  allocatable :: mat_f(:)       ! n*p column-major REAL*4 copy
  real(c_float),  allocatable :: means_f(:)     ! column means REAL*4
  real(c_float),  allocatable :: ss_f(:)        ! column sum-of-squares REAL*4
  real(c_float),  allocatable :: xtx_f(:)       ! p*p upper-tri from ssyrk
  real(c_float),  allocatable :: cor_f(:)       ! p*p absolute correlation REAL*4
  real(c_float)  :: denom_f, corr_f, threshold_f
  real(c_float)  :: max_val_f, current_val_f, sum_i_f, sum_j_f

  ! ---- REAL*8 working arrays (pairwise path) ----
  real(c_double), allocatable :: cor_d(:)
  real(c_double) :: corr_d
  real(c_double) :: max_val_d, current_val_d, sum_i_d, sum_j_d

  ! Jolliffe shared
  logical, allocatable :: active(:)
  integer(c_int) :: num_active, best_i, best_j, remove_index
  integer(c_int) :: n_pairs
  logical        :: use_openmp
  integer(c_int), parameter :: openmp_min_columns   = 4_c_int
  integer(c_int), parameter :: openmp_min_pair_rows = 50000_c_int

  call c_f_pointer(INTEGER(n_rows_sexp), n_rows_ptr, [1])
  call c_f_pointer(INTEGER(n_cols_sexp), n_cols_ptr, [1])
  call c_f_pointer(R_REAL(threshold_sexp), threshold_ptr, [1])

  n         = n_rows_ptr(1)
  p         = n_cols_ptr(1)
  threshold = threshold_ptr(1)
  n_pairs   = (p * (p - 1_c_int)) / 2_c_int
  use_openmp = (p >= openmp_min_columns .and. n * n_pairs >= openmp_min_pair_rows)

  call c_f_pointer(R_REAL(matrix_sexp), mat, [n * p])

  result_sexp = Rf_protect(Rf_allocVector(LGLSXP, p))
  call c_f_pointer(LOGICAL(result_sexp), out_logical, [p])

  ! Check for NA/Inf
  all_valid = .true.
  do i = 1_c_int, n * p
    if (.not. sby_internal_is_valid_double(mat(i))) then
      all_valid = .false.
      exit
    end if
  end do

  allocate(active(p))
  do i = 1, p
    active(i) = .true.
  end do
  num_active = p

  ! ===========================================================
  ! PATH A: all_valid — REAL*4 + oneMKL ssyrk
  ! ===========================================================
  if (all_valid) then

    threshold_f = real(threshold, c_float)

    ! 1. Convert double* -> REAL*4 (doubles AVX-512 register density)
    allocate(mat_f(n * p))
!$omp parallel do simd default(none) shared(mat, mat_f, n, p) schedule(static) if(use_openmp)
    do i = 1_c_int, n * p
      mat_f(i) = real(mat(i), c_float)
    end do
!$omp end parallel do simd

    ! 2. Compute column means in REAL*4
    allocate(means_f(p))
!$omp parallel do default(none) private(i, j) shared(mat_f, means_f, n, p) schedule(static) if(use_openmp)
    do j = 1_c_int, p
      means_f(j) = 0.0_c_float
      do i = 1_c_int, n
        means_f(j) = means_f(j) + mat_f((j - 1_c_int) * n + i)
      end do
      means_f(j) = means_f(j) / real(n, c_float)
    end do
!$omp end parallel do

    ! 3. Center columns in-place (REAL*4)
!$omp parallel do default(none) private(i, j) shared(mat_f, means_f, n, p) schedule(static) if(use_openmp)
    do j = 1_c_int, p
      do i = 1_c_int, n
        mat_f((j - 1_c_int) * n + i) = mat_f((j - 1_c_int) * n + i) - means_f(j)
      end do
    end do
!$omp end parallel do
    deallocate(means_f)

    ! 4. Column sum-of-squares in REAL*4
    allocate(ss_f(p))
!$omp parallel do default(none) private(i, j) shared(mat_f, ss_f, n, p) schedule(static) if(use_openmp)
    do j = 1_c_int, p
      ss_f(j) = 0.0_c_float
      do i = 1_c_int, n
        ss_f(j) = ss_f(j) + mat_f((j - 1_c_int) * n + i) ** 2
      end do
    end do
!$omp end parallel do

    ! 5. X'X via oneMKL ssyrk — REAL*4, upper triangle
    !    C (p x p) := 1.0 * mat_f' * mat_f + 0.0 * C
    !    UPLO='U', TRANS='T', N=p, K=n
    allocate(xtx_f(p * p))
    xtx_f = 0.0_c_float
    call ssyrk('U', 'T', p, n, 1.0_c_float, mat_f, n, 0.0_c_float, xtx_f, p)
    deallocate(mat_f)

    ! 6. Normalise to absolute correlation in REAL*4
    allocate(cor_f(p * p))
    cor_f = 0.0_c_float
    do j = 1_c_int, p
      do i = 1_c_int, j - 1_c_int
        denom_f = sqrt(ss_f(i) * ss_f(j))
        if (denom_f > 0.0_c_float) then
          corr_f = abs(xtx_f((j - 1_c_int) * p + i) / denom_f)
          if (corr_f > 1.0_c_float) corr_f = 1.0_c_float
        else
          corr_f = 0.0_c_float
        end if
        cor_f((j - 1_c_int) * p + i) = corr_f
        cor_f((i - 1_c_int) * p + j) = corr_f
      end do
    end do
    deallocate(xtx_f, ss_f)

    ! 7. Jolliffe pruning in REAL*4
    do while (num_active >= 2_c_int)
      max_val_f = -1.0_c_float
      best_i = -1_c_int;  best_j = -1_c_int

      do j = 2_c_int, p
        if (.not. active(j)) cycle
        do i = 1_c_int, j - 1_c_int
          if (.not. active(i)) cycle
          current_val_f = cor_f((j - 1_c_int) * p + i)
          if (current_val_f > max_val_f) then
            max_val_f = current_val_f
            best_i = i;  best_j = j
          end if
        end do
      end do

      if (max_val_f < threshold_f .or. best_i == -1_c_int) exit

      sum_i_f = 0.0_c_float;  sum_j_f = 0.0_c_float
      do k = 1_c_int, p
        if (active(k) .and. k /= best_i) sum_i_f = sum_i_f + cor_f((k - 1_c_int) * p + best_i)
        if (active(k) .and. k /= best_j) sum_j_f = sum_j_f + cor_f((k - 1_c_int) * p + best_j)
      end do

      if (sum_i_f >= sum_j_f) then
        remove_index = best_i
      else
        remove_index = best_j
      end if

      active(remove_index) = .false.
      num_active = num_active - 1_c_int
    end do

    deallocate(cor_f)

  ! ===========================================================
  ! PATH B: pairwise (NA/Inf present) — REAL*8, unchanged
  ! ===========================================================
  else

    allocate(cor_d(p * p))

!$omp parallel do default(none) private(i, j, corr_d) &
!$omp shared(n, p, mat, cor_d) schedule(dynamic,32) if(use_openmp)
    do j = 1_c_int, p
      cor_d((j - 1_c_int) * p + j) = 0.0_c_double
      do i = j + 1_c_int, p
        call sby_internal_pearson_abs_pairwise_pair(mat, n, i, j, corr_d)
        cor_d((j - 1_c_int) * p + i) = corr_d
        cor_d((i - 1_c_int) * p + j) = corr_d
      end do
    end do
!$omp end parallel do

    do while (num_active >= 2_c_int)
      max_val_d = -1.0_c_double
      best_i = -1_c_int;  best_j = -1_c_int

      do j = 2_c_int, p
        if (.not. active(j)) cycle
        do i = 1_c_int, j - 1_c_int
          if (.not. active(i)) cycle
          current_val_d = cor_d((j - 1_c_int) * p + i)
          if (current_val_d > max_val_d) then
            max_val_d = current_val_d
            best_i = i;  best_j = j
          end if
        end do
      end do

      if (max_val_d < threshold .or. best_i == -1_c_int) exit

      sum_i_d = 0.0_c_double;  sum_j_d = 0.0_c_double
      do k = 1_c_int, p
        if (active(k) .and. k /= best_i) sum_i_d = sum_i_d + cor_d((k - 1_c_int) * p + best_i)
        if (active(k) .and. k /= best_j) sum_j_d = sum_j_d + cor_d((k - 1_c_int) * p + best_j)
      end do

      if (sum_i_d >= sum_j_d) then
        remove_index = best_i
      else
        remove_index = best_j
      end if

      active(remove_index) = .false.
      num_active = num_active - 1_c_int
    end do

    deallocate(cor_d)

  end if

  ! Write result
  do i = 1_c_int, p
    out_logical(i) = merge(1_c_int, 0_c_int, active(i))
  end do

  deallocate(active)
  call Rf_unprotect(1_c_int)

end function sby_internal_correlation_pearson_matrix_fortran

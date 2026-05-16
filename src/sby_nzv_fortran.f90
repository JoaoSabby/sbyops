!===============================================================================
! sby_fe_filter_nzv_fortran.f90
!===============================================================================
!
! Núcleo nativo para detecção de colunas com baixa variabilidade.
!
! Definição operacional
! ---------------------
! Uma coluna entra no filtro quando algum conteúdo aparece com frequência relativa
! maior ou igual a `threshold`.
!
! Exemplos:
!   - threshold = 1.00: somente colunas completamente constantes entram.
!   - threshold = 0.95: colunas em que algum conteúdo aparece em pelo menos 95%
!     das linhas entram.
!   - threshold = 0.00: toda coluna não vazia entra, pois qualquer frequência
!     observada é maior ou igual a zero.
!
! Estratégia adotada
! ------------------
! A interface R prepara cada coluna como um vetor de códigos inteiros positivos.
! Conteúdos iguais recebem o mesmo código. Conteúdos diferentes recebem códigos
! diferentes. Valores ausentes também recebem código próprio, portanto NA é
! contabilizado como conteúdo.
!
! Esta rotina Fortran não precisa conhecer os tipos originais das colunas
! (numeric, integer, logical, factor ou character). Ela só precisa encontrar,
! para cada vetor de códigos:
!
!   1. o código mais frequente;
!   2. a frequência absoluta desse código;
!   3. a razão frequência / número_de_linhas.
!
! O valor original correspondente ao código modal é reconstruído no R. Essa
! separação deixa o núcleo nativo simples, seguro e rápido, evitando manipulação
! direta de strings, fatores e atributos do R dentro do Fortran.
!
! Complexidade
! ------------
! Para cada coluna com n linhas e k conteúdos distintos codificados:
!
!   - tempo:  O(n + k)
!   - memória: O(k)
!
! Como os códigos são compactos e positivos, a contagem direta é mais rápida e
! mais simples do que ordenar a coluna. Não há radix sort, comparação de strings
! ou tratamento especial de NaN no núcleo nativo.
!
!===============================================================================

module sby_fe_filter_nzv_fortran_mod
  use iso_c_binding
  implicit none

  ! Tipos SEXP usados pela API C do R.
  integer(c_int), parameter :: INTSXP  = 13
  integer(c_int), parameter :: REALSXP = 14
  integer(c_int), parameter :: VECSXP  = 19

contains

  !-----------------------------------------------------------------------------
  ! fill_zero_int
  !-----------------------------------------------------------------------------
  !
  ! Zera um vetor de inteiros.
  !
  ! A rotina é pequena, mas existe para deixar claro que o vetor de contadores
  ! precisa começar limpo a cada coluna. O uso de laço explícito também evita
  ! dependência de extensões específicas de compilador.
  !
  ! Argumentos:
  !   x : vetor que será zerado.
  !   n : tamanho efetivo do vetor.
  !
  !-----------------------------------------------------------------------------
  pure subroutine fill_zero_int(x, n)
    integer(c_int), intent(inout) :: x(:)
    integer(c_int), intent(in)    :: n
    integer(c_int)                :: i

    do i = 1, n
      x(i) = 0_c_int
    end do
  end subroutine fill_zero_int

  !-----------------------------------------------------------------------------
  ! column_mode_from_codes
  !-----------------------------------------------------------------------------
  !
  ! Calcula o código modal de uma coluna já codificada.
  !
  ! Pré-condições:
  !   - `codes` contém apenas inteiros positivos.
  !   - conteúdos ausentes já foram convertidos para um código positivo no R.
  !   - `max_code` é o maior código possível na coluna.
  !
  ! Por que contagem direta?
  !   Como os códigos são compactos, o melhor caminho é criar um vetor `counts`
  !   com uma posição para cada código. Cada linha incrementa uma posição. Ao
  !   final, uma varredura curta encontra a maior contagem.
  !
  ! Critério de desempate:
  !   Se dois códigos tiverem a mesma frequência máxima, vence o menor código.
  !   Isso torna o resultado determinístico. O desempate raramente importa para
  !   o filtro, mas é importante para reprodutibilidade.
  !
  ! Argumentos:
  !   codes      : vetor de códigos inteiros.
  !   n          : número de linhas da coluna.
  !   max_code   : maior código possível.
  !   counts     : vetor auxiliar de contagem, tamanho >= max_code.
  !   mode_code  : código mais frequente encontrado.
  !   mode_count : frequência absoluta do código mais frequente.
  !
  !-----------------------------------------------------------------------------
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

end module sby_fe_filter_nzv_fortran_mod

!===============================================================================
! sby_fe_filter_nzv_codes_fortran
!===============================================================================
!
! Ponto de entrada chamado por .Call() no R.
!
! Entrada:
!   codes_list_sexp : lista R; cada elemento é um vetor integer com códigos.
!   max_codes_sexp  : vetor integer; maior código de cada coluna.
!   threshold_sexp  : escalar numeric entre 0 e 1.
!
! Saída:
!   Lista R com quatro vetores:
!     1. column : índices das colunas filtradas, base 1.
!     2. ratio  : frequência relativa do conteúdo modal.
!     3. code   : código modal.
!     4. count  : frequência absoluta do código modal.
!
! A função retorna somente colunas cujo `ratio >= threshold`.
!
! Paralelismo OpenMP
! ------------------
! O processamento das colunas é paralelizado com OpenMP. A API do R é usada
! apenas antes e depois da região paralela. Dentro da região paralela são usados
! somente ponteiros de dados já capturados, arrays Fortran e cálculo numérico.
! Essa separação é obrigatória porque a API interna do R não é thread-safe.
!
!===============================================================================
function sby_fe_filter_nzv_codes_fortran(codes_list_sexp, max_codes_sexp, threshold_sexp, n_threads_sexp) &
    result(result_sexp) bind(C, name="sby_fe_filter_nzv_codes_fortran")

  use iso_c_binding
  use omp_lib
  use sby_fe_filter_nzv_fortran_mod
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
  type(c_ptr), value :: threshold_sexp
  type(c_ptr), value :: n_threads_sexp
  type(c_ptr)        :: result_sexp

  type(c_ptr) :: col_sexp
  type(c_ptr) :: out_column_sexp
  type(c_ptr) :: out_ratio_sexp
  type(c_ptr) :: out_code_sexp
  type(c_ptr) :: out_count_sexp

  integer(c_int), pointer :: codes(:)
  integer(c_int), pointer :: max_codes(:)
  integer(c_int), pointer :: n_threads_ptr(:)
  integer(c_int), pointer :: out_column(:)
  integer(c_int), pointer :: out_code(:)
  integer(c_int), pointer :: out_count(:)
  integer(c_int), allocatable :: counts(:)
  real(c_double), pointer :: threshold_ptr(:)
  real(c_double), pointer :: out_ratio(:)

  type(c_ptr), allocatable :: column_ptr(:)
  integer(c_int), allocatable :: column_n(:)

  integer(c_int), allocatable :: keep_flag(:)
  integer(c_int), allocatable :: mode_code_all(:)
  integer(c_int), allocatable :: mode_count_all(:)
  real(c_double), allocatable :: ratio_all(:)

  integer(c_int) :: ncols
  integer(c_int) :: n
  integer(c_int) :: j
  integer(c_int) :: out_i
  integer(c_int) :: n_keep
  integer(c_int) :: max_code
  integer(c_int) :: mode_code
  integer(c_int) :: mode_count
  integer(c_int) :: n_threads
  real(c_double) :: threshold
  real(c_double) :: ratio

  ! Recupera threshold e metadados.
  call c_f_pointer(R_REAL(threshold_sexp), threshold_ptr, [1])
  threshold = threshold_ptr(1)

  call c_f_pointer(INTEGER(n_threads_sexp), n_threads_ptr, [1])
  n_threads = n_threads_ptr(1)

  if (n_threads > 0_c_int) then
    call omp_set_num_threads(n_threads)
  end if

  ncols = Rf_length(codes_list_sexp)

  if (ncols <= 0_c_int) then
    result_sexp = Rf_protect(Rf_allocVector(VECSXP, 4_c_int))
    call SET_VECTOR_ELT(result_sexp, 0_c_int, Rf_allocVector(INTSXP,  0_c_int))
    call SET_VECTOR_ELT(result_sexp, 1_c_int, Rf_allocVector(REALSXP, 0_c_int))
    call SET_VECTOR_ELT(result_sexp, 2_c_int, Rf_allocVector(INTSXP,  0_c_int))
    call SET_VECTOR_ELT(result_sexp, 3_c_int, Rf_allocVector(INTSXP,  0_c_int))
    call Rf_unprotect(1_c_int)
    return
  end if

  call c_f_pointer(INTEGER(max_codes_sexp), max_codes, [ncols])

  allocate(column_ptr(ncols))
  allocate(column_n(ncols))
  allocate(keep_flag(ncols))
  allocate(mode_code_all(ncols))
  allocate(mode_count_all(ncols))
  allocate(ratio_all(ncols))

  ! ---------------------------------------------------------------------------
  ! Captura sequencial de ponteiros e tamanhos.
  ! ---------------------------------------------------------------------------
  ! R não permite uso seguro da sua API em várias threads. Por isso, os acessos
  ! a VECTOR_ELT(), Rf_length() e INTEGER() acontecem antes do bloco OpenMP.
  ! Dentro da região paralela, cada thread usa apenas os ponteiros de dados já
  ! obtidos e escreve em posições exclusivas dos arrays de resultado.
  do j = 1_c_int, ncols
    col_sexp      = VECTOR_ELT(codes_list_sexp, j - 1_c_int)
    column_n(j)   = Rf_length(col_sexp)
    column_ptr(j) = INTEGER(col_sexp)
  end do

  keep_flag      = 0_c_int
  mode_code_all  = 0_c_int
  mode_count_all = 0_c_int
  ratio_all      = 0.0_c_double

  ! ---------------------------------------------------------------------------
  ! Região paralela.
  ! ---------------------------------------------------------------------------
  ! Cada thread recebe um vetor `counts` privado. Esse vetor cresce sob demanda
  ! até o maior `max_code` visto por aquela thread. Essa estratégia evita data
  ! races, reduz realocações repetidas e evita alocar, em todas as threads, o
  ! maior número de códigos observado globalmente.
  !
  ! `schedule(dynamic,1)` é usado porque colunas com muitos níveis distintos
  ! custam mais caro do que colunas com poucos níveis. O escalonamento dinâmico
  ! melhora o balanceamento de carga em bases heterogêneas.
!$omp parallel default(none) &
!$omp shared(ncols, column_ptr, column_n, max_codes, threshold, &
!$omp        keep_flag, mode_code_all, mode_count_all, ratio_all) &
!$omp private(j, n, max_code, codes, counts, mode_code, mode_count, ratio)

!$omp do schedule(dynamic,1)
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

      call column_mode_from_codes( &
        codes      = codes, &
        n          = n, &
        max_code   = max_code, &
        counts     = counts, &
        mode_code  = mode_code, &
        mode_count = mode_count &
      )

      ratio = real(mode_count, c_double) / real(n, c_double)

      mode_code_all(j)  = mode_code
      mode_count_all(j) = mode_count
      ratio_all(j)      = ratio

      if (ratio >= threshold) then
        keep_flag(j) = 1_c_int
      end if
    end if
  end do
!$omp end do

  if (allocated(counts)) deallocate(counts)

!$omp end parallel

  n_keep = 0_c_int
  do j = 1_c_int, ncols
    if (keep_flag(j) == 1_c_int) n_keep = n_keep + 1_c_int
  end do

  result_sexp     = Rf_protect(Rf_allocVector(VECSXP, 4_c_int))
  out_column_sexp = Rf_protect(Rf_allocVector(INTSXP,  n_keep))
  out_ratio_sexp  = Rf_protect(Rf_allocVector(REALSXP, n_keep))
  out_code_sexp   = Rf_protect(Rf_allocVector(INTSXP,  n_keep))
  out_count_sexp  = Rf_protect(Rf_allocVector(INTSXP,  n_keep))

  call c_f_pointer(INTEGER(out_column_sexp), out_column, [n_keep])
  call c_f_pointer(R_REAL(out_ratio_sexp),     out_ratio,  [n_keep])
  call c_f_pointer(INTEGER(out_code_sexp),   out_code,   [n_keep])
  call c_f_pointer(INTEGER(out_count_sexp),  out_count,  [n_keep])

  out_i = 0_c_int
  do j = 1_c_int, ncols
    if (keep_flag(j) == 1_c_int) then
      out_i = out_i + 1_c_int
      out_column(out_i) = j
      out_ratio(out_i)  = ratio_all(j)
      out_code(out_i)   = mode_code_all(j)
      out_count(out_i)  = mode_count_all(j)
    end if
  end do

  call SET_VECTOR_ELT(result_sexp, 0_c_int, out_column_sexp)
  call SET_VECTOR_ELT(result_sexp, 1_c_int, out_ratio_sexp)
  call SET_VECTOR_ELT(result_sexp, 2_c_int, out_code_sexp)
  call SET_VECTOR_ELT(result_sexp, 3_c_int, out_count_sexp)

  call Rf_unprotect(5_c_int)

  deallocate(column_ptr)
  deallocate(column_n)
  deallocate(keep_flag)
  deallocate(mode_code_all)
  deallocate(mode_count_all)
  deallocate(ratio_all)

end function sby_fe_filter_nzv_codes_fortran

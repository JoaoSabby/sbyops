
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/**
 * @title Verifica se um vetor atomico e constante
 * @description Segue uma logica de saida rapida e encerra no primeiro valor diferente
 * @details Para REAL e COMPLEX o comportamento trata NA e NaN como equivalentes dentro da coluna
 * @param x Vetor atomico sem dimensao
 * @return 1 quando a coluna e constante e 0 caso contrario
 */
static int sby_internal_is_constant_atomic(SEXP x) {
  /* Le tamanho da coluna e sai rapido para vetores com 0 ou 1 elemento */
  R_xlen_t n = XLENGTH(x);
  if (n <= 1) return 1;

  /* Escolhe caminho especializado por tipo para reduzir overhead */
  switch (TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    /* Compara todos os inteiros/logicos com o primeiro valor */
    int first = INTEGER(x)[0];
    for (R_xlen_t i = 1; i < n; ++i) {
      if (INTEGER(x)[i] != first) return 0;
    }
    return 1;
  }
  case REALSXP: {
    /* Para double trata NA e NaN via ISNAN para semantica consistente */
    double first = REAL(x)[0];
    int first_nan = ISNAN(first);
    for (R_xlen_t i = 1; i < n; ++i) {
      double cur = REAL(x)[i];
      if (first_nan) {
        if (!ISNAN(cur)) return 0;
      } else {
        if (cur != first) return 0;
      }
    }
    return 1;
  }
  case CPLXSXP: {
    /* Em complex avalia parte real e imaginaria com a mesma logica de NaN */
    Rcomplex first = COMPLEX(x)[0];
    int first_nan_r = ISNAN(first.r), first_nan_i = ISNAN(first.i);
    for (R_xlen_t i = 1; i < n; ++i) {
      Rcomplex cur = COMPLEX(x)[i];
      if (first_nan_r) { if (!ISNAN(cur.r)) return 0; } else if (cur.r != first.r) return 0;
      if (first_nan_i) { if (!ISNAN(cur.i)) return 0; } else if (cur.i != first.i) return 0;
    }
    return 1;
  }
  case STRSXP: {
    /* Em string usa comparacao por ponteiro quando possivel e fallback por conteudo */
    SEXP first = STRING_ELT(x, 0);
    for (R_xlen_t i = 1; i < n; ++i) {
      SEXP cur = STRING_ELT(x, i);
      if (cur == first) continue;
      if (cur == NA_STRING || first == NA_STRING) return 0;
      if (strcmp(CHAR(cur), CHAR(first)) != 0) return 0;
    }
    return 1;
  }
  case RAWSXP: {
    /* Para bytes faz comparacao direta valor a valor */
    Rbyte first = RAW(x)[0];
    for (R_xlen_t i = 1; i < n; ++i) {
      if (RAW(x)[i] != first) return 0;
    }
    return 1;
  }
  default:
    /* Tipo nao tratado explicitamente fica como nao constante por seguranca */
    return 0;
  }
}

/**
 * @title Retorna mascara de colunas nao constantes
 * @description Processa colunas de forma independente com paralelismo implicito por coluna
 * @details Colunas nao atomicas ficam como manter para evitar remocao indevida
 * @param cols Lista de colunas
 * @return Vetor logico com TRUE para manter e FALSE para remover
 */
SEXP sby_internal_non_constant_mask(SEXP cols) {
  /* Valida contrato de entrada para evitar acesso invalido de memoria */
  if (!isNewList(cols)) {
    Rf_error("`cols` must be a list.");
  }

  /* Prepara vetor de saida com um flag por coluna */
  R_xlen_t p = XLENGTH(cols);
  SEXP out = PROTECT(allocVector(LGLSXP, p));
  int *res = LOGICAL(out);

  /* Executa sequencialmente porque objetos R e ALTREP nao sao seguros para acesso via R API em regioes OpenMP */
  for (R_xlen_t j = 0; j < p; ++j) {
    /* Le coluna corrente e assume manter por padrao */
    SEXP col = VECTOR_ELT(cols, j);
    int keep = 1;

    /* So aplica regra de constancia para vetor atomico sem dimensao */
    if (isVectorAtomic(col) && isNull(getAttrib(col, R_DimSymbol))) {
      keep = !sby_internal_is_constant_atomic(col);
    }

    /* Salva decisao da coluna no vetor de mascara */
    res[j] = keep;
  }

  /* Libera protecao e retorna mascara final para o R */
  UNPROTECT(1);
  return out;
}

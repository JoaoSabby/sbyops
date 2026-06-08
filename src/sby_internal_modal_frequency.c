#include <math.h>
#include <stdint.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>

static size_t sby_internal_next_power_two(R_xlen_t n) {
  size_t cap = 16u;
  double target = (double)n * 2.0;
  while ((double)cap < target) {
    if (cap > ((size_t)-1) / 2u) {
      Rf_error("modal-frequency hash table is too large.");
    }
    cap *= 2u;
  }
  return cap;
}

static uint32_t sby_internal_mix_u32(uint32_t x) {
  x ^= x >> 16;
  x *= 0x7feb352du;
  x ^= x >> 15;
  x *= 0x846ca68bu;
  x ^= x >> 16;
  return x;
}

static uint32_t sby_internal_mix_u64(uint64_t x) {
  x ^= x >> 33;
  x *= UINT64_C(0xff51afd7ed558ccd);
  x ^= x >> 33;
  x *= UINT64_C(0xc4ceb9fe1a85ec53);
  x ^= x >> 33;
  return (uint32_t)(x ^ (x >> 32));
}

static int sby_internal_modal_keep_integer(SEXP x, R_xlen_t cutoff, R_xlen_t n) {
  if (cutoff <= 0) return 0;

  size_t cap = sby_internal_next_power_two(n);
  size_t mask = cap - 1u;
  int *keys = R_Calloc(cap, int);
  R_xlen_t *counts = R_Calloc(cap, R_xlen_t);
  unsigned char *used = R_Calloc(cap, unsigned char);
  int keep = 1;

  for (R_xlen_t i = 0; i < n; ++i) {
    int value = INTEGER(x)[i];
    size_t pos = (size_t)sby_internal_mix_u32((uint32_t)value) & mask;

    while (used[pos] && keys[pos] != value) {
      pos = (pos + 1u) & mask;
    }

    if (!used[pos]) {
      used[pos] = 1u;
      keys[pos] = value;
      counts[pos] = 1;
    } else {
      counts[pos] += 1;
    }

    if (counts[pos] >= cutoff) {
      keep = 0;
      break;
    }
  }

  R_Free(used);
  R_Free(counts);
  R_Free(keys);
  return keep;
}

static int sby_internal_real_equal(double a, double b) {
  if (R_IsNA(a) || R_IsNA(b)) return R_IsNA(a) && R_IsNA(b);
  if (R_IsNaN(a) || R_IsNaN(b)) return R_IsNaN(a) && R_IsNaN(b);
  return a == b;
}

static uint32_t sby_internal_hash_real(double value) {
  if (R_IsNA(value)) return 0x9e3779b9u;
  if (R_IsNaN(value)) return 0x85ebca6bu;
  if (value == 0.0) value = 0.0;

  uint64_t bits = 0u;
  memcpy(&bits, &value, sizeof(double));
  return sby_internal_mix_u64(bits);
}

static int sby_internal_modal_keep_real(SEXP x, R_xlen_t cutoff, R_xlen_t n) {
  if (cutoff <= 0) return 0;

  size_t cap = sby_internal_next_power_two(n);
  size_t mask = cap - 1u;
  double *keys = R_Calloc(cap, double);
  R_xlen_t *counts = R_Calloc(cap, R_xlen_t);
  unsigned char *used = R_Calloc(cap, unsigned char);
  int keep = 1;

  for (R_xlen_t i = 0; i < n; ++i) {
    double value = REAL(x)[i];
    size_t pos = (size_t)sby_internal_hash_real(value) & mask;

    while (used[pos] && !sby_internal_real_equal(keys[pos], value)) {
      pos = (pos + 1u) & mask;
    }

    if (!used[pos]) {
      used[pos] = 1u;
      keys[pos] = value;
      counts[pos] = 1;
    } else {
      counts[pos] += 1;
    }

    if (counts[pos] >= cutoff) {
      keep = 0;
      break;
    }
  }

  R_Free(used);
  R_Free(counts);
  R_Free(keys);
  return keep;
}

static int sby_internal_modal_keep_string(SEXP x, R_xlen_t cutoff, R_xlen_t n) {
  if (cutoff <= 0) return 0;

  size_t cap = sby_internal_next_power_two(n);
  size_t mask = cap - 1u;
  SEXP *keys = R_Calloc(cap, SEXP);
  R_xlen_t *counts = R_Calloc(cap, R_xlen_t);
  unsigned char *used = R_Calloc(cap, unsigned char);
  int keep = 1;

  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP value = STRING_ELT(x, i);
    size_t pos = (size_t)sby_internal_mix_u64((uint64_t)(uintptr_t)value) & mask;

    while (used[pos] && keys[pos] != value) {
      pos = (pos + 1u) & mask;
    }

    if (!used[pos]) {
      used[pos] = 1u;
      keys[pos] = value;
      counts[pos] = 1;
    } else {
      counts[pos] += 1;
    }

    if (counts[pos] >= cutoff) {
      keep = 0;
      break;
    }
  }

  R_Free(used);
  R_Free(counts);
  R_Free(keys);
  return keep;
}

SEXP sby_internal_modal_frequency_keep_mask(SEXP cols, SEXP threshold_sexp, SEXP n_rows_sexp) {
  if (!isNewList(cols)) {
    Rf_error("`cols` must be a list.");
  }
  if (!isReal(threshold_sexp) || XLENGTH(threshold_sexp) != 1) {
    Rf_error("`threshold` must be a numeric scalar.");
  }
  if (!isInteger(n_rows_sexp) || XLENGTH(n_rows_sexp) != 1) {
    Rf_error("`n_rows` must be an integer scalar.");
  }

  double threshold = REAL(threshold_sexp)[0];
  R_xlen_t n = (R_xlen_t)INTEGER(n_rows_sexp)[0];
  R_xlen_t cutoff = (R_xlen_t)ceil(threshold * (double)n);
  R_xlen_t p = XLENGTH(cols);
  SEXP out = PROTECT(allocVector(LGLSXP, p));
  int *res = LOGICAL(out);

  for (R_xlen_t j = 0; j < p; ++j) {
    SEXP col = VECTOR_ELT(cols, j);

    if (!isVectorAtomic(col) || !isNull(getAttrib(col, R_DimSymbol)) || XLENGTH(col) != n) {
      res[j] = NA_LOGICAL;
      continue;
    }

    switch (TYPEOF(col)) {
    case LGLSXP:
    case INTSXP:
      res[j] = sby_internal_modal_keep_integer(col, cutoff, n);
      break;
    case REALSXP:
      res[j] = sby_internal_modal_keep_real(col, cutoff, n);
      break;
    case STRSXP:
      res[j] = sby_internal_modal_keep_string(col, cutoff, n);
      break;
    default:
      res[j] = NA_LOGICAL;
      break;
    }
  }

  UNPROTECT(1);
  return out;
}

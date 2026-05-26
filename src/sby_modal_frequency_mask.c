#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef _OPENMP
#include <omp.h>
#endif

static int should_remove_logical(SEXP x, R_xlen_t n, R_xlen_t limit){
  const int *v = LOGICAL(x);
  R_xlen_t c0=0,c1=0,cna=0;
  for(R_xlen_t i=0;i<n;i++){
    int cur=v[i];
    if(cur==0){ if(++c0>=limit) return 1; }
    else if(cur==1){ if(++c1>=limit) return 1; }
    else { if(++cna>=limit) return 1; }
  }
  return 0;
}

static int should_remove_integer_like(SEXP x, R_xlen_t n, R_xlen_t limit){
  const int *v = INTEGER(x);
  int has_non_na=0,minv=0,maxv=0;
  R_xlen_t cna=0;
  for(R_xlen_t i=0;i<n;i++){
    int cur=v[i];
    if(cur==NA_INTEGER){ if(++cna>=limit) return 1; continue; }
    if(!has_non_na){ minv=maxv=cur; has_non_na=1; }
    else { if(cur<minv) minv=cur; if(cur>maxv) maxv=cur; }
  }
  if(!has_non_na) return 0;
  long long span = (long long)maxv - (long long)minv + 1LL;
  if(span > 0 && span <= 1000000LL){
    int *counts = (int*) calloc((size_t)span, sizeof(int));
    if(!counts) Rf_error("Memory allocation failed.");
    for(R_xlen_t i=0;i<n;i++){
      int cur=v[i];
      if(cur==NA_INTEGER) continue;
      size_t idx = (size_t)((long long)cur - (long long)minv);
      int cnt = ++counts[idx];
      if((R_xlen_t)cnt >= limit){ free(counts); return 1; }
    }
    free(counts);
    return 0;
  }
  int cap = 1;
  while(cap < 4096) cap <<= 1;
  int *keys = (int*) malloc((size_t)cap*sizeof(int));
  int *vals = (int*) calloc((size_t)cap,sizeof(int));
  unsigned char *used = (unsigned char*) calloc((size_t)cap,1);
  if(!keys||!vals||!used){
    free(keys); free(vals); free(used);
    Rf_error("Memory allocation failed.");
  }
  for(R_xlen_t i=0;i<n;i++){
    int cur=v[i]; if(cur==NA_INTEGER) continue;
    uint32_t h = (uint32_t)cur * 2654435761u;
    int idx = (int)(h & (uint32_t)(cap-1));
    while(used[idx] && keys[idx]!=cur) idx=(idx+1)&(cap-1);
    if(!used[idx]){ used[idx]=1; keys[idx]=cur; vals[idx]=0; }
    if((R_xlen_t)(++vals[idx]) >= limit){ free(keys);free(vals);free(used); return 1; }
  }
  free(keys);free(vals);free(used); return 0;
}

static int should_remove_real(SEXP x, R_xlen_t n, R_xlen_t limit){
  const double *v = REAL(x);
  int cap=1; while(cap<4096) cap<<=1;
  uint64_t *keys=(uint64_t*)malloc((size_t)cap*sizeof(uint64_t));
  int *vals=(int*)calloc((size_t)cap,sizeof(int));
  unsigned char *used=(unsigned char*)calloc((size_t)cap,1);
  if(!keys||!vals||!used){
    free(keys); free(vals); free(used);
    Rf_error("Memory allocation failed.");
  }
  for(R_xlen_t i=0;i<n;i++){
    double d=v[i];
    uint64_t k;
    if(ISNAN(d)) k=UINT64_C(0x7ff8000000000001);
    else {
      if(d==0.0) d=0.0;
      union { double d; uint64_t u; } u; u.d=d; k=u.u;
    }
    uint64_t h = k * UINT64_C(11400714819323198485);
    int idx = (int)(h & (uint64_t)(cap-1));
    while(used[idx] && keys[idx]!=k) idx=(idx+1)&(cap-1);
    if(!used[idx]){ used[idx]=1; keys[idx]=k; vals[idx]=0; }
    if((R_xlen_t)(++vals[idx]) >= limit){ free(keys);free(vals);free(used); return 1; }
  }
  free(keys);free(vals);free(used); return 0;
}

static int should_remove_character(SEXP x, R_xlen_t n, R_xlen_t limit){
  int cap=1; while(cap<4096) cap<<=1;
  SEXP *keys=(SEXP*)malloc((size_t)cap*sizeof(SEXP));
  int *vals=(int*)calloc((size_t)cap,sizeof(int));
  unsigned char *used=(unsigned char*)calloc((size_t)cap,1);
  if(!keys||!vals||!used){
    free(keys); free(vals); free(used);
    Rf_error("Memory allocation failed.");
  }
  for(R_xlen_t i=0;i<n;i++){
    SEXP s=STRING_ELT(x,i);
    uintptr_t kk=(uintptr_t)s;
    uint64_t h=(uint64_t)kk*UINT64_C(11400714819323198485);
    int idx=(int)(h & (uint64_t)(cap-1));
    while(used[idx] && keys[idx]!=s) idx=(idx+1)&(cap-1);
    if(!used[idx]){ used[idx]=1; keys[idx]=s; vals[idx]=0; }
    if((R_xlen_t)(++vals[idx]) >= limit){ free(keys);free(vals);free(used); return 1; }
  }
  free(keys);free(vals);free(used); return 0;
}

SEXP sby_modal_frequency_mask(SEXP selected_list, SEXP threshold, SEXP max_threads){
  if(!isNewList(selected_list)) Rf_error("`selected_list` must be a list.");
  if(TYPEOF(threshold)!=REALSXP || XLENGTH(threshold)!=1) Rf_error("`threshold` must be scalar double.");
  if(TYPEOF(max_threads)!=INTSXP || XLENGTH(max_threads)!=1) Rf_error("`max_threads` must be scalar integer.");
  double thr = REAL(threshold)[0];
  int mthreads = INTEGER(max_threads)[0];
  if(!R_finite(thr) || thr < 0.0 || thr > 1.0) Rf_error("`threshold` must be in [0, 1].");
  if(mthreads < 1) mthreads = 1;

  R_xlen_t p = XLENGTH(selected_list);
  SEXP out = PROTECT(allocVector(LGLSXP, p));
  int *res = LOGICAL(out);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(mthreads)
#endif
  for(R_xlen_t j=0;j<p;j++){
    SEXP col = VECTOR_ELT(selected_list, j);
    R_xlen_t n = XLENGTH(col);
    if(n == 0){ res[j] = 1; continue; }
    R_xlen_t limit = (R_xlen_t) ceil(thr * (double)n);
    int remove = 0;
    if(limit <= 0) { res[j]=0; continue; }
    switch(TYPEOF(col)){
      case LGLSXP: remove = should_remove_logical(col,n,limit); break;
      case INTSXP: remove = should_remove_integer_like(col,n,limit); break;
      case REALSXP: remove = should_remove_real(col,n,limit); break;
      case STRSXP: remove = should_remove_character(col,n,limit); break;
      default: remove = 0;
    }
    res[j] = remove ? 0 : 1;
  }
  UNPROTECT(1);
  return out;
}

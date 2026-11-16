#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Random.h>


extern void forest_mat(
  const double* const x__,
  const int* const depths__,
  const int* const vars__,
  const double* const thresh__,
  const int* const p__,
  int* const out__,
  const R_xlen_t depths__len_,
  const R_xlen_t thresh__len_,
  const R_xlen_t vars__len_,
  const R_len_t x__dim_1_,
  const R_len_t x__dim_2_);

SEXP forest_mat_(SEXP _args) {
  // x
  _args = CDR(_args);
  SEXP x = CAR(_args);
  if (TYPEOF(x) != REALSXP) {
    Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
  }
  const double* const x__ = REAL(x);
  const int* const x__dim_ = ({
  SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "x must be a 2D-array, but length(dim(x)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int x__dim_1_ = x__dim_[0];
  const int x__dim_2_ = x__dim_[1];

  // depths
  _args = CDR(_args);
  SEXP depths = CAR(_args);
  if (TYPEOF(depths) != INTSXP) {
    Rf_error("typeof(depths) must be 'integer', not '%s'", Rf_type2char(TYPEOF(depths)));
  }
  const int* const depths__ = INTEGER(depths);
  const R_xlen_t depths__len_ = Rf_xlength(depths);

  // vars
  _args = CDR(_args);
  SEXP vars = CAR(_args);
  if (TYPEOF(vars) != INTSXP) {
    Rf_error("typeof(vars) must be 'integer', not '%s'", Rf_type2char(TYPEOF(vars)));
  }
  const int* const vars__ = INTEGER(vars);
  const R_xlen_t vars__len_ = Rf_xlength(vars);

  // thresh
  _args = CDR(_args);
  SEXP thresh = CAR(_args);
  if (TYPEOF(thresh) != REALSXP) {
    Rf_error("typeof(thresh) must be 'double', not '%s'", Rf_type2char(TYPEOF(thresh)));
  }
  const double* const thresh__ = REAL(thresh);
  const R_xlen_t thresh__len_ = Rf_xlength(thresh);

  // p
  _args = CDR(_args);
  SEXP p = CAR(_args);
  if (TYPEOF(p) != INTSXP) {
    Rf_error("typeof(p) must be 'integer', not '%s'", Rf_type2char(TYPEOF(p)));
  }
  const int* const p__ = INTEGER(p);
  const R_xlen_t p__len_ = Rf_xlength(p);

  if (p__len_ != 1)
    Rf_error("length(p) must be 1, not %0.f",
              (double)p__len_);
  const int _as_int_p = Rf_asInteger(p);
  const R_xlen_t out__len_ = (x__dim_1_) * (_as_int_p);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
  int* out__ = INTEGER(out);
  {
    const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
    int* const _dim = INTEGER(_dim_sexp);
    _dim[0] = x__dim_1_;
    _dim[1] = _as_int_p;
    Rf_dimgets(out, _dim_sexp);
  }

  forest_mat(
    x__,
    depths__,
    vars__,
    thresh__,
    p__,
    out__,
    depths__len_,
    thresh__len_,
    vars__len_,
    x__dim_1_,
    x__dim_2_);

  UNPROTECT(2);
  return out;
}

extern void im2col(
  const double* const x__,
  const int* const h__,
  const int* const w__,
  const int* const channels__,
  const int* const size__,
  const int* const stride__,
  double* const out__,
  const R_xlen_t x__len_);

SEXP im2col_(SEXP _args) {
  // x
  _args = CDR(_args);
  SEXP x = CAR(_args);
  if (TYPEOF(x) != REALSXP) {
    Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
  }
  const double* const x__ = REAL(x);
  const R_xlen_t x__len_ = Rf_xlength(x);

  // h
  _args = CDR(_args);
  SEXP h = CAR(_args);
  if (TYPEOF(h) != INTSXP) {
    Rf_error("typeof(h) must be 'integer', not '%s'", Rf_type2char(TYPEOF(h)));
  }
  const int* const h__ = INTEGER(h);
  const R_xlen_t h__len_ = Rf_xlength(h);

  // w
  _args = CDR(_args);
  SEXP w = CAR(_args);
  if (TYPEOF(w) != INTSXP) {
    Rf_error("typeof(w) must be 'integer', not '%s'", Rf_type2char(TYPEOF(w)));
  }
  const int* const w__ = INTEGER(w);
  const R_xlen_t w__len_ = Rf_xlength(w);

  // channels
  _args = CDR(_args);
  SEXP channels = CAR(_args);
  if (TYPEOF(channels) != INTSXP) {
    Rf_error("typeof(channels) must be 'integer', not '%s'", Rf_type2char(TYPEOF(channels)));
  }
  const int* const channels__ = INTEGER(channels);
  const R_xlen_t channels__len_ = Rf_xlength(channels);

  // size
  _args = CDR(_args);
  SEXP size = CAR(_args);
  if (TYPEOF(size) != INTSXP) {
    Rf_error("typeof(size) must be 'integer', not '%s'", Rf_type2char(TYPEOF(size)));
  }
  const int* const size__ = INTEGER(size);
  const R_xlen_t size__len_ = Rf_xlength(size);

  // stride
  _args = CDR(_args);
  SEXP stride = CAR(_args);
  if (TYPEOF(stride) != INTSXP) {
    Rf_error("typeof(stride) must be 'integer', not '%s'", Rf_type2char(TYPEOF(stride)));
  }
  const int* const stride__ = INTEGER(stride);
  const R_xlen_t stride__len_ = Rf_xlength(stride);

  if (h__len_ != 1)
    Rf_error("length(h) must be 1, not %0.f",
              (double)h__len_);
  if (w__len_ != 1)
    Rf_error("length(w) must be 1, not %0.f",
              (double)w__len_);
  if (channels__len_ != 1)
    Rf_error("length(channels) must be 1, not %0.f",
              (double)channels__len_);
  if (size__len_ != 1)
    Rf_error("length(size) must be 1, not %0.f",
              (double)size__len_);
  if (stride__len_ != 1)
    Rf_error("length(stride) must be 1, not %0.f",
              (double)stride__len_);
  const int _as_int_h = Rf_asInteger(h);
  const int _as_int_size = Rf_asInteger(size);
  const int _as_int_stride = Rf_asInteger(stride);
  const int _as_int_w = Rf_asInteger(w);
  const int _as_int_channels = Rf_asInteger(channels);
  const R_xlen_t out__len_ = ((((((R_xlen_t)(_as_int_h - _as_int_size) / (R_xlen_t)_as_int_stride) + 1) * (((R_xlen_t)(_as_int_w - _as_int_size) / (R_xlen_t)_as_int_stride) + 1)) * _as_int_channels)) * ((_as_int_size * _as_int_size));
  SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
  double* out__ = REAL(out);
  {
    const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
    int* const _dim = INTEGER(_dim_sexp);
    _dim[0] = (((((R_xlen_t)(_as_int_h - _as_int_size) / (R_xlen_t)_as_int_stride) + 1) * (((R_xlen_t)(_as_int_w - _as_int_size) / (R_xlen_t)_as_int_stride) + 1)) * _as_int_channels);
    _dim[1] = (_as_int_size * _as_int_size);
    Rf_dimgets(out, _dim_sexp);
  }

  im2col(
    x__,
    h__,
    w__,
    channels__,
    size__,
    stride__,
    out__,
    x__len_);

  UNPROTECT(2);
  return out;
}

extern void pool_max(
  const double* const x__,
  double* const out__,
  const R_len_t x__dim_1_,
  const R_len_t x__dim_2_);

SEXP pool_max_(SEXP _args) {
  // x
  _args = CDR(_args);
  SEXP x = CAR(_args);
  if (TYPEOF(x) != REALSXP) {
    Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
  }
  const double* const x__ = REAL(x);
  const int* const x__dim_ = ({
  SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "x must be a 2D-array, but length(dim(x)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int x__dim_1_ = x__dim_[0];
  const int x__dim_2_ = x__dim_[1];

  const R_xlen_t out__len_ = x__dim_2_;
  SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
  double* out__ = REAL(out);

  pool_max(
    x__,
    out__,
    x__dim_1_,
    x__dim_2_);

  UNPROTECT(1);
  return out;
}

extern void row_prod(
  const double* const x__,
  double* const out__,
  const R_len_t x__dim_1_,
  const R_len_t x__dim_2_);

SEXP row_prod_(SEXP _args) {
  // x
  _args = CDR(_args);
  SEXP x = CAR(_args);
  if (TYPEOF(x) != REALSXP) {
    Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
  }
  const double* const x__ = REAL(x);
  const int* const x__dim_ = ({
  SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "x must be a 2D-array, but length(dim(x)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int x__dim_1_ = x__dim_[0];
  const int x__dim_2_ = x__dim_[1];

  const R_xlen_t out__len_ = x__dim_1_;
  SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
  double* out__ = REAL(out);

  row_prod(
    x__,
    out__,
    x__dim_1_,
    x__dim_2_);

  UNPROTECT(1);
  return out;
}

extern void dist_l2_mat(
  const double* const x__,
  const double* const y__,
  double* const out__,
  const R_len_t x__dim_1_,
  const R_len_t x__dim_2_,
  const R_len_t y__dim_1_);

SEXP dist_l2_mat_(SEXP _args) {
  // x
  _args = CDR(_args);
  SEXP x = CAR(_args);
  if (TYPEOF(x) != REALSXP) {
    Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
  }
  const double* const x__ = REAL(x);
  const int* const x__dim_ = ({
  SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "x must be a 2D-array, but length(dim(x)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int x__dim_1_ = x__dim_[0];
  const int x__dim_2_ = x__dim_[1];

  // y
  _args = CDR(_args);
  SEXP y = CAR(_args);
  if (TYPEOF(y) != REALSXP) {
    Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
  }
  const double* const y__ = REAL(y);
  const int* const y__dim_ = ({
  SEXP dim_ = Rf_getAttrib(y, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "y must be a 2D-array, but length(dim(y)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int y__dim_1_ = y__dim_[0];
  const int y__dim_2_ = y__dim_[1];

  if (x__dim_2_ != y__dim_2_)
    Rf_error("dim(y)[2] must equal dim(x)[2],"
             " but are %0.f and %0.f",
              (double)y__dim_2_, (double)x__dim_2_);
  const R_xlen_t out__len_ = (x__dim_1_) * (y__dim_1_);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
  double* out__ = REAL(out);
  {
    const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
    int* const _dim = INTEGER(_dim_sexp);
    _dim[0] = x__dim_1_;
    _dim[1] = y__dim_1_;
    Rf_dimgets(out, _dim_sexp);
  }

  dist_l2_mat(
    x__,
    y__,
    out__,
    x__dim_1_,
    x__dim_2_,
    y__dim_1_);

  UNPROTECT(2);
  return out;
}

extern void dist_l1_mat(
  const double* const x__,
  const double* const y__,
  double* const out__,
  const R_len_t x__dim_1_,
  const R_len_t x__dim_2_,
  const R_len_t y__dim_1_);

SEXP dist_l1_mat_(SEXP _args) {
  // x
  _args = CDR(_args);
  SEXP x = CAR(_args);
  if (TYPEOF(x) != REALSXP) {
    Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
  }
  const double* const x__ = REAL(x);
  const int* const x__dim_ = ({
  SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "x must be a 2D-array, but length(dim(x)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int x__dim_1_ = x__dim_[0];
  const int x__dim_2_ = x__dim_[1];

  // y
  _args = CDR(_args);
  SEXP y = CAR(_args);
  if (TYPEOF(y) != REALSXP) {
    Rf_error("typeof(y) must be 'double', not '%s'", Rf_type2char(TYPEOF(y)));
  }
  const double* const y__ = REAL(y);
  const int* const y__dim_ = ({
  SEXP dim_ = Rf_getAttrib(y, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "y must be a 2D-array, but length(dim(y)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int y__dim_1_ = y__dim_[0];
  const int y__dim_2_ = y__dim_[1];

  if (x__dim_2_ != y__dim_2_)
    Rf_error("dim(y)[2] must equal dim(x)[2],"
             " but are %0.f and %0.f",
              (double)y__dim_2_, (double)x__dim_2_);
  const R_xlen_t out__len_ = (x__dim_1_) * (y__dim_1_);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, out__len_));
  double* out__ = REAL(out);
  {
    const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
    int* const _dim = INTEGER(_dim_sexp);
    _dim[0] = x__dim_1_;
    _dim[1] = y__dim_1_;
    Rf_dimgets(out, _dim_sexp);
  }

  dist_l1_mat(
    x__,
    y__,
    out__,
    x__dim_1_,
    x__dim_2_,
    y__dim_1_);

  UNPROTECT(2);
  return out;
}

static const R_ExternalMethodDef QuickrEntries[] = {
  {"forest_mat_", (DL_FUNC) &forest_mat_, -1},
  {"im2col_", (DL_FUNC) &im2col_, -1},
  {"pool_max_", (DL_FUNC) &pool_max_, -1},
  {"row_prod_", (DL_FUNC) &row_prod_, -1},
  {"dist_l2_mat_", (DL_FUNC) &dist_l2_mat_, -1},
  {"dist_l1_mat_", (DL_FUNC) &dist_l1_mat_, -1},
  {NULL, NULL, 0}
};

#include <R_ext/Rdynload.h>

void R_init_bases_quick_functions(DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
}

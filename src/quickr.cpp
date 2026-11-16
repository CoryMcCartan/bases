#include <R_ext/Rdynload.h>

extern "C" void R_init_bases_quick_functions(DllInfo* dll);

[[cpp11::init]]
void init_quickr(DllInfo* dll) {
    R_init_bases_quick_functions(dll);
}

#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>


static R_NativePrimitiveArgType thisType[] = {
  INTSXP,  // N
  INTSXP,  // V
};


extern void F77_NAME(dostuff)(int *N, int *V);


static const R_FortranMethodDef FortranEntries[] = {
  {"dostuff", (DL_FUNC) &F77_NAME(dostuff), 2, thisType},
  {NULL, NULL, 0, NULL}
};


void attribute_visible R_init_minWE(DllInfo *info) 
{
  R_registerRoutines(info,
                     NULL,          // .C
                     NULL,          // .Call
                     FortranEntries, // .Fortran
                     NULL);         // .External
  R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);  
}

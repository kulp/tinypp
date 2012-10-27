#ifndef LIBTCC_H
#define LIBTCC_H

struct TCCState;

typedef struct TCCState TCCState;

/*****************************/
/* preprocessor */

/* add include path */
STATIC int tcc_add_include_path(TCCState *s, const char *pathname);

/* define preprocessor symbol 'sym'. Can put optional value */
STATIC void tcc_define_symbol(TCCState *s, const char *sym, const char *value);

/* undefine preprocess symbol 'sym' */
STATIC void tcc_undefine_symbol(TCCState *s, const char *sym);

/*****************************/
/* compiling */

/* add a file (either a C file, dll, an object, a library or an ld
   script). Return -1 if error. */
STATIC int tcc_add_file(TCCState *s, const char *filename);

#define TCC_OUTPUT_PREPROCESS 4 /* preprocessed file (used internally) */
STATIC int tcc_set_output_type(TCCState *s, int output_type);

#endif

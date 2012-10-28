#ifndef LIBTCC_H
#define LIBTCC_H

struct TCCState;
typedef struct TCCState TCCState;

static int tcc_add_include_path(TCCState *s, const char *pathname);
static void tcc_define_symbol(TCCState *s, const char *sym, const char *value);
static void tcc_undefine_symbol(TCCState *s, const char *sym);
static int tcc_add_file(TCCState *s, const char *filename);

#define TCC_OUTPUT_PREPROCESS 4 /* preprocessed file (used internally) */

#endif

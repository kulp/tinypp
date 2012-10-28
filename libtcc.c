/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *  Modifications (c) 2012 Darren Kulp
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "tcc.h"

/********************************************************/
/* global variables */

/* display benchmark infos */
int total_lines;
int total_bytes;

/* parser */
static struct BufferedFile *file;
static int ch, tok;
static CValue tokc;
static CString tokcstr; /* current parsed string, if any */
/* additional informations about token */
static int tok_flags;
#define TOK_FLAG_BOL   0x0001 /* beginning of line before */
#define TOK_FLAG_BOF   0x0002 /* beginning of file before */
#define TOK_FLAG_ENDIF 0x0004 /* a endif was found matching starting #ifdef */
#define TOK_FLAG_EOF   0x0008 /* end of file */

static int *macro_ptr, *macro_ptr_allocated;
static int *unget_saved_macro_ptr;
static int unget_saved_buffer[TOK_MAX_SIZE + 1];
static int unget_buffer_enabled;
static int parse_flags;
#define PARSE_FLAG_PREPROCESS 0x0001 /* activate preprocessing */
#define PARSE_FLAG_TOK_NUM    0x0002 /* return numbers instead of TOK_PPNUM */
#define PARSE_FLAG_LINEFEED   0x0004 /* line feed is returned as a
                                        token. line feed is also
                                        returned at eof */
#define PARSE_FLAG_ASM_COMMENTS 0x0008 /* '#' can be used for line comment */
#define PARSE_FLAG_SPACES     0x0010 /* next() returns space tokens (for -E) */
 

/* expression generation modifiers */
static int tok_ident;
static TokenSym **table_ident;
static TokenSym *hash_ident[TOK_HASH_SIZE];
static char token_buf[STRING_MAX_SIZE + 1];
static Sym *define_stack;
/* symbol allocator */
#define SYM_POOL_NB (8192 / sizeof(Sym))
static Sym *sym_free_first;
static void **sym_pools;
static int nb_sym_pools;

static SValue vstack[VSTACK_SIZE], *vtop;
/* some predefined types */
static CType int_type;

/* use GNU C extensions */
static int gnu_ext = 1;

/* use Tiny C extensions */
static int tcc_ext = 1;

/* XXX: get rid of this ASAP */
static struct TCCState *tcc_state;

/********************************************************/
/* function prototypes */

/* tccpp.c */
static void next(void);
static char *get_tok_str(int v, CValue *cv);

/* tccgen.c */
static int expr_const(void);
static void expr_eq(void);
static void gexpr(void);
static void vpop(void);
static void vswap(void);
static void vdup(void);

static void vstore(void);
static Sym *sym_find(int v);

static void vpushi(int v);

#define AFF_PRINT_ERROR     0x0001 /* print error if file not found */
#define AFF_REFERENCED_DLL  0x0002 /* load a referenced dll from another dll */
#define AFF_PREPROCESS      0x0004 /* preprocess file */
static int tcc_add_file_internal(TCCState *s, const char *filename, int flags);

/********************************************************/
/* global variables */

#define RTLD_LAZY       0x001
#define RTLD_NOW        0x002
#define RTLD_GLOBAL     0x100
#define RTLD_DEFAULT    NULL

typedef struct TCCSyms {
    char *str;
    void *ptr;
} TCCSyms;

#define TCCSYM(a) { #a, &a, },

/********************************************************/

/* copy a string and truncate it. */
static char *pstrcpy(char *buf, int buf_size, const char *s)
{
    char *q, *q_end;
    int c;

    if (buf_size > 0) {
        q = buf;
        q_end = buf + buf_size - 1;
        while (q < q_end) {
            c = *s++;
            if (c == '\0')
                break;
            *q++ = c;
        }
        *q = '\0';
    }
    return buf;
}

/* strcat and truncate. */
static char *pstrcat(char *buf, int buf_size, const char *s)
{
    int len;
    len = strlen(buf);
    if (len < buf_size) 
        pstrcpy(buf + len, buf_size - len, s);
    return buf;
}

/* extract the basename of a file */
static char *tcc_basename(const char *name)
{
    char *p = strchr(name, 0);
    while (p > name && !IS_PATHSEP(p[-1]))
        --p;
    return p;
}

static char *tcc_fileextension (const char *name)
{
    char *b = tcc_basename(name);
    char *e = strrchr(b, '.');
    return e ? e : strchr(b, 0);
}

/* memory management */

static void tcc_free(void *ptr)
{
    free(ptr);
}

static void *tcc_malloc(unsigned long size)
{
    void *ptr;
    ptr = malloc(size);
    if (!ptr && size)
        error("memory full");
    return ptr;
}

static void *tcc_mallocz(unsigned long size)
{
    void *ptr;
    ptr = tcc_malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static void *tcc_realloc(void *ptr, unsigned long size)
{
    void *ptr1;
    ptr1 = realloc(ptr, size);
    return ptr1;
}

static char *tcc_strdup(const char *str)
{
    char *ptr;
    ptr = tcc_malloc(strlen(str) + 1);
    strcpy(ptr, str);
    return ptr;
}

#define free(p) use_tcc_free(p)
#define malloc(s) use_tcc_malloc(s)
#define realloc(p, s) use_tcc_realloc(p, s)

static void dynarray_add(void ***ptab, int *nb_ptr, void *data)
{
    int nb, nb_alloc;
    void **pp;
    
    nb = *nb_ptr;
    pp = *ptab;
    /* every power of two we double array size */
    if ((nb & (nb - 1)) == 0) {
        if (!nb)
            nb_alloc = 1;
        else
            nb_alloc = nb * 2;
        pp = tcc_realloc(pp, nb_alloc * sizeof(void *));
        if (!pp)
            error("memory full");
        *ptab = pp;
    }
    pp[nb++] = data;
    *nb_ptr = nb;
}

static void dynarray_reset(void *pp, int *n)
{
    void **p;
    for (p = *(void***)pp; *n; ++p, --*n)
        if (*p)
            tcc_free(*p);
    tcc_free(*(void**)pp);
    *(void**)pp = NULL;
}

/* symbol allocator */
static Sym *__sym_malloc(void)
{
    Sym *sym_pool, *sym, *last_sym;
    int i;

    sym_pool = tcc_malloc(SYM_POOL_NB * sizeof(Sym));
    dynarray_add(&sym_pools, &nb_sym_pools, sym_pool);

    last_sym = sym_free_first;
    sym = sym_pool;
    for(i = 0; i < SYM_POOL_NB; i++) {
        sym->next = last_sym;
        last_sym = sym;
        sym++;
    }
    sym_free_first = last_sym;
    return last_sym;
}

static inline Sym *sym_malloc(void)
{
    Sym *sym;
    sym = sym_free_first;
    if (!sym)
        sym = __sym_malloc();
    sym_free_first = sym->next;
    return sym;
}

static inline void sym_free(Sym *sym)
{
    sym->next = sym_free_first;
    sym_free_first = sym;
}

static inline int isid(int c)
{
    return (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        c == '_';
}

static inline int isnum(int c)
{
    return c >= '0' && c <= '9';
}

static inline int isoct(int c)
{
    return c >= '0' && c <= '7';
}

static inline int toup(int c)
{
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 'A';
    else
        return c;
}

static void strcat_vprintf(char *buf, int buf_size, const char *fmt, va_list ap)
{
    int len;
    len = strlen(buf);
    vsnprintf(buf + len, buf_size - len, fmt, ap);
}

static void strcat_printf(char *buf, int buf_size, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    strcat_vprintf(buf, buf_size, fmt, ap);
    va_end(ap);
}

static void error1(TCCState *s1, int is_warning, const char *fmt, va_list ap)
{
    char buf[2048];
    BufferedFile **f;
    
    buf[0] = '\0';
    if (file) {
        for(f = s1->include_stack; f < s1->include_stack_ptr; f++)
            strcat_printf(buf, sizeof(buf), "In file included from %s:%d:\n", 
                          (*f)->filename, (*f)->line_num);
        if (file->line_num > 0) {
            strcat_printf(buf, sizeof(buf), 
                          "%s:%d: ", file->filename, file->line_num);
        } else {
            strcat_printf(buf, sizeof(buf),
                          "%s: ", file->filename);
        }
    } else {
        strcat_printf(buf, sizeof(buf),
                      "tcc: ");
    }
    if (is_warning)
        strcat_printf(buf, sizeof(buf), "warning: ");
    strcat_vprintf(buf, sizeof(buf), fmt, ap);

    if (!s1->error_func) {
        /* default case: stderr */
        fprintf(stderr, "%s\n", buf);
    } else {
        s1->error_func(s1->error_opaque, buf);
    }
    if (!is_warning || s1->warn_error)
        s1->nb_errors++;
}

/* error without aborting current compilation */
static void error_noabort(const char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
}

static void error(const char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
    /* better than nothing: in some cases, we accept to handle errors */
    if (s1->error_set_jmp_enabled) {
        longjmp(s1->error_jmp_buf, 1);
    } else {
        /* XXX: eliminate this someday */
        exit(1);
    }
}

static void expect(const char *msg)
{
    error("%s expected", msg);
}

static void warning(const char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    if (s1->warn_none)
        return;

    va_start(ap, fmt);
    error1(s1, 1, fmt, ap);
    va_end(ap);
}

static void skip(int c)
{
    if (tok != c)
        error("'%c' expected", c);
    next();
}

static void test_lvalue(void)
{
    if (!(vtop->r & VT_LVAL))
        expect("lvalue");
}

/* CString handling */

static void cstr_realloc(CString *cstr, int new_size)
{
    int size;
    void *data;

    size = cstr->size_allocated;
    if (size == 0)
        size = 8; /* no need to allocate a too small first string */
    while (size < new_size)
        size = size * 2;
    data = tcc_realloc(cstr->data_allocated, size);
    if (!data)
        error("memory full");
    cstr->data_allocated = data;
    cstr->size_allocated = size;
    cstr->data = data;
}

/* add a byte */
static inline void cstr_ccat(CString *cstr, int ch)
{
    int size;
    size = cstr->size + 1;
    if (size > cstr->size_allocated)
        cstr_realloc(cstr, size);
    ((unsigned char *)cstr->data)[size - 1] = ch;
    cstr->size = size;
}

static void cstr_cat(CString *cstr, const char *str)
{
    int c;
    for(;;) {
        c = *str;
        if (c == '\0')
            break;
        cstr_ccat(cstr, c);
        str++;
    }
}

/* add a wide char */
static void cstr_wccat(CString *cstr, int ch)
{
    int size;
    size = cstr->size + sizeof(nwchar_t);
    if (size > cstr->size_allocated)
        cstr_realloc(cstr, size);
    *(nwchar_t *)(((unsigned char *)cstr->data) + size - sizeof(nwchar_t)) = ch;
    cstr->size = size;
}

static void cstr_new(CString *cstr)
{
    memset(cstr, 0, sizeof(CString));
}

/* free string and reset it to NULL */
static void cstr_free(CString *cstr)
{
    tcc_free(cstr->data_allocated);
    cstr_new(cstr);
}

#define cstr_reset(cstr) cstr_free(cstr)

/* XXX: unicode ? */
static void add_char(CString *cstr, int c)
{
    if (c == '\'' || c == '\"' || c == '\\') {
        /* XXX: could be more precise if char or string */
        cstr_ccat(cstr, '\\');
    }
    if (c >= 32 && c <= 126) {
        cstr_ccat(cstr, c);
    } else {
        cstr_ccat(cstr, '\\');
        if (c == '\n') {
            cstr_ccat(cstr, 'n');
        } else if (c == '\r') {
            cstr_ccat(cstr, 'r');
        } else {
            cstr_ccat(cstr, '0' + ((c >> 6) & 7));
            cstr_ccat(cstr, '0' + ((c >> 3) & 7));
            cstr_ccat(cstr, '0' + (c & 7));
        }
    }
}

/* push, without hashing */
static Sym *sym_push2(Sym **ps, int v, int t, long c)
{
    Sym *s;
    s = sym_malloc();
    s->v = v;
    s->type.t = t;
    s->c = c;
    s->next = NULL;
    /* add in stack */
    s->prev = *ps;
    *ps = s;
    return s;
}

/* find a symbol and return its associated structure. 's' is the top
   of the symbol stack */
static Sym *sym_find2(Sym *s, int v)
{
    while (s) {
        if (s->v == v)
            return s;
        s = s->prev;
    }
    return NULL;
}

/* structure lookup */
static inline Sym *struct_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_struct;
}

/* find an identifier */
static inline Sym *sym_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_identifier;
}

/* I/O layer */

static BufferedFile *tcc_open(TCCState *s1, const char *filename)
{
    int fd;
    BufferedFile *bf;

    if (strcmp(filename, "-") == 0)
        fd = STDIN_FILENO, filename = "stdin";
    else
        fd = open(filename, O_RDONLY | O_BINARY);
    if (fd < 0)
        return NULL;
    bf = tcc_malloc(sizeof(BufferedFile));
    bf->fd = fd;
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer;
    bf->buffer[0] = CH_EOB; /* put eob symbol */
    pstrcpy(bf->filename, sizeof(bf->filename), filename);
    bf->line_num = 1;
    bf->ifndef_macro = 0;
    bf->ifdef_stack_ptr = s1->ifdef_stack_ptr;
    //    printf("opening '%s'\n", filename);
    return bf;
}

static void tcc_close(BufferedFile *bf)
{
    total_lines += bf->line_num;
    close(bf->fd);
    tcc_free(bf);
}

#include "tccpp.c"
#include "tccgen.c"

/* define a preprocessor symbol. A value can also be provided with the '=' operator */
static void tcc_define_symbol(TCCState *s1, const char *sym, const char *value)
{
    BufferedFile bf1, *bf = &bf1;

    pstrcpy(bf->buffer, IO_BUF_SIZE, sym);
    pstrcat(bf->buffer, IO_BUF_SIZE, " ");
    /* default value */
    if (!value) 
        value = "1";
    pstrcat(bf->buffer, IO_BUF_SIZE, value);
    
    /* init file structure */
    bf->fd = -1;
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer + strlen(bf->buffer);
    *bf->buf_end = CH_EOB;
    bf->filename[0] = '\0';
    bf->line_num = 1;
    file = bf;
    
    s1->include_stack_ptr = s1->include_stack;

    /* parse with define parser */
    ch = file->buf_ptr[0];
    next_nomacro();
    parse_define();
    file = NULL;
}

/* undefine a preprocessor symbol */
static void tcc_undefine_symbol(TCCState *s1, const char *sym)
{
    TokenSym *ts;
    Sym *s;
    ts = tok_alloc(sym, strlen(sym));
    s = define_find(ts->tok);
    /* undefine symbol by putting an invalid name */
    if (s)
        define_undef(s);
}

/* copy code into memory passed in by the caller and do all relocations
   (needed before using tcc_get_symbol()).
   returns -1 on error and required size if ptr is NULL */
static void tcc_cleanup(void)
{
    int i, n;

    if (NULL == tcc_state)
        return;
    tcc_state = NULL;

    /* free -D defines */
    free_defines(NULL);

    /* free tokens */
    n = tok_ident - TOK_IDENT;
    for(i = 0; i < n; i++)
        tcc_free(table_ident[i]);
    tcc_free(table_ident);

    /* free sym_pools */
    dynarray_reset(&sym_pools, &nb_sym_pools);
    /* string buffer */
    cstr_free(&tokcstr);
    /* reset symbol stack */
    sym_free_first = NULL;
    /* cleanup from error/setjmp */
    macro_ptr = NULL;
}

static TCCState *tcc_new(void)
{
    TCCState *s;

    tcc_cleanup();

    s = tcc_mallocz(sizeof(TCCState));
    if (!s)
        return NULL;
    tcc_state = s;
    s->output_type = TCC_OUTPUT_PREPROCESS;
    //s->tcc_lib_path = CONFIG_TCCDIR;

    preprocess_new();

    /* we add dummy defines for some special macros to speed up tests
       and to have working defined() */
    define_push(TOK___LINE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___FILE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___DATE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___TIME__, MACRO_OBJ, NULL, NULL);

    /* no section zero */
    dynarray_add((void ***)&s->sections, &s->nb_sections, NULL);

    return s;
}

static int tcc_add_include_path(TCCState *s1, const char *pathname)
{
    char *pathname1;
    
    pathname1 = tcc_strdup(pathname);
    dynarray_add((void ***)&s1->include_paths, &s1->nb_include_paths, pathname1);
    return 0;
}

static int tcc_add_file_internal(TCCState *s1, const char *filename, int flags)
{
    const char *ext;
    int ret;
    BufferedFile *saved_file;

    /* find source file type with extension */
    ext = tcc_fileextension(filename);
    if (ext[0])
        ext++;

    /* open the file */
    saved_file = file;
    file = tcc_open(s1, filename);
    if (!file) {
        if (flags & AFF_PRINT_ERROR) {
            error_noabort("file '%s' not found", filename);
        }
        ret = -1;
        goto fail1;
    }

    if (flags & AFF_PREPROCESS) {
        ret = tcc_preprocess(s1);
    } else 
        goto fail;

 the_end:
    tcc_close(file);
 fail1:
    file = saved_file;
    return ret;
 fail:
    ret = -1;
    goto the_end;
}

static int tcc_add_file(TCCState *s, const char *filename)
{
    return tcc_add_file_internal(s, filename, AFF_PRINT_ERROR | AFF_PREPROCESS);
}


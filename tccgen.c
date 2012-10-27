/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
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

STATIC void swap(int *p, int *q)
{
    int t;
    t = *p;
    *p = *q;
    *q = t;
}

STATIC void vsetc(CType *type, int r, CValue *vc)
{
    int v;

    if (vtop >= vstack + (VSTACK_SIZE - 1))
        error("memory full");
    /* cannot let cpu flags if other instruction are generated. Also
       avoid leaving VT_JMP anywhere except on the top of the stack
       because it would complicate the code generator. */
    if (vtop >= vstack) {
        v = vtop->r & VT_VALMASK;
        if (v == VT_CMP || (v & ~1) == VT_JMP)
            abort();
            //gv(RC_INT);
    }
    vtop++;
    vtop->type = *type;
    vtop->r = r;
    vtop->r2 = VT_CONST;
    vtop->c = *vc;
}

/* push integer constant */
STATIC void vpushi(int v)
{
    CValue cval;
    cval.i = v;
    vsetc(&int_type, VT_CONST, &cval);
}

/* define a new external reference to a symbol 'v' of type 'u' */
static Sym *external_global_sym(int v, CType *type, int r)
{
    Sym *s;

    s = sym_find(v);
    if (!s) {
        /* push forward reference */
        s = global_identifier_push(v, type->t | VT_EXTERN, 0);
        s->type.ref = type->ref;
        s->r = r | VT_CONST | VT_SYM;
    }
    return s;
}

/* push a reference to global symbol v */
STATIC void vset(CType *type, int r, int v)
{
    CValue cval;

    cval.i = v;
    vsetc(type, r, &cval);
}

STATIC void vswap(void)
{
    SValue tmp;

    tmp = vtop[0];
    vtop[0] = vtop[-1];
    vtop[-1] = tmp;
}

STATIC void vpushv(SValue *v)
{
    if (vtop >= vstack + (VSTACK_SIZE - 1))
        error("memory full");
    vtop++;
    *vtop = *v;
}

STATIC void vdup(void)
{
    vpushv(vtop);
}

/* get address of vtop (vtop MUST BE an lvalue) */
STATIC void gaddrof(void)
{
    vtop->r &= ~VT_LVAL;
    /* tricky: if saved lvalue, then we can go back to lvalue */
    if ((vtop->r & VT_VALMASK) == VT_LLOCAL)
        vtop->r = (vtop->r & ~(VT_VALMASK | VT_LVAL_TYPE)) | VT_LOCAL | VT_LVAL;
}

#ifdef TCC_TARGET_ARM
/* like vrott but in other direction
   In ... I1 -> I(n-1) ... I1 In  [top is right]
 */
STATIC void vnrott(int n)
{
    int i;
    SValue tmp;

    tmp = vtop[-n + 1];
    for(i = n - 1; i > 0; i--)
        vtop[-i] = vtop[-i + 1];
    vtop[0] = tmp;
}
#endif

/* pop stack value */
STATIC void vpop(void)
{
    int v;
    v = vtop->r & VT_VALMASK;
    vtop--;
}

/* handle integer constant optimizations and various machine
   independent opt */
STATIC void gen_opic(int op)
{
    int c1, c2, t1, t2, n;
    SValue *v1, *v2;
    long long l1, l2;
    typedef unsigned long long U;

    v1 = vtop - 1;
    v2 = vtop;
    t1 = v1->type.t & VT_BTYPE;
    t2 = v2->type.t & VT_BTYPE;

    if (t1 == VT_LLONG)
        l1 = v1->c.ll;
    else if (v1->type.t & VT_UNSIGNED)
        l1 = v1->c.ui;
    else
        l1 = v1->c.i;

    if (t2 == VT_LLONG)
        l2 = v2->c.ll;
    else if (v2->type.t & VT_UNSIGNED)
        l2 = v2->c.ui;
    else
        l2 = v2->c.i;

    /* currently, we cannot do computations with forward symbols */
    c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    if (c1 && c2) {
        switch(op) {
        case '+': l1 += l2; break;
        case '-': l1 -= l2; break;
        case '&': l1 &= l2; break;
        case '^': l1 ^= l2; break;
        case '|': l1 |= l2; break;
        case '*': l1 *= l2; break;

        case TOK_PDIV:
        case '/':
        case '%':
        case TOK_UDIV:
        case TOK_UMOD:
            /* if division by zero, generate explicit division */
            if (l2 == 0) {
                if (const_wanted)
                    error("division by zero in constant");
                goto general_case;
            }
            switch(op) {
            default: l1 /= l2; break;
            case '%': l1 %= l2; break;
            case TOK_UDIV: l1 = (U)l1 / l2; break;
            case TOK_UMOD: l1 = (U)l1 % l2; break;
            }
            break;
        case TOK_SHL: l1 <<= l2; break;
        case TOK_SHR: l1 = (U)l1 >> l2; break;
        case TOK_SAR: l1 >>= l2; break;
            /* tests */
        case TOK_ULT: l1 = (U)l1 < (U)l2; break;
        case TOK_UGE: l1 = (U)l1 >= (U)l2; break;
        case TOK_EQ: l1 = l1 == l2; break;
        case TOK_NE: l1 = l1 != l2; break;
        case TOK_ULE: l1 = (U)l1 <= (U)l2; break;
        case TOK_UGT: l1 = (U)l1 > (U)l2; break;
        case TOK_LT: l1 = l1 < l2; break;
        case TOK_GE: l1 = l1 >= l2; break;
        case TOK_LE: l1 = l1 <= l2; break;
        case TOK_GT: l1 = l1 > l2; break;
            /* logical */
        case TOK_LAND: l1 = l1 && l2; break;
        case TOK_LOR: l1 = l1 || l2; break;
        default:
            goto general_case;
        }
        v1->c.ll = l1;
        vtop--;
    } else {
        /* if commutative ops, put c2 as constant */
        if (c1 && (op == '+' || op == '&' || op == '^' || 
                   op == '|' || op == '*')) {
            vswap();
            c2 = c1; //c = c1, c1 = c2, c2 = c;
            l2 = l1; //l = l1, l1 = l2, l2 = l;
        }
        /* Filter out NOP operations like x*1, x-0, x&-1... */
        if (c2 && (((op == '*' || op == '/' || op == TOK_UDIV || 
                     op == TOK_PDIV) && 
                    l2 == 1) ||
                   ((op == '+' || op == '-' || op == '|' || op == '^' || 
                     op == TOK_SHL || op == TOK_SHR || op == TOK_SAR) && 
                    l2 == 0) ||
                   (op == '&' && 
                    l2 == -1))) {
            /* nothing to do */
            vtop--;
        } else if (c2 && (op == '*' || op == TOK_PDIV || op == TOK_UDIV)) {
            /* try to use shifts instead of muls or divs */
            if (l2 > 0 && (l2 & (l2 - 1)) == 0) {
                n = -1;
                while (l2) {
                    l2 >>= 1;
                    n++;
                }
                vtop->c.ll = n;
                if (op == '*')
                    op = TOK_SHL;
                else if (op == TOK_PDIV)
                    op = TOK_SAR;
                else
                    op = TOK_SHR;
            }
            goto general_case;
        } else if (c2 && (op == '+' || op == '-') &&
                   ((vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) ==
                   (VT_CONST | VT_SYM) ||
                   (vtop[-1].r & (VT_VALMASK | VT_LVAL)) == VT_LOCAL)) {
            /* symbol + constant case */
            if (op == '-')
                l2 = -l2;
            vtop--;
            vtop->c.ll += l2;
        } else {
        general_case:
            vtop--;
        }
    }
}

/* generate a floating point operation with constant propagation */
STATIC void gen_opif(int op)
{
    int c1, c2;
    SValue *v1, *v2;
    long double f1, f2;

    v1 = vtop - 1;
    v2 = vtop;
    /* currently, we cannot do computations with forward symbols */
    c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    if (c1 && c2) {
        if (v1->type.t == VT_FLOAT) {
            f1 = v1->c.f;
            f2 = v2->c.f;
        } else if (v1->type.t == VT_DOUBLE) {
            f1 = v1->c.d;
            f2 = v2->c.d;
        } else {
            f1 = v1->c.ld;
            f2 = v2->c.ld;
        }

        /* NOTE: we only do constant propagation if finite number (not
           NaN or infinity) (ANSI spec) */
        if (!ieee_finite(f1) || !ieee_finite(f2))
            goto general_case;

        switch(op) {
        case '+': f1 += f2; break;
        case '-': f1 -= f2; break;
        case '*': f1 *= f2; break;
        case '/': 
            if (f2 == 0.0) {
                if (const_wanted)
                    error("division by zero in constant");
                goto general_case;
            }
            f1 /= f2; 
            break;
            /* XXX: also handles tests ? */
        default:
            goto general_case;
        }
        /* XXX: overflow test ? */
        if (v1->type.t == VT_FLOAT) {
            v1->c.f = f1;
        } else if (v1->type.t == VT_DOUBLE) {
            v1->c.d = f1;
        } else {
            v1->c.ld = f1;
        }
        vtop--;
    } else {
    general_case:
        vtop--;
    }
}

static int pointed_size(CType *type)
{
    int align;
    return type_size(pointed_type(type), &align);
}

static inline int is_null_pointer(SValue *p)
{
    if ((p->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        return 0;
    return ((p->type.t & VT_BTYPE) == VT_INT && p->c.i == 0) ||
        ((p->type.t & VT_BTYPE) == VT_LLONG && p->c.ll == 0);
}

static inline int is_integer_btype(int bt)
{
    return (bt == VT_BYTE || bt == VT_SHORT || 
            bt == VT_INT || bt == VT_LLONG);
}

/* check types for comparison or substraction of pointers */
static void check_comparison_pointer_types(SValue *p1, SValue *p2, int op)
{
    CType *type1, *type2, tmp_type1, tmp_type2;
    int bt1, bt2;
    
    /* null pointers are accepted for all comparisons as gcc */
    if (is_null_pointer(p1) || is_null_pointer(p2))
        return;
    type1 = &p1->type;
    type2 = &p2->type;
    bt1 = type1->t & VT_BTYPE;
    bt2 = type2->t & VT_BTYPE;
    /* accept comparison between pointer and integer with a warning */
    if ((is_integer_btype(bt1) || is_integer_btype(bt2)) && op != '-') {
        if (op != TOK_LOR && op != TOK_LAND )
            warning("comparison between pointer and integer");
        return;
    }

    /* both must be pointers or implicit function pointers */
    if (bt1 == VT_PTR) {
        type1 = pointed_type(type1);
    } else if (bt1 != VT_FUNC) 
        goto invalid_operands;

    if (bt2 == VT_PTR) {
        type2 = pointed_type(type2);
    } else if (bt2 != VT_FUNC) { 
    invalid_operands:
        error("invalid operands to binary %s", get_tok_str(op, NULL));
    }
    if ((type1->t & VT_BTYPE) == VT_VOID || 
        (type2->t & VT_BTYPE) == VT_VOID)
        return;
    tmp_type1 = *type1;
    tmp_type2 = *type2;
    tmp_type1.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
    tmp_type2.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
    if (!is_compatible_types(&tmp_type1, &tmp_type2)) {
        /* gcc-like error if '-' is used */
        if (op == '-')
            goto invalid_operands;
        else
            warning("comparison of distinct pointer types lacks a cast");
    }
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
static void gen_cast(CType *type)
{
    int sbt, dbt, sf, df, c, p;

    /* special delayed cast for char/short */
    /* XXX: in some cases (multiple cascaded casts), it may still
       be incorrect */

    dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
    sbt = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);

    if (sbt != dbt) {
        sf = is_float(sbt);
        df = is_float(dbt);
        c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        p = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM);
        if (c) {
            /* constant case: we can do it now */
            /* XXX: in ISOC, cannot do it if error in convert */
            if (sbt == VT_FLOAT)
                vtop->c.ld = vtop->c.f;
            else if (sbt == VT_DOUBLE)
                vtop->c.ld = vtop->c.d;

            if (df) {
                if ((sbt & VT_BTYPE) == VT_LLONG) {
                    if (sbt & VT_UNSIGNED)
                        vtop->c.ld = vtop->c.ull;
                    else
                        vtop->c.ld = vtop->c.ll;
                } else if(!sf) {
                    if (sbt & VT_UNSIGNED)
                        vtop->c.ld = vtop->c.ui;
                    else
                        vtop->c.ld = vtop->c.i;
                }

                if (dbt == VT_FLOAT)
                    vtop->c.f = (float)vtop->c.ld;
                else if (dbt == VT_DOUBLE)
                    vtop->c.d = (double)vtop->c.ld;
            } else if (sf && dbt == (VT_LLONG|VT_UNSIGNED)) {
                vtop->c.ull = (unsigned long long)vtop->c.ld;
            } else if (sf && dbt == VT_BOOL) {
                vtop->c.i = (vtop->c.ld != 0);
            } else {
                if(sf)
                    vtop->c.ll = (long long)vtop->c.ld;
                else if (sbt == (VT_LLONG|VT_UNSIGNED))
                    vtop->c.ll = vtop->c.ull;
                else if (sbt & VT_UNSIGNED)
                    vtop->c.ll = vtop->c.ui;
                else if (sbt != VT_LLONG)
                    vtop->c.ll = vtop->c.i;

                if (dbt == (VT_LLONG|VT_UNSIGNED))
                    vtop->c.ull = vtop->c.ll;
                else if (dbt == VT_BOOL)
                    vtop->c.i = (vtop->c.ll != 0);
                else if (dbt != VT_LLONG) {
                    int s = 0;
                    if ((dbt & VT_BTYPE) == VT_BYTE)
                        s = 24;
                    else if ((dbt & VT_BTYPE) == VT_SHORT)
                        s = 16;

                    if(dbt & VT_UNSIGNED)
                        vtop->c.ui = ((unsigned int)vtop->c.ll << s) >> s;
                    else
                        vtop->c.i = ((int)vtop->c.ll << s) >> s;
                }
            }
        } else if (p && dbt == VT_BOOL) {
            vtop->r = VT_CONST;
            vtop->c.i = 1;
        }
    } else if ((dbt & VT_BTYPE) == VT_PTR && !(vtop->r & VT_LVAL)) {
        /* if we are casting between pointer types,
           we must update the VT_LVAL_xxx size */
        vtop->r = (vtop->r & ~VT_LVAL_TYPE)
                  | (lvalue_type(type->ref->type.t) & VT_LVAL_TYPE);
    }
    vtop->type = *type;
}

/* return type size. Put alignment at 'a' */
static int type_size(CType *type, int *a)
{
    Sym *s;
    int bt;

    bt = type->t & VT_BTYPE;
    if (bt == VT_STRUCT) {
        /* struct/union */
        s = type->ref;
        *a = s->r;
        return s->c;
    } else if (bt == VT_PTR) {
        if (type->t & VT_ARRAY) {
            int ts;

            s = type->ref;
            ts = type_size(&s->type, a);

            if (ts < 0 && s->c < 0)
                ts = -ts;

            return ts * s->c;
        } else {
            *a = PTR_SIZE;
            return PTR_SIZE;
        }
    } else if (bt == VT_LDOUBLE) {
        *a = LDOUBLE_ALIGN;
        return LDOUBLE_SIZE;
    } else if (bt == VT_DOUBLE || bt == VT_LLONG) {
#ifdef TCC_TARGET_I386
#ifdef TCC_TARGET_PE
        *a = 8;
#else
        *a = 4;
#endif
#elif defined(TCC_TARGET_ARM)
#ifdef TCC_ARM_EABI
        *a = 8; 
#else
        *a = 4;
#endif
#else
        *a = 8;
#endif
        return 8;
    } else if (bt == VT_INT || bt == VT_ENUM || bt == VT_FLOAT) {
        *a = 4;
        return 4;
    } else if (bt == VT_SHORT) {
        *a = 2;
        return 2;
    } else {
        /* char, void, function, _Bool */
        *a = 1;
        return 1;
    }
}

/* return the pointed type of t */
static inline CType *pointed_type(CType *type)
{
    return &type->ref->type;
}

/* modify type so that its it is a pointer to type. */
static void mk_pointer(CType *type)
{
    Sym *s;
    s = sym_push(SYM_FIELD, type, 0, -1);
    type->t = VT_PTR | (type->t & ~VT_TYPE);
    type->ref = s;
}

/* compare function types. OLD functions match any new functions */
static int is_compatible_func(CType *type1, CType *type2)
{
    Sym *s1, *s2;

    s1 = type1->ref;
    s2 = type2->ref;
    if (!is_compatible_types(&s1->type, &s2->type))
        return 0;
    /* check func_call */
    if (FUNC_CALL(s1->r) != FUNC_CALL(s2->r))
        return 0;
    /* XXX: not complete */
    if (s1->c == FUNC_OLD || s2->c == FUNC_OLD)
        return 1;
    if (s1->c != s2->c)
        return 0;
    while (s1 != NULL) {
        if (s2 == NULL)
            return 0;
        if (!is_compatible_parameter_types(&s1->type, &s2->type))
            return 0;
        s1 = s1->next;
        s2 = s2->next;
    }
    if (s2)
        return 0;
    return 1;
}

/* return true if type1 and type2 are the same.  If unqualified is
   true, qualifiers on the types are ignored.

   - enums are not checked as gcc __builtin_types_compatible_p () 
 */
static int compare_types(CType *type1, CType *type2, int unqualified)
{
    int bt1, t1, t2;

    t1 = type1->t & VT_TYPE;
    t2 = type2->t & VT_TYPE;
    if (unqualified) {
        /* strip qualifiers before comparing */
        t1 &= ~(VT_CONSTANT | VT_VOLATILE);
        t2 &= ~(VT_CONSTANT | VT_VOLATILE);
    }
    /* XXX: bitfields ? */
    if (t1 != t2)
        return 0;
    /* test more complicated cases */
    bt1 = t1 & VT_BTYPE;
    if (bt1 == VT_PTR) {
        type1 = pointed_type(type1);
        type2 = pointed_type(type2);
        return is_compatible_types(type1, type2);
    } else if (bt1 == VT_STRUCT) {
        return (type1->ref == type2->ref);
    } else if (bt1 == VT_FUNC) {
        return is_compatible_func(type1, type2);
    } else {
        return 1;
    }
}

/* return true if type1 and type2 are exactly the same (including
   qualifiers). 
*/
static int is_compatible_types(CType *type1, CType *type2)
{
    return compare_types(type1,type2,0);
}

/* return true if type1 and type2 are the same (ignoring qualifiers).
*/
static int is_compatible_parameter_types(CType *type1, CType *type2)
{
    return compare_types(type1,type2,1);
}

/* print a type. If 'varstr' is not NULL, then the variable is also
   printed in the type */
/* XXX: union */
/* XXX: add array and function pointers */
STATIC void type_to_str(char *buf, int buf_size, 
                 CType *type, const char *varstr)
{
    int bt, v, t;
    Sym *s, *sa;
    char buf1[256];
    const char *tstr;

    t = type->t & VT_TYPE;
    bt = t & VT_BTYPE;
    buf[0] = '\0';
    if (t & VT_CONSTANT)
        pstrcat(buf, buf_size, "const ");
    if (t & VT_VOLATILE)
        pstrcat(buf, buf_size, "volatile ");
    if (t & VT_UNSIGNED)
        pstrcat(buf, buf_size, "unsigned ");
    switch(bt) {
    case VT_VOID:
        tstr = "void";
        goto add_tstr;
    case VT_BOOL:
        tstr = "_Bool";
        goto add_tstr;
    case VT_BYTE:
        tstr = "char";
        goto add_tstr;
    case VT_SHORT:
        tstr = "short";
        goto add_tstr;
    case VT_INT:
        tstr = "int";
        goto add_tstr;
    case VT_LONG:
        tstr = "long";
        goto add_tstr;
    case VT_LLONG:
        tstr = "long long";
        goto add_tstr;
    case VT_FLOAT:
        tstr = "float";
        goto add_tstr;
    case VT_DOUBLE:
        tstr = "double";
        goto add_tstr;
    case VT_LDOUBLE:
        tstr = "long double";
    add_tstr:
        pstrcat(buf, buf_size, tstr);
        break;
    case VT_ENUM:
    case VT_STRUCT:
        if (bt == VT_STRUCT)
            tstr = "struct ";
        else
            tstr = "enum ";
        pstrcat(buf, buf_size, tstr);
        v = type->ref->v & ~SYM_STRUCT;
        if (v >= SYM_FIRST_ANOM)
            pstrcat(buf, buf_size, "<anonymous>");
        else
            pstrcat(buf, buf_size, get_tok_str(v, NULL));
        break;
    case VT_FUNC:
        s = type->ref;
        type_to_str(buf, buf_size, &s->type, varstr);
        pstrcat(buf, buf_size, "(");
        sa = s->next;
        while (sa != NULL) {
            type_to_str(buf1, sizeof(buf1), &sa->type, NULL);
            pstrcat(buf, buf_size, buf1);
            sa = sa->next;
            if (sa)
                pstrcat(buf, buf_size, ", ");
        }
        pstrcat(buf, buf_size, ")");
        goto no_var;
    case VT_PTR:
        s = type->ref;
        pstrcpy(buf1, sizeof(buf1), "*");
        if (varstr)
            pstrcat(buf1, sizeof(buf1), varstr);
        type_to_str(buf, buf_size, &s->type, buf1);
        goto no_var;
    }
    if (varstr) {
        pstrcat(buf, buf_size, " ");
        pstrcat(buf, buf_size, varstr);
    }
 no_var: ;
}

/* verify type compatibility to store vtop in 'dt' type, and generate
   casts if needed. */
static void gen_assign_cast(CType *dt)
{
    CType *st, *type1, *type2, tmp_type1, tmp_type2;
    char buf1[256], buf2[256];
    int dbt, sbt;

    st = &vtop->type; /* source type */
    dbt = dt->t & VT_BTYPE;
    sbt = st->t & VT_BTYPE;
    if (dt->t & VT_CONSTANT)
        warning("assignment of read-only location");
    switch(dbt) {
    case VT_PTR:
        /* special cases for pointers */
        /* '0' can also be a pointer */
        if (is_null_pointer(vtop))
            goto type_ok;
        /* accept implicit pointer to integer cast with warning */
        if (is_integer_btype(sbt)) {
            warning("assignment makes pointer from integer without a cast");
            goto type_ok;
        }
        type1 = pointed_type(dt);
        /* a function is implicitely a function pointer */
        if (sbt == VT_FUNC) {
            if ((type1->t & VT_BTYPE) != VT_VOID &&
                !is_compatible_types(pointed_type(dt), st))
                goto error;
            else
                goto type_ok;
        }
        if (sbt != VT_PTR)
            goto error;
        type2 = pointed_type(st);
        if ((type1->t & VT_BTYPE) == VT_VOID || 
            (type2->t & VT_BTYPE) == VT_VOID) {
            /* void * can match anything */
        } else {
            /* exact type match, except for unsigned */
            tmp_type1 = *type1;
            tmp_type2 = *type2;
            tmp_type1.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
            tmp_type2.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
            if (!is_compatible_types(&tmp_type1, &tmp_type2))
                warning("assignment from incompatible pointer type");
        }
        /* check const and volatile */
        if ((!(type1->t & VT_CONSTANT) && (type2->t & VT_CONSTANT)) ||
            (!(type1->t & VT_VOLATILE) && (type2->t & VT_VOLATILE)))
            warning("assignment discards qualifiers from pointer target type");
        break;
    case VT_BYTE:
    case VT_SHORT:
    case VT_INT:
    case VT_LLONG:
        if (sbt == VT_PTR || sbt == VT_FUNC) {
            warning("assignment makes integer from pointer without a cast");
        }
        /* XXX: more tests */
        break;
    case VT_STRUCT:
        tmp_type1 = *dt;
        tmp_type2 = *st;
        tmp_type1.t &= ~(VT_CONSTANT | VT_VOLATILE);
        tmp_type2.t &= ~(VT_CONSTANT | VT_VOLATILE);
        if (!is_compatible_types(&tmp_type1, &tmp_type2)) {
        error:
            type_to_str(buf1, sizeof(buf1), st, NULL);
            type_to_str(buf2, sizeof(buf2), dt, NULL);
            error("cannot cast '%s' to '%s'", buf1, buf2);
        }
        break;
    }
 type_ok:
    gen_cast(dt);
}

/* store vtop in lvalue pushed on stack */
STATIC void vstore(void)
{
    int sbt, dbt, ft, r, t, size, align, bit_size, bit_pos, rc, delayed_cast;

    ft = vtop[-1].type.t;
    sbt = vtop->type.t & VT_BTYPE;
    dbt = ft & VT_BTYPE;
    if (((sbt == VT_INT || sbt == VT_SHORT) && dbt == VT_BYTE) ||
        (sbt == VT_INT && dbt == VT_SHORT)) {
        /* optimize char/short casts */
        delayed_cast = VT_MUSTCAST;
        vtop->type.t = ft & (VT_TYPE & ~(VT_BITFIELD | (-1 << VT_STRUCT_SHIFT)));
        /* XXX: factorize */
        if (ft & VT_CONSTANT)
            warning("assignment of read-only location");
    } else {
        delayed_cast = 0;
        if (!(ft & VT_BITFIELD))
            gen_assign_cast(&vtop[-1].type);
    }

    if (sbt == VT_STRUCT) {
        /* if structure, only generate pointer */
        /* structure assignment : generate memcpy */
        /* XXX: optimize if small size */
        vswap();
        vpop();
        /* leave source on stack */
    } else {
#ifdef CONFIG_TCC_BCHECK
        /* bound check case */
        if (vtop[-1].r & VT_MUSTBOUND) {
            vswap();
            gbound();
            vswap();
        }
#endif
        vswap();
        vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        vtop->r |= delayed_cast;
    }
}

/* convert a function parameter type (array to pointer and function to
   function pointer) */
static inline void convert_parameter_type(CType *pt)
{
    /* remove const and volatile qualifiers (XXX: const could be used
       to indicate a const function parameter */
    pt->t &= ~(VT_CONSTANT | VT_VOLATILE);
    /* array must be transformed to pointer according to ANSI C */
    pt->t &= ~VT_ARRAY;
    if ((pt->t & VT_BTYPE) == VT_FUNC) {
        mk_pointer(pt);
    }
}


/* compute the lvalue VT_LVAL_xxx needed to match type t. */
static int lvalue_type(int t)
{
    int bt, r;
    r = VT_LVAL;
    bt = t & VT_BTYPE;
    if (bt == VT_BYTE || bt == VT_BOOL)
        r |= VT_LVAL_BYTE;
    else if (bt == VT_SHORT)
        r |= VT_LVAL_SHORT;
    else
        return r;
    if (t & VT_UNSIGNED)
        r |= VT_LVAL_UNSIGNED;
    return r;
}

/* indirection with full error checking and bound check */
static void indir(void)
{
    if ((vtop->type.t & VT_BTYPE) != VT_PTR) {
        if ((vtop->type.t & VT_BTYPE) == VT_FUNC)
            return;
        expect("pointer");
    }
    vtop->type = *pointed_type(&vtop->type);
    /* Arrays and functions are never lvalues */
    if (!(vtop->type.t & VT_ARRAY)
        && (vtop->type.t & VT_BTYPE) != VT_FUNC) {
        vtop->r |= lvalue_type(vtop->type.t);
    }
}

/* pass a parameter to a function and do type checking and casting */
static void gfunc_param_typed(Sym *func, Sym *arg)
{
    int func_type;
    CType type;

    func_type = func->c;
    if (func_type == FUNC_OLD ||
        (func_type == FUNC_ELLIPSIS && arg == NULL)) {
        /* default casting : only need to convert float to double */
        if ((vtop->type.t & VT_BTYPE) == VT_FLOAT) {
            type.t = VT_DOUBLE;
            gen_cast(&type);
        }
    } else if (arg == NULL) {
        error("too many arguments to function");
    } else {
        type = arg->type;
        type.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */
        gen_assign_cast(&type);
    }
}

static void vpush_tokc(int t)
{
    CType type;
    type.t = t;
    vsetc(&type, VT_CONST, &tokc);
}

static void unary(void)
{
    int n, t, align, size, r;
    CType type;
    Sym *s;
    AttributeDef ad;

    /* XXX: GCC 2.95.3 does not generate a table although it should be
       better here */
 tok_next:
    switch(tok) {
    case TOK_EXTENSION:
        next();
        goto tok_next;
    case TOK_CINT:
    case TOK_CCHAR: 
    case TOK_LCHAR:
        vpushi(tokc.i);
        next();
        break;
    case TOK_CUINT:
        vpush_tokc(VT_INT | VT_UNSIGNED);
        next();
        break;
    case TOK_CLLONG:
        vpush_tokc(VT_LLONG);
        next();
        break;
    case TOK_CULLONG:
        vpush_tokc(VT_LLONG | VT_UNSIGNED);
        next();
        break;
    case TOK_CFLOAT:
        vpush_tokc(VT_FLOAT);
        next();
        break;
    case TOK_CDOUBLE:
        vpush_tokc(VT_DOUBLE);
        next();
        break;
    case TOK_CLDOUBLE:
        vpush_tokc(VT_LDOUBLE);
        next();
        break;
    case '(':
        next();
        gexpr();
        skip(')');
        break;
    case '*':
        next();
        unary();
        indir();
        break;
    case '&':
        next();
        unary();
        /* functions names must be treated as function pointers,
           except for unary '&' and sizeof. Since we consider that
           functions are not lvalues, we only have to handle it
           there and in function calls. */
        /* arrays can also be used although they are not lvalues */
        if ((vtop->type.t & VT_BTYPE) != VT_FUNC &&
            !(vtop->type.t & VT_ARRAY) && !(vtop->type.t & VT_LLOCAL))
            test_lvalue();
        mk_pointer(&vtop->type);
        gaddrof();
        break;
    case '!':
        next();
        unary();
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            CType boolean;
            boolean.t = VT_BOOL;
            gen_cast(&boolean);
            vtop->c.i = !vtop->c.i;
        }
        break;
    case '~':
        next();
        unary();
        vpushi(-1);
        break;
    case '+':
        next();
        /* in order to force cast, we add zero */
        unary();
        if ((vtop->type.t & VT_BTYPE) == VT_PTR)
            error("pointer not accepted for unary plus");
        vpushi(0);
        break;
    case TOK_INC:
    case TOK_DEC:
        t = tok;
        next();
        unary();
        break;
    case '-':
        next();
        vpushi(0);
        unary();
        break;
    case TOK_LAND:
        // fallthrough
    default:
    tok_identifier:
        t = tok;
        next();
        if (t < TOK_UIDENT)
            expect("identifier");
        s = sym_find(t);
        if (!s) {
            if (tok != '(')
                error("'%s' undeclared", get_tok_str(t, NULL));
            /* for simple function calls, we tolerate undeclared
               external reference to int() function */
            if (tcc_state->warn_implicit_function_declaration)
                warning("implicit declaration of function '%s'",
                        get_tok_str(t, NULL));
            s = external_global_sym(t, &func_old_type, 0); 
        }
        if ((s->type.t & (VT_STATIC | VT_INLINE | VT_BTYPE)) ==
            (VT_STATIC | VT_INLINE | VT_FUNC)) {
            /* if referencing an inline function, then we generate a
               symbol to it if not already done. It will have the
               effect to generate code for it at the end of the
               compilation unit. Inline function as always
               generated in the text section. */
            if (!s->c)
                ;//put_extern_sym(s, text_section, 0, 0);
            r = VT_SYM | VT_CONST;
        } else {
            r = s->r;
        }
        vset(&s->type, r, s->c);
        /* if forward reference, we must point to s */
        if (vtop->r & VT_SYM) {
            vtop->sym = s;
            vtop->c.ul = 0;
        }
        break;
    }
    
    /* post operations */
    while (1) {
        if (tok == TOK_INC || tok == TOK_DEC) {
            next();
        } else if (tok == '.' || tok == TOK_ARROW) {
            /* field */ 
            if (tok == TOK_ARROW) 
                indir();
            test_lvalue();
            gaddrof();
            next();
            /* expect pointer on structure */
            if ((vtop->type.t & VT_BTYPE) != VT_STRUCT)
                expect("struct or union");
            s = vtop->type.ref;
            /* find field */
            tok |= SYM_FIELD;
            while ((s = s->next) != NULL) {
                if (s->v == tok)
                    break;
            }
            if (!s)
                error("field not found: %s",  get_tok_str(tok & ~SYM_FIELD, NULL));
            /* add field offset to pointer */
            vtop->type = char_pointer_type; /* change type to 'char *' */
            vpushi(s->c);
            /* change type to field type, and set to lvalue */
            vtop->type = s->type;
            /* an array is never an lvalue */
            if (!(vtop->type.t & VT_ARRAY)) {
                vtop->r |= lvalue_type(vtop->type.t);
            }
            next();
        } else if (tok == '[') {
            next();
            gexpr();
            indir();
            skip(']');
        } else if (tok == '(') {
            SValue ret;
            Sym *sa;
            int nb_args;

            /* function call  */
            if ((vtop->type.t & VT_BTYPE) != VT_FUNC) {
                /* pointer test (no array accepted) */
                if ((vtop->type.t & (VT_BTYPE | VT_ARRAY)) == VT_PTR) {
                    vtop->type = *pointed_type(&vtop->type);
                    if ((vtop->type.t & VT_BTYPE) != VT_FUNC)
                        goto error_func;
                } else {
                error_func:
                    expect("function pointer");
                }
            } else {
                vtop->r &= ~VT_LVAL; /* no lvalue */
            }
            /* get return type */
            s = vtop->type.ref;
            next();
            sa = s->next; /* first parameter */
            nb_args = 0;
            ret.r2 = VT_CONST;
            {
                ret.type = s->type; 
                /* return in register */
                ret.r = REG_IRET;
                ret.c.i = 0;
            }
            if (tok != ')') {
                for(;;) {
                    expr_eq();
                    gfunc_param_typed(s, sa);
                    nb_args++;
                    if (sa)
                        sa = sa->next;
                    if (tok == ')')
                        break;
                    skip(',');
                }
            }
            if (sa)
                error("too few arguments to function");
            skip(')');
            vtop -= (nb_args + 1);
            /* return value */
            vsetc(&ret.type, ret.r, &ret.c);
            vtop->r2 = ret.r2;
        } else {
            break;
        }
    }
}

static void uneq(void)
{
    int t;
    
    unary();
    if (tok == '=' ||
        (tok >= TOK_A_MOD && tok <= TOK_A_DIV) ||
        tok == TOK_A_XOR || tok == TOK_A_OR ||
        tok == TOK_A_SHL || tok == TOK_A_SAR) {
        test_lvalue();
        t = tok;
        next();
        if (t == '=') {
            expr_eq();
        } else {
            vdup();
            expr_eq();
        }
        vstore();
    }
}

static void expr_prod(void)
{
    int t;

    uneq();
    while (tok == '*' || tok == '/' || tok == '%') {
        t = tok;
        next();
        uneq();
    }
}

static void expr_sum(void)
{
    int t;

    expr_prod();
    while (tok == '+' || tok == '-') {
        t = tok;
        next();
        expr_prod();
    }
}

static void expr_shift(void)
{
    int t;

    expr_sum();
    while (tok == TOK_SHL || tok == TOK_SAR) {
        t = tok;
        next();
        expr_sum();
    }
}

static void expr_cmp(void)
{
    int t;

    expr_shift();
    while ((tok >= TOK_ULE && tok <= TOK_GT) ||
           tok == TOK_ULT || tok == TOK_UGE) {
        t = tok;
        next();
        expr_shift();
    }
}

static void expr_cmpeq(void)
{
    int t;

    expr_cmp();
    while (tok == TOK_EQ || tok == TOK_NE) {
        t = tok;
        next();
        expr_cmp();
    }
}

static void expr_and(void)
{
    expr_cmpeq();
    while (tok == '&') {
        next();
        expr_cmpeq();
    }
}

static void expr_xor(void)
{
    expr_and();
    while (tok == '^') {
        next();
        expr_and();
    }
}

static void expr_or(void)
{
    expr_xor();
    while (tok == '|') {
        next();
        expr_xor();
    }
}

/* XXX: fix this mess */
static void expr_land_const(void)
{
    expr_or();
    while (tok == TOK_LAND) {
        next();
        expr_or();
    }
}

/* XXX: fix this mess */
static void expr_lor_const(void)
{
    expr_land_const();
    while (tok == TOK_LOR) {
        next();
        expr_land_const();
    }
}

/* XXX: better constant handling */
static void expr_eq(void)
{
    int tt, u, r1, r2, rc, t1, t2, bt1, bt2;
    SValue sv;
    CType type, type1, type2;

    if (const_wanted) {
        expr_lor_const();
        if (tok == '?') {
            CType boolean;
            int c;
            boolean.t = VT_BOOL;
            vdup();
            gen_cast(&boolean);
            c = vtop->c.i;
            vpop();
            next();
            if (tok != ':' || !gnu_ext) {
                vpop();
                gexpr();
            }
            if (!c)
                vpop();
            skip(':');
            expr_eq();
            if (c)
                vpop();
        }
    }
}

static void gexpr(void)
{
    while (1) {
        expr_eq();
        if (tok != ',')
            break;
        vpop();
        next();
    }
}

/* parse a constant expression and return value in vtop.  */
static void expr_const1(void)
{
    int a;
    a = const_wanted;
    const_wanted = 1;
    expr_eq();
    const_wanted = a;
}

/* parse an integer constant and return its value. */
static int expr_const(void)
{
    int c;
    expr_const1();
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect("constant expression");
    c = vtop->c.i;
    vpop();
    return c;
}

/* return the label token if current token is a label, otherwise
   return zero */
static int is_label(void)
{
    int last_tok;

    /* fast test first */
    if (tok < TOK_UIDENT)
        return 0;
    /* no need to save tokc because tok is an identifier */
    last_tok = tok;
    next();
    if (tok == ':') {
        next();
        return last_tok;
    } else {
        unget_tok(last_tok);
        return 0;
    }
}

/* t is the array or struct type. c is the array or struct
   address. cur_index/cur_field is the pointer to the current
   value. 'size_only' is true if only size info is needed (only used
   in arrays) */
static void decl_designator(CType *type, Section *sec, unsigned long c, 
                            int *cur_index, Sym **cur_field, 
                            int size_only)
{
    Sym *s, *f;
    int notfirst, index, index_last, align, l, nb_elems, elem_size;
    CType type1;

    notfirst = 0;
    elem_size = 0;
    nb_elems = 1;
    if (gnu_ext && (l = is_label()) != 0)
        goto struct_field;
    while (tok == '[' || tok == '.') {
        if (tok == '[') {
            if (!(type->t & VT_ARRAY))
                expect("array type");
            s = type->ref;
            next();
            index = expr_const();
            if (index < 0 || (s->c >= 0 && index >= s->c))
                expect("invalid index");
            if (tok == TOK_DOTS && gnu_ext) {
                next();
                index_last = expr_const();
                if (index_last < 0 || 
                    (s->c >= 0 && index_last >= s->c) ||
                    index_last < index)
                    expect("invalid index");
            } else {
                index_last = index;
            }
            skip(']');
            if (!notfirst)
                *cur_index = index_last;
            type = pointed_type(type);
            elem_size = type_size(type, &align);
            c += index * elem_size;
            /* NOTE: we only support ranges for last designator */
            nb_elems = index_last - index + 1;
            if (nb_elems != 1) {
                notfirst = 1;
                break;
            }
        } else {
            next();
            l = tok;
            next();
        struct_field:
            if ((type->t & VT_BTYPE) != VT_STRUCT)
                expect("struct/union type");
            s = type->ref;
            l |= SYM_FIELD;
            f = s->next;
            while (f) {
                if (f->v == l)
                    break;
                f = f->next;
            }
            if (!f)
                expect("field");
            if (!notfirst)
                *cur_field = f;
            /* XXX: fix this mess by using explicit storage field */
            type1 = f->type;
            type1.t |= (type->t & ~VT_TYPE);
            type = &type1;
            c += f->c;
        }
        notfirst = 1;
    }
    if (notfirst) {
        if (tok == '=') {
            next();
        } else {
            if (!gnu_ext)
                expect("=");
        }
    } else {
        if (type->t & VT_ARRAY) {
            index = *cur_index;
            type = pointed_type(type);
            c += index * type_size(type, &align);
        } else {
            f = *cur_field;
            if (!f)
                error("too many field init");
            /* XXX: fix this mess by using explicit storage field */
            type1 = f->type;
            type1.t |= (type->t & ~VT_TYPE);
            type = &type1;
            c += f->c;
        }
    }
    decl_initializer(type, sec, c, 0, size_only);

    /* XXX: make it more general */
    if (!size_only && nb_elems > 1) {
        unsigned long c_end;
        uint8_t *src, *dst;
        int i;

        if (!sec)
            error("range init not supported yet for dynamic storage");
        c_end = c + nb_elems * elem_size;
        if (c_end > sec->data_allocated)
            section_realloc(sec, c_end);
        src = sec->data + c;
        dst = src;
        for(i = 1; i < nb_elems; i++) {
            dst += elem_size;
            memcpy(dst, src, elem_size);
        }
    }
}

#define EXPR_VAL   0
#define EXPR_CONST 1
#define EXPR_ANY   2

/* store a value or an expression directly in global data or in local array */
static void init_putv(CType *type, Section *sec, unsigned long c, 
                      int v, int expr_type)
{
    int saved_global_expr, bt, bit_pos, bit_size;
    void *ptr;
    unsigned long long bit_mask;
    CType dtype;

    switch(expr_type) {
    case EXPR_VAL:
        vpushi(v);
        break;
    case EXPR_CONST:
        /* compound literals must be allocated globally in this case */
        saved_global_expr = global_expr;
        global_expr = 1;
        expr_const1();
        global_expr = saved_global_expr;
        /* NOTE: symbols are accepted */
        if ((vtop->r & (VT_VALMASK | VT_LVAL)) != VT_CONST)
            error("initializer element is not constant");
        break;
    case EXPR_ANY:
        expr_eq();
        break;
    }
    
    dtype = *type;
    dtype.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */

    if (sec) {
        /* XXX: not portable */
        /* XXX: generate error if incorrect relocation */
        gen_assign_cast(&dtype);
        bt = type->t & VT_BTYPE;
        /* we'll write at most 12 bytes */
        if (c + 12 > sec->data_allocated) {
            section_realloc(sec, c + 12);
        }
        ptr = sec->data + c;
        /* XXX: make code faster ? */
        if (!(type->t & VT_BITFIELD)) {
            bit_pos = 0;
            bit_size = 32;
            bit_mask = -1LL;
        } else {
            bit_pos = (vtop->type.t >> VT_STRUCT_SHIFT) & 0x3f;
            bit_size = (vtop->type.t >> (VT_STRUCT_SHIFT + 6)) & 0x3f;
            bit_mask = (1LL << bit_size) - 1;
        }
        if ((vtop->r & VT_SYM) &&
            (bt == VT_BYTE ||
             bt == VT_SHORT ||
             bt == VT_DOUBLE ||
             bt == VT_LDOUBLE ||
             bt == VT_LLONG ||
             (bt == VT_INT && bit_size != 32)))
            error("initializer element is not computable at load time");
        switch(bt) {
        case VT_BOOL:
            vtop->c.i = (vtop->c.i != 0);
        case VT_BYTE:
            *(char *)ptr |= (vtop->c.i & bit_mask) << bit_pos;
            break;
        case VT_SHORT:
            *(short *)ptr |= (vtop->c.i & bit_mask) << bit_pos;
            break;
        case VT_DOUBLE:
            *(double *)ptr = vtop->c.d;
            break;
        case VT_LDOUBLE:
            *(long double *)ptr = vtop->c.ld;
            break;
        case VT_LLONG:
            *(long long *)ptr |= (vtop->c.ll & bit_mask) << bit_pos;
            break;
        default:
            if (vtop->r & VT_SYM) {
                // XXX
                // greloc(sec, vtop->sym, c, R_DATA_32);
            }
            *(int *)ptr |= (vtop->c.i & bit_mask) << bit_pos;
            break;
        }
        vtop--;
    } else {
        vset(&dtype, VT_LOCAL|VT_LVAL, c);
        vswap();
        vstore();
        vpop();
    }
}

/* put zeros for variable based init */
/* 't' contains the type and storage info. 'c' is the offset of the
   object in section 'sec'. If 'sec' is NULL, it means stack based
   allocation. 'first' is true if array '{' must be read (multi
   dimension implicit array init handling). 'size_only' is true if
   size only evaluation is wanted (only for arrays). */
static void decl_initializer(CType *type, Section *sec, unsigned long c, 
                             int first, int size_only)
{
    int index, array_length, n, no_oblock, nb, parlevel, i;
    int size1, align1, expr_type;
    Sym *s, *f;
    CType *t1;

    if (type->t & VT_ARRAY) {
        s = type->ref;
        n = s->c;
        array_length = 0;
        t1 = pointed_type(type);
        size1 = type_size(t1, &align1);

        no_oblock = 1;
        if ((first && tok != TOK_LSTR && tok != TOK_STR) || 
            tok == '{') {
            skip('{');
            no_oblock = 0;
        }

        {
            index = 0;
            while (tok != '}') {
                decl_designator(type, sec, c, &index, NULL, size_only);
                if (n >= 0 && index >= n)
                    error("index too large");
                /* must put zero in holes (note that doing it that way
                   ensures that it even works with designators) */
                index++;
                if (index > array_length)
                    array_length = index;
                /* special test for multi dimensional arrays (may not
                   be strictly correct if designators are used at the
                   same time) */
                if (index >= n && no_oblock)
                    break;
                if (tok == '}')
                    break;
                skip(',');
            }
        }
        if (!no_oblock)
            skip('}');
        /* patch type size if needed */
        if (n < 0)
            s->c = array_length;
    } else {
        /* currently, we always use constant expression for globals
           (may change for scripting case) */
        expr_type = EXPR_CONST;
        if (!sec)
            expr_type = EXPR_ANY;
        init_putv(type, sec, c, 0, expr_type);
    }
}


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

STATIC void vsetc(CType *type, int r, CValue *vc)
{
    int v;

    if (vtop >= vstack + (VSTACK_SIZE - 1))
        error("memory full");
    if (vtop >= vstack) {
        v = vtop->r & VT_VALMASK;
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

/* pop stack value */
STATIC void vpop(void)
{
    int v;
    v = vtop->r & VT_VALMASK;
    vtop--;
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
static void gen_cast(CType *type)
{
    int sbt, dbt, c, p;

    /* special delayed cast for char/short */
    /* XXX: in some cases (multiple cascaded casts), it may still
       be incorrect */

    dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
    sbt = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);

    if (sbt != dbt) {
        c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        p = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM);
        if (c) {
            /* constant case: we can do it now */
            /* XXX: in ISOC, cannot do it if error in convert */
            if (sbt == VT_FLOAT)
                vtop->c.ld = vtop->c.f;
            else if (sbt == VT_DOUBLE)
                vtop->c.ld = vtop->c.d;

            {
                if (sbt == (VT_LLONG|VT_UNSIGNED))
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
    }
    vtop->type = *type;
}

/* store vtop in lvalue pushed on stack */
STATIC void vstore(void)
{
    int sbt, dbt, ft, delayed_cast;

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
    }

    if (sbt == VT_STRUCT) {
        /* if structure, only generate pointer */
        /* structure assignment : generate memcpy */
        /* XXX: optimize if small size */
        vswap();
        vpop();
        /* leave source on stack */
    } else {
        vswap();
        vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        vtop->r |= delayed_cast;
    }
}

static void vpush_tokc(int t)
{
    CType type = { 0 };
    type.t = t;
    vsetc(&type, VT_CONST, &tokc);
}

static void unary(void)
{
    int t;

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
        break;
#if 0
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
#endif
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
        t = tok;
        next();
        if (t < TOK_UIDENT)
            expect("identifier");
        break;
    }
    
    /* post operations */
    while (1) {
        if (tok == TOK_INC || tok == TOK_DEC) {
            next();
        } else if (tok == '.' || tok == TOK_ARROW) {
            /* field */ 
            test_lvalue();
            gaddrof();
            next();
            /* find field */
            tok |= SYM_FIELD;
            next();
        } else if (tok == '[') {
            next();
            gexpr();
            skip(']');
        } else if (tok == '(') {
            next();
            if (tok != ')') {
                for(;;) {
                    expr_eq();
                    if (tok == ')')
                        break;
                    skip(',');
                }
            }
            skip(')');
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

/* parse an integer constant and return its value. */
static int expr_const(void)
{
    int c;
    expr_eq();
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect("constant expression");
    c = vtop->c.i;
    vpop();
    return c;
}


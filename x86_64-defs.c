#define STATIC static
#define NB_REGS             5
#define RC_INT     0x0001 /* generic integer register */
#define RC_FLOAT   0x0002 /* generic float register */
#define RC_RAX     0x0004
#define RC_RCX     0x0008
#define RC_RDX     0x0010
#define RC_XMM0    0x0020
#define RC_ST0     0x0040 /* only for long double */
#define RC_IRET    RC_RAX /* function return: integer register */
#define RC_LRET    RC_RDX /* function return: second integer register */
#define RC_FRET    RC_XMM0 /* function return: float register */
#define REX_BASE(reg) (((reg) >> 3) & 1)
#define REG_VALUE(reg) ((reg) & 7)
#define REG_IRET TREG_RAX /* single word int return register */
#define REG_LRET TREG_RDX /* second word return register (for long long) */
#define REG_FRET TREG_XMM0 /* float return register */
#define INVERT_FUNC_PARAMS
#define PTR_SIZE 8
#define LDOUBLE_SIZE  16
#define LDOUBLE_ALIGN 8
#define MAX_ALIGN     8
/* ELF defines */
#define EM_TCC_TARGET EM_X86_64
#define R_DATA_32   R_X86_64_64
#define R_JMP_SLOT  R_X86_64_JUMP_SLOT
#define R_COPY      R_X86_64_COPY
#define ELF_START_ADDR 0x08048000
#define ELF_PAGE_SIZE  0x1000
#define psym oad
#define FUNC_PROLOG_SIZE 11
/* pretty names for the registers */
enum {
    TREG_RAX = 0,
    TREG_RCX = 1,
    TREG_RDX = 2,
    TREG_RSI = 6,
    TREG_RDI = 7,
    TREG_R8  = 8,
    TREG_R9  = 9,
    TREG_R10 = 10,
    TREG_R11 = 11,

    TREG_XMM0 = 3,
    TREG_ST0 = 4,

    TREG_MEM = 0x10,
};

int reg_classes[NB_REGS] = {
    /* eax */ RC_INT | RC_RAX,
    /* ecx */ RC_INT | RC_RCX,
    /* edx */ RC_INT | RC_RDX,
    /* xmm0 */ RC_FLOAT | RC_XMM0,
    /* st0 */ RC_ST0,
};


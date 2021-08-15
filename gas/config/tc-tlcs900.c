/* tc-tlcs900.c -- Assemble code for the TOSHIBA TLCS900
   Copyright (C) 2005-2021 Free Software Foundation, Inc.
   Contributed by Felipe Sanches <juca@members.fsf.org>

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include "as.h"
#include "safe-ctype.h"
#include "subsegs.h"
#include "elf/tlcs900.h"
#include "dwarf2dbg.h"
#include "dw2gencfi.h"

/* Exported constants.  */
const char comment_chars[] = ";\0";
const char line_comment_chars[] = "#;\0";
const char line_separator_chars[] = "\0";
const char EXP_CHARS[] = "eE\0";
const char FLT_CHARS[] = "RrDdFfSsHh\0";

/* For machine specific options.  */
const char * md_shortopts = ""; /* None yet.  */

enum options
{
  OPTION_MARCH = OPTION_MD_BASE,
  OPTION_MACH_TLCS900,
  OPTION_MACH_TLCS900L,
  OPTION_MACH_INST,
  OPTION_MACH_NO_INST,
  OPTION_MACH_IUD,
  OPTION_MACH_WUD,
  OPTION_MACH_FUD,
  OPTION_MACH_IUP,
  OPTION_MACH_WUP,
  OPTION_MACH_FUP,
  OPTION_FP_SINGLE_FORMAT,
  OPTION_FP_DOUBLE_FORMAT,
  OPTION_COMPAT_LL_PREFIX,
  OPTION_COMPAT_COLONLESS,
  OPTION_COMPAT_SDCC
};

#define INS_TLCS900      (1 << 0)
#define INS_TLCS900L    (1 << 1)
#define INS_MARCH_MASK 0xffff

#define INS_IDX_HALF (1 << 16)
#define INS_IN_F_C   (1 << 17)
#define INS_OUT_C_0  (1 << 18)
#define INS_SLI      (1 << 19)
#define INS_ROT_II_LD (1 << 20)  /* instructions like SLA (ii+d),r; which is: LD r,(ii+d); SLA r; LD (ii+d),r */
#define INS_TUNE_MASK 0xffff0000

#define INS_ALL 0
#define INS_UNDOC (INS_IDX_HALF | INS_IN_F_C)
#define INS_UNPORT (INS_OUT_C_0 | INS_SLI | INS_ROT_II_LD)

struct option md_longopts[] =
{
  { "march",     required_argument, NULL, OPTION_MARCH},
  { "tlcs900",       no_argument, NULL, OPTION_MACH_TLCS900},
  { "tlcs900l",      no_argument, NULL, OPTION_MACH_TLCS900L},
  { "fp-s",      required_argument, NULL, OPTION_FP_SINGLE_FORMAT},
  { "fp-d",      required_argument, NULL, OPTION_FP_DOUBLE_FORMAT},
  { "strict",    no_argument, NULL, OPTION_MACH_FUD},
  { "full",      no_argument, NULL, OPTION_MACH_IUP},
  { "with-inst", required_argument, NULL, OPTION_MACH_INST},
  { "Wnins",     required_argument, NULL, OPTION_MACH_INST},
  { "without-inst", required_argument, NULL, OPTION_MACH_NO_INST},
  { "local-prefix", required_argument, NULL, OPTION_COMPAT_LL_PREFIX},
  { "colonless", no_argument, NULL, OPTION_COMPAT_COLONLESS},
  { "sdcc",      no_argument, NULL, OPTION_COMPAT_SDCC},
  { "Fins",      required_argument, NULL, OPTION_MACH_NO_INST},
  { "ignore-undocumented-instructions", no_argument, NULL, OPTION_MACH_IUD },
  { "Wnud",  no_argument, NULL, OPTION_MACH_IUD },
  { "warn-undocumented-instructions",  no_argument, NULL, OPTION_MACH_WUD },
  { "Wud",  no_argument, NULL, OPTION_MACH_WUD },
  { "forbid-undocumented-instructions", no_argument, NULL, OPTION_MACH_FUD },
  { "Fud",  no_argument, NULL, OPTION_MACH_FUD },
  { "ignore-unportable-instructions", no_argument, NULL, OPTION_MACH_IUP },
  { "Wnup",  no_argument, NULL, OPTION_MACH_IUP },
  { "warn-unportable-instructions",  no_argument, NULL, OPTION_MACH_WUP },
  { "Wup",  no_argument, NULL, OPTION_MACH_WUP },
  { "forbid-unportable-instructions", no_argument, NULL, OPTION_MACH_FUP },
  { "Fup",  no_argument, NULL, OPTION_MACH_FUP },

  { NULL, no_argument, NULL, 0 }
} ;

size_t md_longopts_size = sizeof (md_longopts);

static int cpu_mode = 0;
extern int coff_flags;
/* Instruction classes that silently assembled.  */
static int ins_ok = INS_TLCS900 | INS_UNDOC;
/* Instruction classes that generate errors.  */
static int ins_err = ~(INS_TLCS900 | INS_UNDOC);
/* accept SDCC specific instruction encoding */
static int sdcc_compat = 0;
/* accept colonless labels */
static int colonless_labels = 0;
/* local label prefix (NULL - default) */
static const char *local_label_prefix = NULL;
/* floating point support */
typedef const char *(*str_to_float_t)(char *litP, int *sizeP);
static str_to_float_t str_to_float;
static str_to_float_t str_to_double;

/* mode of current instruction */
#define INST_MODE_S 0      /* short data mode */
#define INST_MODE_IS 0     /* short instruction mode */
#define INST_MODE_L 2      /* long data mode */
#define INST_MODE_IL 1     /* long instruction mode */
#define INST_MODE_FORCED 4 /* CPU mode changed by instruction suffix*/
static char inst_mode;

struct match_info
{
  const char *name;
  int ins_ok;
  int ins_err;
  int cpu_mode;
  const char *comment;
};

static const struct match_info
match_cpu_table [] =
{
  {"tlcs900",     INS_TLCS900, 0, 0, "TOSHIBA TLCS900" },
  {"tlcs900l",    INS_TLCS900L, 0, 0, "TOSHIBA TLCS900/L" },
};

static int signed_overflow (signed long value, unsigned bitsize);
static int unsigned_overflow (unsigned long value, unsigned bitsize);
static int is_overflow (long value, unsigned bitsize);

static void
setup_march (const char *name, int *ok, int *err, int *mode)
{
  unsigned i;
  size_t len = strlen(name);
  for (i = 0; i < ARRAY_SIZE (match_cpu_table); ++i)
    if (!strncasecmp (name, match_cpu_table[i].name, len)
	&& strlen (match_cpu_table[i].name) == len)
      {
	*ok = match_cpu_table[i].ins_ok;
	*err = match_cpu_table[i].ins_err;
	*mode = match_cpu_table[i].cpu_mode;
	break;
      }

  if (i >= ARRAY_SIZE (match_cpu_table))
    as_fatal (_("Invalid CPU is specified: %s"), name);
}

int
md_parse_option (int c, const char* arg)
{
  switch (c)
    {
    default:
      return 0;
    case OPTION_MARCH:
      setup_march (arg, & ins_ok, & ins_err, & cpu_mode);
      break;
    case OPTION_MACH_TLCS900:
      setup_march ("tlcs900", & ins_ok, & ins_err, & cpu_mode);
      break;
    case OPTION_MACH_TLCS900L:
      setup_march ("tlcs900l", & ins_ok, & ins_err, & cpu_mode);
      break;
    }

  return 1;
}

void
md_show_usage (FILE * f)
{
  unsigned i;
  fprintf (f, _("\n\
CPU model options:\n\
  -march=CPU\n\
\t\t\t  generate code for CPU, where CPU is one of:\n"));
  for (i = 0; i < ARRAY_SIZE(match_cpu_table); ++i)
    fprintf (f, "  %-8s\t\t  %s\n", match_cpu_table[i].name, match_cpu_table[i].comment);
  fprintf (f, _("\n\
Default: -march=tlcs900\n"));
}

static symbolS * zero;

struct reg_entry
{
  const char* name;
  int number;
  int isa;
};
#define R_STACKABLE (0x80)
#define R_ARITH     (0x40)
#define R_IX        (0x20)
#define R_IY        (0x10)
#define R_INDEX     (R_IX | R_IY)

#define REG_A (7)
#define REG_B (0)
#define REG_C (1)
#define REG_D (2)
#define REG_E (3)
#define REG_H (4)
#define REG_L (5)
#define REG_F (6 | 8)
#define REG_I (9)
#define REG_R (10)
#define REG_MB (11)

#define REG_AF (3 | R_STACKABLE)
#define REG_BC (0 | R_STACKABLE | R_ARITH)
#define REG_DE (1 | R_STACKABLE | R_ARITH)
#define REG_HL (2 | R_STACKABLE | R_ARITH)
#define REG_IX (REG_HL | R_IX)
#define REG_IY (REG_HL | R_IY)
#define REG_SP (3 | R_ARITH)

static const struct reg_entry regtable[] =
{
  {"a",   REG_A,        INS_ALL },
  {"af",  REG_AF,       INS_ALL },
  {"b",   REG_B,        INS_ALL },
  {"bc",  REG_BC,       INS_ALL },
  {"c",   REG_C,        INS_ALL },
  {"d",   REG_D,        INS_ALL },
  {"de",  REG_DE,       INS_ALL },
  {"e",   REG_E,        INS_ALL },
  {"f",   REG_F,        INS_IN_F_C },
  {"h",   REG_H,        INS_ALL },
  {"hl",  REG_HL,       INS_ALL },
  {"i",   REG_I,        INS_ALL },
  {"ix",  REG_IX,       INS_ALL },
  {"ixh", REG_H | R_IX, INS_IDX_HALF },
  {"ixl", REG_L | R_IX, INS_IDX_HALF },
  {"iy",  REG_IY,       INS_ALL },
  {"iyh", REG_H | R_IY, INS_IDX_HALF },
  {"iyl", REG_L | R_IY, INS_IDX_HALF },
  {"l",   REG_L,        INS_ALL },
//  {"mb",  REG_MB,       INS_ALL },
  {"r",   REG_R,        INS_ALL },
  {"sp",  REG_SP,       INS_ALL },
} ;

#define BUFLEN 8 /* Large enough for any keyword.  */

void
md_begin (void)
{
  expressionS nul, reg;
  char * p;
  unsigned int i, j, k;
  char buf[BUFLEN];

  memset (&reg, 0, sizeof (reg));
  memset (&nul, 0, sizeof (nul));

  reg.X_op = O_register;
  reg.X_md = 0;
  reg.X_add_symbol = reg.X_op_symbol = 0;
  for ( i = 0 ; i < ARRAY_SIZE ( regtable ) ; ++i )
    {
      if (regtable[i].isa && !(regtable[i].isa & ins_ok))
	continue;
      reg.X_add_number = regtable[i].number;
      k = strlen ( regtable[i].name );
      buf[k] = 0;
      if ( k+1 < BUFLEN )
        {
          for ( j = ( 1<<k ) ; j ; --j )
            {
              for ( k = 0 ; regtable[i].name[k] ; ++k )
                {
                  buf[k] = ( j & ( 1<<k ) ) ? TOUPPER (regtable[i].name[k]) : regtable[i].name[k];
                }
              symbolS * psym = symbol_find_or_make (buf);
	      S_SET_SEGMENT (psym, reg_section);
	      symbol_set_value_expression (psym, &reg);
            }
        }
    }
  p = input_line_pointer;
  input_line_pointer = (char *) "0";
  nul.X_md=0;
  expression (& nul);
  input_line_pointer = p;
  zero = make_expr_symbol (& nul);
  /* We do not use relaxation (yet).  */
  linkrelax = 0;
}

void
tlcs900_md_end (void)
{
  int mach_type;

  switch (ins_ok & INS_MARCH_MASK)
    {
    case INS_TLCS900:
      mach_type = bfd_mach_tlcs900;
      break;
    case INS_TLCS900L:
      mach_type = bfd_mach_tlcs900l;
      break;
    default:
      mach_type = 0;
    }
  bfd_set_arch_mach (stdoutput, TARGET_ARCH, mach_type);
}

#if defined (OBJ_ELF) || defined (OBJ_MAYBE_ELF)
void
tlcs900_elf_final_processing (void)
{/* nothing to do, all is done by BFD itself */
/*
  unsigned elf_flags;
  elf_elfheader (stdoutput)->e_flags = elf_flags;
*/
}
#endif

static const char *
skip_space (const char *s)
{
  while (*s == ' ' || *s == '\t')
    ++s;
  return s;
}

/* A non-zero return-value causes a continue in the
   function read_a_source_file () in ../read.c.  */
int
tlcs900_start_line_hook (void)
{
  char *p, quote;
  char buf[4];

  /* Convert one character constants.  */
  for (p = input_line_pointer; *p && *p != '\n'; ++p)
    {
      switch (*p)
	{
	case '\'':
	  if (p[1] != 0 && p[1] != '\'' && p[2] == '\'')
	    {
	      snprintf (buf, 4, "%3d", (unsigned char)p[1]);
	      *p++ = buf[0];
	      *p++ = buf[1];
	      *p++ = buf[2];
	      break;
	    }
	  /* Fall through.  */
	case '"':
	  for (quote = *p++; quote != *p && '\n' != *p; ++p)
	    /* No escapes.  */ ;
	  if (quote != *p)
	    {
	      as_bad (_("-- unterminated string"));
	      ignore_rest_of_line ();
	      return 1;
	    }
	  break;
	case '#': /* force to use next expression as immediate value in SDCC */
	  if (!sdcc_compat)
	   break;
	  if (ISSPACE(p[1]) && *skip_space (p + 1) == '(')
	    { /* ld a,# (expr)... -> ld a,0+(expr)... */
	      *p++ = '0';
	      *p = '+';
	    }
	  else /* ld a,#(expr)... -> ld a,+(expr); ld a,#expr -> ld a, expr */
	    *p = (p[1] == '(') ? '+' : ' ';
	  break;
	}
    }
  /* Check for <label>[:] =|([.](EQU|DEFL)) <value>.  */
  if (is_name_beginner (*input_line_pointer))
    {
      char *name;
      char c, *rest, *line_start;
      int len;

      line_start = input_line_pointer;
      if (ignore_input ())
	return 0;
      c = get_symbol_name (&name);
      rest = input_line_pointer + 1;
      if (c == ':' && *rest == ':')
        {
          /* remove second colon if SDCC compatibility enabled */
          if (sdcc_compat)
            *rest = ' ';
          ++rest;
        }
      rest = (char*)skip_space (rest);
      if (*rest == '=')
	len = (rest[1] == '=') ? 2 : 1;
      else
	{
	  if (*rest == '.')
	    ++rest;
	  if (strncasecmp (rest, "EQU", 3) == 0)
	    len = 3;
	  else if (strncasecmp (rest, "DEFL", 4) == 0)
	    len = 4;
	  else
	    len = 0;
	}
      if (len && (len <= 2 || !ISALPHA (rest[len])))
	{
	  /* Handle assignment here.  */
	  if (line_start[-1] == '\n')
	    {
	      bump_line_counters ();
	      LISTING_NEWLINE ();
	    }
	  input_line_pointer = rest + len - 1;
	  /* Allow redefining with "DEFL" (len == 4), but not with "EQU".  */
	  switch (len)
	    {
	    case 1: /* label = expr */
	    case 4: /* label DEFL expr */
	      equals (name, 1);
	      break;
	    case 2: /* label == expr */
	    case 3: /* label EQU expr */
	      equals (name, 0);
	      break;
	    }
	  return 1;
	}
      else
	{
	  /* Restore line and pointer.  */
	  (void) restore_line_pointer (c);
	  input_line_pointer = line_start;
	}
    }
  return 0;
}

symbolS *
md_undefined_symbol (char *name ATTRIBUTE_UNUSED)
{
  return NULL;
}

const char *
md_atof (int type, char *litP, int *sizeP)
{
  switch (type)
    {
    case 'f':
    case 'F':
    case 's':
    case 'S':
      if (str_to_float)
	return str_to_float (litP, sizeP);
      break;
    case 'd':
    case 'D':
    case 'r':
    case 'R':
      if (str_to_double)
	return str_to_double (litP, sizeP);
      break;
    }
  return ieee_md_atof (type, litP, sizeP, false);
}

valueT
md_section_align (segT seg ATTRIBUTE_UNUSED, valueT size)
{
  return size;
}

long
md_pcrel_from (fixS * fixp)
{
  return fixp->fx_where + fixp->fx_frag->fr_address;
}

typedef const char * (asfunc)(char, char, const char*);

typedef struct _table_t
{
  const char* name;
  unsigned char prefix;
  unsigned char opcode;
  asfunc * fp;
  unsigned inss; /*0 - all CPU types or list of supported INS_* */
} table_t;

/* Compares the key for structs that start with a char * to the key.  */
static int
key_cmp (const void * a, const void * b)
{
  const char *str_a, *str_b;

  str_a = *((const char**)a);
  str_b = *((const char**)b);
  return strcmp (str_a, str_b);
}

char buf[BUFLEN];
const char *key = buf;

/* Prevent an error on a line from also generating
   a "junk at end of line" error message.  */
static char err_flag;

static void
error (const char * message)
{
  if (err_flag)
    return;

  as_bad ("%s", message);
  err_flag = 1;
}

static void
ill_op (void)
{
  error (_("illegal operand"));
}


/* Check whether an expression is indirect.  */
static int
is_indir (const char *s)
{
  char quote;
  const char *p;
  int indir, depth;

  /* Indirection is indicated with parentheses.  */
  indir = (*s == '(');

  for (p = s, depth = 0; *p && *p != ','; ++p)
    {
      switch (*p)
	{
	case '"':
	case '\'':
	  for (quote = *p++; quote != *p && *p != '\n'; ++p)
	    if (*p == '\\' && p[1])
	      ++p;
	  break;
	case '(':
	  ++ depth;
	  break;
	case ')':
	  -- depth;
	  if (depth == 0)
	    {
	      p = skip_space (p + 1);
	      if (*p && *p != ',')
		indir = 0;
	      --p;
	    }
	  if (depth < 0)
	    error (_("mismatched parentheses"));
	  break;
	}
    }

  if (depth != 0)
    error (_("mismatched parentheses"));

  return indir;
}

/* Check whether a symbol involves a register.  */
static bool
contains_register (symbolS *sym)
{
  if (sym)
    {
      expressionS * ex = symbol_get_value_expression (sym);

      switch (ex->X_op)
	{
	case O_register:
	  return true;

	case O_add:
	case O_subtract:
	  if (ex->X_op_symbol && contains_register (ex->X_op_symbol))
	    return true;
	  /* Fall through.  */
	case O_uminus:
	case O_symbol:
	  if (ex->X_add_symbol && contains_register (ex->X_add_symbol))
	    return true;
	  break;

	default:
	  break;
	}
    }

  return false;
}

/* Parse general expression, not looking for indexed addressing.  */
static const char *
parse_exp_not_indexed (const char *s, expressionS *op)
{
  const char *p;
  int indir;
  int make_shift = -1;

  memset (op, 0, sizeof (*op));
  p = skip_space (s);
  if (sdcc_compat && (*p == '<' || *p == '>'))
    {
      switch (*p)
	{
	case '<': /* LSB request */
	  make_shift = 0;
	  break;
	case '>': /* MSB request */
	  make_shift = cpu_mode ? 16 : 8;
	  break;
	}
      s = ++p;
      p = skip_space (p);
    }

  if (make_shift == -1)
    indir = is_indir (p);
  else
    indir = 0;
  op->X_md = indir;
  input_line_pointer = (char*) s ;
  expression (op);
  switch (op->X_op)
    {
    case O_absent:
      error (_("missing operand"));
      break;
    case O_illegal:
      error (_("bad expression syntax"));
      break;
    default:
      break;
    }

  if (make_shift >= 0)
    {
      /* replace [op] by [op >> shift] */
      expressionS data;
      op->X_add_symbol = make_expr_symbol (op);
      op->X_add_number = 0;
      op->X_op = O_right_shift;
      memset (&data, 0, sizeof (data));
      data.X_op = O_constant;
      data.X_add_number = make_shift;
      op->X_op_symbol = make_expr_symbol (&data);
    }
  return input_line_pointer;
}

static int
unify_indexed (expressionS *op)
{
  if (O_register != symbol_get_value_expression (op->X_add_symbol)->X_op)
    return 0;

  int rnum = symbol_get_value_expression (op->X_add_symbol)->X_add_number;
  if ( ((REG_IX != rnum) && (REG_IY != rnum)) || contains_register (op->X_op_symbol))
    {
      ill_op ();
      return 0;
    }

  /* Convert subtraction to addition of negative value.  */
  if (O_subtract == op->X_op)
    {
      expressionS minus;
      memset (&minus, 0, sizeof (minus));
      minus.X_op = O_uminus;
      minus.X_add_symbol = op->X_op_symbol;
      op->X_op_symbol = make_expr_symbol (&minus);
      op->X_op = O_add;
    }

  /* Clear X_add_number of the expression.  */
  if (op->X_add_number != 0)
    {
      expressionS add;
      memset (&add, 0, sizeof (add));
      add.X_op = O_symbol;
      add.X_add_number = op->X_add_number;
      add.X_add_symbol = op->X_op_symbol;
      op->X_add_symbol = make_expr_symbol (&add);
    }
  else
    op->X_add_symbol = op->X_op_symbol;

  op->X_add_number = rnum;
  op->X_op_symbol = 0;
  return 1;
}

/* Parse expression, change operator to O_md1 for indexed addressing.  */
static const char *
parse_exp (const char *s, expressionS *op)
{
  const char* res = parse_exp_not_indexed (s, op);
  switch (op->X_op)
    {
    case O_add:
    case O_subtract:
      if (unify_indexed (op) && op->X_md)
        op->X_op = O_md1;
      break;
    case O_register:
      if (op->X_md && ((REG_IX == op->X_add_number) || (REG_IY == op->X_add_number)))
        {
	  op->X_add_symbol = zero;
	  op->X_op = O_md1;
	}
	break;
    case O_constant:
      /* parse SDCC syntax where index register offset placed before parentheses */
      if (sdcc_compat && is_indir (res))
        {
          expressionS off;
          off = *op;
          res = parse_exp (res, op);
          if (op->X_op != O_md1 || op->X_add_symbol != zero)
            ill_op ();
          else
              op->X_add_symbol = make_expr_symbol (&off);
        }
      break;
    default:
      break;
    }
  return res;
}

void tlcs900_cons_fix_new (fragS *frag_p, int offset, int nbytes, expressionS *exp)
{
  bfd_reloc_code_real_type r[4] =
    {
      BFD_RELOC_8,
      BFD_RELOC_16,
      BFD_RELOC_24,
      BFD_RELOC_32
    };

  if (nbytes < 1 || nbytes > 4)
    {
      as_bad (_("unsupported BFD relocation size %u"), nbytes);
    }
  else
    {
      fix_new_exp (frag_p, offset, nbytes, exp, 0, r[nbytes-1]);
    }
}

static void
emit_data_val (expressionS * val, int size)
{
  char *p;
  bfd_reloc_code_real_type r_type;

  p = frag_more (size);
  if (val->X_op == O_constant)
    {
      int i;
      if (is_overflow (val->X_add_number, size*8))
	as_warn ( _("%d-bit overflow (%+ld)"), size*8, val->X_add_number);
      for (i = 0; i < size; ++i)
	p[i] = (char)(val->X_add_number >> (i*8));
      return;
    }

  switch (size)
    {
    case 1: r_type = BFD_RELOC_8; break;
    case 2: r_type = BFD_RELOC_16; break;
    case 3: r_type = BFD_RELOC_24; break;
    case 4: r_type = BFD_RELOC_32; break;
    case 8: r_type = BFD_RELOC_64; break;
    default:
      as_fatal (_("invalid data size %d"), size);
    }

  if (   (val->X_op == O_register)
      || (val->X_op == O_md1)
      || contains_register (val->X_add_symbol)
      || contains_register (val->X_op_symbol))
    ill_op ();

  if (size <= 2 && val->X_op_symbol)
    {
      bool simplify = true;
      int shift = symbol_get_value_expression (val->X_op_symbol)->X_add_number;
      if (val->X_op == O_bit_and && shift == (1 << (size*8))-1)
	shift = 0;
      else if (val->X_op != O_right_shift)
	shift = -1;

      if (size == 1)
	{
	  switch (shift)
	    {
	    case 0: r_type = BFD_RELOC_TLCS900_BYTE0; break;
	    case 8: r_type = BFD_RELOC_TLCS900_BYTE1; break;
	    case 16: r_type = BFD_RELOC_TLCS900_BYTE2; break;
	    case 24: r_type = BFD_RELOC_TLCS900_BYTE3; break;
	    default: simplify = false;
	    }
	}
      else /* if (size == 2) */
	{
	  switch (shift)
	    {
	    case 0: r_type = BFD_RELOC_TLCS900_WORD0; break;
	    case 16: r_type = BFD_RELOC_TLCS900_WORD1; break;
	    case 8:
	    case 24: /* add two byte fixups */
	      val->X_op = O_symbol;
	      val->X_op_symbol = NULL;
	      val->X_add_number = 0;
	      if (shift == 8)
		{
		  fix_new_exp (frag_now, p++ - frag_now->fr_literal, 1, val, false,
			       BFD_RELOC_TLCS900_BYTE1);
		  /* prepare to next byte */
		  r_type = BFD_RELOC_TLCS900_BYTE2;
		}
	      else
		r_type = BFD_RELOC_TLCS900_BYTE3; /* high byte will be 0 */
	      size = 1;
	      simplify = false;
	      break;
	    default: simplify = false;
	    }
	}

      if (simplify)
	{
	  val->X_op = O_symbol;
	  val->X_op_symbol = NULL;
	  val->X_add_number = 0;
	}
    }

  fix_new_exp (frag_now, p - frag_now->fr_literal, size, val, false, r_type);
}

static void
emit_byte (expressionS * val, bfd_reloc_code_real_type r_type)
{
  char *p;

  if (r_type == BFD_RELOC_8)
    {
      emit_data_val (val, 1);
      return;
    }
  p = frag_more (1);
  *p = val->X_add_number;
  if (contains_register (val->X_add_symbol) || contains_register (val->X_op_symbol))
    {
      ill_op ();
    }
  else if ((r_type == BFD_RELOC_8_PCREL) && (val->X_op == O_constant))
    {
      as_bad (_("cannot make a relative jump to an absolute location"));
    }
  else if (val->X_op == O_constant)
    {
      if ((val->X_add_number < -128) || (val->X_add_number >= 128))
	{
	  if (r_type == BFD_RELOC_TLCS900_DISP8)
	    as_bad (_("index overflow (%+ld)"), val->X_add_number);
	  else
	    as_bad (_("offset overflow (%+ld)"), val->X_add_number);
	}
    }
  else
    {
      /* For symbols only, constants are stored at begin of function.  */
      fix_new_exp (frag_now, p - frag_now->fr_literal, 1, val,
		   r_type == BFD_RELOC_8_PCREL, r_type);
    }
}

static void
emit_data (int size ATTRIBUTE_UNUSED)
{
  const char *p, *q;
  char *u, quote;
  int cnt;
  expressionS exp;

  if (is_it_end_of_statement ())
    {
      demand_empty_rest_of_line ();
      return;
    }
  p = skip_space (input_line_pointer);

  do
    {
      if (*p == '\"' || *p == '\'')
	{
	    for (quote = *p, q = ++p, cnt = 0; *p && quote != *p; ++p, ++cnt)
	      ;
	    u = frag_more (cnt);
	    memcpy (u, q, cnt);
	    if (!*p)
	      as_warn (_("unterminated string"));
	    else
	      p = skip_space (p+1);
	}
      else
	{
	  p = parse_exp (p, &exp);
	  if (exp.X_op == O_md1 || exp.X_op == O_register)
	    {
	      ill_op ();
	      break;
	    }
	  if (exp.X_md)
	    as_warn (_("parentheses ignored"));
	  emit_byte (&exp, BFD_RELOC_8);
	  p = skip_space (p);
	}
    }
  while (*p++ == ',') ;
  input_line_pointer = (char *)(p-1);
}


static void
tlcs900_cons (int size)
{
  const char *p;
  expressionS exp;

  if (is_it_end_of_statement ())
    {
      demand_empty_rest_of_line ();
      return;
    }
  p = skip_space (input_line_pointer);

  do
    {
      p = parse_exp (p, &exp);
      if (exp.X_op == O_md1 || exp.X_op == O_register)
	{
	  ill_op ();
	  break;
	}
      if (exp.X_md)
	as_warn (_("parentheses ignored"));
      emit_data_val (&exp, size);
      p = skip_space (p);
  } while (*p++ == ',') ;
  input_line_pointer = (char *)(p-1);
}

/* next functions were commented out because it is difficult to mix
   both ADL and TLCS900 mode instructions within one COFF file:
   objdump cannot recognize point of mode switching.
*/
static void
set_cpu_mode (int mode)
{
// FIXME: This is completely wrong!
    if (mode < 0)
        error (_("CPU mode is unsupported by target"));
}

static void
assume (int arg ATTRIBUTE_UNUSED)
{
  char *name;
  char c;
  int n;

  input_line_pointer = (char*)skip_space (input_line_pointer);
  c = get_symbol_name (& name);
  if (strncasecmp (name, "ADL", 4) != 0)
    {
      ill_op ();
      return;
    }

  restore_line_pointer (c);
  input_line_pointer = (char*)skip_space (input_line_pointer);
  if (*input_line_pointer++ != '=')
    {
      error (_("assignment expected"));
      return;
    }
  input_line_pointer = (char*)skip_space (input_line_pointer);
  n = get_single_number ();

  set_cpu_mode (n);
}


static int
assemble_suffix (const char **suffix)
{
  static
  const char sf[8][4] = 
    {
      "il",
      "is",
      "l",
      "lil",
      "lis",
      "s",
      "sil",
      "sis"
    };
  const char *p;
  const char (*t)[4];
  char sbuf[4];
  int i;

  p = *suffix;
  if (*p++ != '.')
    return 0;

  for (i = 0; (i < 3) && (ISALPHA (*p)); i++)
    sbuf[i] = TOLOWER (*p++);
  if (*p && !ISSPACE (*p))
    return 0;
  *suffix = p;
  sbuf[i] = 0;

  t = bsearch (sbuf, sf, ARRAY_SIZE (sf), sizeof (sf[0]), (int(*)(const void*, const void*)) strcmp);
  if (t == NULL)
    return 0;
  i = t - sf;
  switch (i)
    {
      case 0: /* IL */
        i = cpu_mode ? 0x5B : 0x52;
        break;
      case 1: /* IS */
        i = cpu_mode ? 0x49 : 0x40;
        break;
      case 2: /* L */
        i = cpu_mode ? 0x5B : 0x49;
        break;
      case 3: /* LIL */
        i = 0x5B;
        break;
      case 4: /* LIS */
        i = 0x49;
        break;
      case 5: /* S */
        i = cpu_mode ? 0x52 : 0x40;
        break;
      case 6: /* SIL */
        i = 0x52;
        break;
      case 7: /* SIS */
        i = 0x40;
        break;
    }
  *frag_more (1) = (char)i;
  switch (i)
    {
    case 0x40: inst_mode = INST_MODE_FORCED | INST_MODE_S | INST_MODE_IS; break;
    case 0x49: inst_mode = INST_MODE_FORCED | INST_MODE_L | INST_MODE_IS; break;
    case 0x52: inst_mode = INST_MODE_FORCED | INST_MODE_S | INST_MODE_IL; break;
    case 0x5B: inst_mode = INST_MODE_FORCED | INST_MODE_L | INST_MODE_IL; break;
    }
  return 1;
}

static void
psect (int arg)
{
#if defined(OBJ_ELF)
  return obj_elf_section (arg);
#elif defined(OBJ_COFF)
  return obj_coff_section (arg);
#else
#error Unknown object format
#endif
}

static void
set_inss (int inss)
{
  int old_ins;

  if (!sdcc_compat)
    as_fatal (_("Invalid directive"));

  old_ins = ins_ok;
  ins_ok &= INS_MARCH_MASK;
  ins_ok |= inss;
  if (old_ins != ins_ok)
    cpu_mode = 0;
}

static void
ignore (int arg ATTRIBUTE_UNUSED)
{
  ignore_rest_of_line ();
}

static void
area (int arg)
{
  char *p;
  if (!sdcc_compat)
    as_fatal (_("Invalid directive"));
  for (p = input_line_pointer; *p && *p != '(' && *p != '\n'; p++)
    ;
  if (*p == '(')
    {
      *p = '\n';
      psect (arg);
      *p++ = '(';
      ignore_rest_of_line ();
    }
  else
    psect (arg);
}

/* Handle the .bss pseudo-op.  */

static void
s_bss (int ignore ATTRIBUTE_UNUSED)
{
  subseg_set (bss_section, 0);
  demand_empty_rest_of_line ();
}

/* Port specific pseudo ops.  */
const pseudo_typeS md_pseudo_table[] =
{
  { ".area", area, 0},
  { ".assume", assume, 0},
  { ".module", ignore, 0},
  { ".optsdcc", ignore, 0},
  { ".set", s_set, 0},
  { ".tlcs900", set_inss, INS_TLCS900},
  { ".tlcs900l", set_inss, INS_TLCS900L},
  { "bss", s_bss, 0},
  { "db" , emit_data, 1},
  { "d24", tlcs900_cons, 3},
  { "d32", tlcs900_cons, 4},
  { "def24", tlcs900_cons, 3},
  { "def32", tlcs900_cons, 4},
  { "defb", emit_data, 1},
  { "defm", emit_data, 1},
  { "defs", s_space, 1}, /* Synonym for ds on some assemblers.  */
  { "defw", tlcs900_cons, 2},
  { "ds",   s_space, 1}, /* Fill with bytes rather than words.  */
  { "dw", tlcs900_cons, 2},
  { "psect", psect, 0}, /* TODO: Translate attributes.  */
  { "set", 0, 0}, 		/* Real instruction on tlcs900.  */
  { "xdef", s_globl, 0},	/* Synonym for .GLOBAL */
  { "xref", s_ignore, 0},	/* Synonym for .EXTERN */
  { NULL, 0, 0 }
} ;

// FIXME!!!
static table_t instab[] =
{
  { "adc",  0x88, 0x4A, NULL,  INS_ALL },
  { "xor",  0x00, 0xA8, NULL,    INS_ALL },
} ;

void
md_assemble (char *str)
{
  const char *p;
  char * old_ptr;
  int i;
  table_t *insp;

  err_flag = 0;
  inst_mode = cpu_mode ? (INST_MODE_L | INST_MODE_IL) : (INST_MODE_S | INST_MODE_IS);
  old_ptr = input_line_pointer;
  p = skip_space (str);
  for (i = 0; (i < BUFLEN) && (ISALPHA (*p) || ISDIGIT (*p));)
    buf[i++] = TOLOWER (*p++);

  if (i == BUFLEN)
    {
      buf[BUFLEN-3] = buf[BUFLEN-2] = '.'; /* Mark opcode as abbreviated.  */
      buf[BUFLEN-1] = 0;
      as_bad (_("Unknown instruction '%s'"), buf);
    }
  else
    {
      dwarf2_emit_insn (0);
      if ((*p) && (!ISSPACE (*p)))
        {
          if (*p != '.' || !assemble_suffix (&p))
            {
              as_bad (_("syntax error"));
              goto end;
            }
        }
      buf[i] = 0;
      p = skip_space (p);
      key = buf;

      insp = bsearch (&key, instab, ARRAY_SIZE (instab),
		    sizeof (instab[0]), key_cmp);
      if (!insp || (insp->inss && !(insp->inss & ins_ok)))
	{
	  *frag_more (1) = 0;
	  as_bad (_("Unknown instruction `%s'"), buf);
	}
      else
	{
	  p = insp->fp (insp->prefix, insp->opcode, p);
	  p = skip_space (p);
	  if ((!err_flag) && *p)
	    as_bad (_("junk at end of line, "
		      "first unrecognized character is `%c'"), *p);
	}
    }
 end:
  input_line_pointer = old_ptr;
}

static int
signed_overflow (signed long value, unsigned bitsize)
{
  signed long max = (signed long) ((1UL << (bitsize - 1)) - 1);
  return value < -max - 1 || value > max;
}

static int
unsigned_overflow (unsigned long value, unsigned bitsize)
{
  return value >> (bitsize - 1) >> 1 != 0;
}

static int
is_overflow (long value, unsigned bitsize)
{
  if (value < 0)
    return signed_overflow (value, bitsize);
  return unsigned_overflow ((unsigned long)value, bitsize);
}

void
md_apply_fix (fixS * fixP, valueT* valP, segT seg)
{
  long val = *valP;
  char *p_lit = fixP->fx_where + fixP->fx_frag->fr_literal;

  if (fixP->fx_addsy == NULL)
    fixP->fx_done = 1;
  else if (fixP->fx_pcrel)
    {
      segT s = S_GET_SEGMENT (fixP->fx_addsy);
      if (s == seg || s == absolute_section)
	{
	  val += S_GET_VALUE (fixP->fx_addsy);
	  fixP->fx_done = 1;
	}
    }

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_8_PCREL:
    case BFD_RELOC_TLCS900_DISP8:
    case BFD_RELOC_8:
    case BFD_RELOC_16:
    case BFD_RELOC_24:
    case BFD_RELOC_32:
    case BFD_RELOC_TLCS900_16_BE:
      fixP->fx_no_overflow = 0;
      break;
    default:
      fixP->fx_no_overflow = 1;
      break;
    }

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_8_PCREL:
    case BFD_RELOC_TLCS900_DISP8:
      if (fixP->fx_done && signed_overflow (val, 8))
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      _("8-bit signed offset out of range (%+ld)"), val);
      *p_lit++ = val;
      break;

    case BFD_RELOC_TLCS900_BYTE0:
      *p_lit++ = val;
      break;

    case BFD_RELOC_TLCS900_BYTE1:
      *p_lit++ = (val >> 8);
      break;

    case BFD_RELOC_TLCS900_BYTE2:
      *p_lit++ = (val >> 16);
      break;

    case BFD_RELOC_TLCS900_BYTE3:
      *p_lit++ = (val >> 24);
      break;

    case BFD_RELOC_8:
      if (fixP->fx_done && is_overflow(val, 8))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("8-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      break;

    case BFD_RELOC_TLCS900_WORD1:
      *p_lit++ = (val >> 16);
      *p_lit++ = (val >> 24);
      break;

    case BFD_RELOC_TLCS900_WORD0:
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      break;

    case BFD_RELOC_16:
      if (fixP->fx_done && is_overflow(val, 16))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("16-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      break;

    case BFD_RELOC_24: /* Def24 may produce this.  */
      if (fixP->fx_done && is_overflow(val, 24))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("24-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      *p_lit++ = (val >> 16);
      break;

    case BFD_RELOC_32: /* Def32 and .long may produce this.  */
      if (fixP->fx_done && is_overflow(val, 32))
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       _("32-bit overflow (%+ld)"), val);
      *p_lit++ = val;
      *p_lit++ = (val >> 8);
      *p_lit++ = (val >> 16);
      *p_lit++ = (val >> 24);
      break;

    case BFD_RELOC_TLCS900_16_BE: /* TLCS900N PUSH nn instruction produce this.  */
      *p_lit++ = val >> 8;
      *p_lit++ = val;
      break;

    default:
      printf (_("md_apply_fix: unknown reloc type 0x%x\n"), fixP->fx_r_type);
      abort ();
    }
}

/* GAS will call this to generate a reloc.  GAS will pass the
   resulting reloc to `bfd_install_relocation'.  This currently works
   poorly, as `bfd_install_relocation' often does the wrong thing, and
   instances of `tc_gen_reloc' have been written to work around the
   problems, which in turns makes it difficult to fix
   `bfd_install_relocation'.  */

/* If while processing a fixup, a reloc really
   needs to be created then it is done here.  */

arelent *
tc_gen_reloc (asection *seg ATTRIBUTE_UNUSED , fixS *fixp)
{
  arelent *reloc;

  if (fixp->fx_subsy != NULL)
    {
      as_bad_subtract (fixp);
      return NULL;
    }

  reloc               = XNEW (arelent);
  reloc->sym_ptr_ptr  = XNEW (asymbol *);
  *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
  reloc->address      = fixp->fx_frag->fr_address + fixp->fx_where;
  reloc->addend       = fixp->fx_offset;
  reloc->howto        = bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type);
  if (reloc->howto == NULL)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
		    _("reloc %d not supported by object file format"),
		    (int) fixp->fx_r_type);
      return NULL;
    }

  if (fixp->fx_r_type == BFD_RELOC_VTABLE_INHERIT
      || fixp->fx_r_type == BFD_RELOC_VTABLE_ENTRY)
    reloc->address = fixp->fx_offset;

  return reloc;
}

int
tlcs900_tc_labels_without_colon (void)
{
  return colonless_labels;
}

int
tlcs900_tc_label_is_local (const char *name)
{
  const char *n;
  const char *p;
  if (local_label_prefix == NULL)
    return 0;
  for (p = local_label_prefix, n = name; *p && *n && *n == *p; p++, n++)
    ;
  return *p == '\0';
}


#ifdef TARGET_USE_CFIPOP
/* Initialize the DWARF-2 unwind information for this procedure. */
void
tlcs900_tc_frame_initial_instructions (void)
{
  static int sp_regno = -1;

  if (sp_regno < 0)
    sp_regno = tlcs900_tc_regname_to_dw2regnum ("sp");

  cfi_add_CFA_def_cfa (sp_regno, 0);
}

int
tlcs900_tc_regname_to_dw2regnum (const char *regname)
{
  static const char *regs[] =
    { /* same registers as for GDB */
      "af", "bc", "de", "hl",
      "sp", "pc", "ix", "iy",
      "af_", "bc_", "de_", "hl_",
      "ir"
    };
  unsigned i;

  for (i = 0; i < ARRAY_SIZE(regs); ++i)
    if (!strcasecmp (regs[i], regname))
      return i;

  return -1;
}
#endif

/* Implement DWARF2_ADDR_SIZE.  */
int
tlcs900_dwarf2_addr_size (const bfd *abfd)
{
  switch (bfd_get_mach (abfd))
    {
    default:
      return 2;
    }
}

/* Print TOSHIBA TLCS900 instructions
   Copyright (C) 2005-2021 Free Software Foundation, Inc.
   Contributed by Felipe Correa da Silva Sanches <juca@members.fsf.org>

   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "disassemble.h"
#include <stdio.h>

struct buffer
{
  bfd_vma base;
  int n_fetch;
  int n_used;
  signed char data[6];
  long inss; /* instruction set bit mask, taken from bfd_mach */
  int nn_len; /* address length: 2 - Z80 mode, 3 - ADL mode*/
} ;

typedef int (*func)(struct buffer *, disassemble_info *, const char *);

struct tab_elt
{
  unsigned char val;
  unsigned char mask;
  func          fp;
  const char *  text;
  unsigned      inss; /* bit mask of supported bfd_mach_* or 0 for all mach */
} ;

#define INSS_ALL 0
#define INSS_TLCS900	(1 << bfd_mach_tlcs900)
#define INSS_TLCS900L	(1 << bfd_mach_tlcs900l)
//#define INSS_TLCS900H	(1 << bfd_mach_tlcs900h)
//#define INSS_TLCS900H2	(1 << bfd_mach_tlcs900h2)

#define TXTSIZ 24
/* Names of 8-bit registers.  */
static const char * r_str[]  = { "w", "a", "b", "c", "d", "e", "h", "l" };
/* Names of 16-bit registers.  */
static const char * rr_str[] = { "wa", "bc", "de", "hl", "ix", "iy", "iz", "sp" };
/* Names of 32-bit registers.  */
static const char * xrr_str[] = { "xwa", "xbc", "xde", "xhl", "xix", "xiy", "xiz", "xsp" };
/* Texts for condition codes.  */
static const char * cc_str[] = { "f", "lt", "le", "ule", "pe/ov", "m/mi", "z", "c",
                                 "(t)", "ge", "gt", "ugt", "po/nov", "p/pl", "nz", "nc" };

/* Instruction names for 8-bit arithmetic, operand "a" is often implicit */
//static const char * arit_str[] =
//{
//  "add a,", "adc a,", "sub ", "sbc a,", "and ", "xor ", "or ", "cp "
//} ;

static int
mach_inst (struct buffer *buf, const struct tab_elt *p)
{
  return !p->inss || (p->inss & buf->inss);
}

static int
fetch_data (struct buffer *buf, disassemble_info * info, int n)
{
  int r;

  if (buf->n_fetch + n > (int)sizeof (buf->data))
    abort ();

  r = info->read_memory_func (buf->base + buf->n_fetch,
			      (unsigned char*) buf->data + buf->n_fetch,
			      n, info);
  if (r == 0)
    buf->n_fetch += n;
  return !r;
}

static int
prt (struct buffer *buf, disassemble_info * info, const char *txt)
{
  info->fprintf_func (info->stream, "%s", txt);
  buf->n_used = buf->n_fetch;
  return 1;
}

static int
prt_nn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  int nn;
  unsigned char *p;
  int i;

  p = (unsigned char*) buf->data + buf->n_fetch;
  if (fetch_data (buf, info, buf->nn_len))
    {
      nn = 0x00;
      i = buf->nn_len;
      while (i--)
        nn = nn * 0x100 + p[i];
      info->fprintf_func (info->stream, txt, nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;
  return buf->n_used;
}

static int
prt_n (struct buffer *buf, disassemble_info * info, const char *txt)
{
  int n;
  unsigned char *p;

  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = p[0];
      info->fprintf_func (info->stream, txt, n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}


static int
prt_rr_nn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  char mytxt[TXTSIZ];
  int rr;

  rr = buf->data[buf->n_fetch - 1] & 7;
  snprintf (mytxt, TXTSIZ, txt, rr_str[rr]);

  return prt_nn (buf, info, mytxt);
}

static int
prt_rr (struct buffer *buf, disassemble_info * info, const char *txt)
{
  info->fprintf_func (info->stream, "%s%s", txt,
		      rr_str[(buf->data[buf->n_fetch - 1] >> 3) & 7]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_xrr (struct buffer *buf, disassemble_info * info, const char *txt)
{
//FIXME!!!
  info->fprintf_func (info->stream, "%s%s", txt,
		      xrr_str[(buf->data[buf->n_fetch - 1] >> 4) & 0xf]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_xrr_nnn (struct buffer *buf, disassemble_info * info, const char *txt)
{
//FIXME!!!
  info->fprintf_func (info->stream, "%s%s", txt,
		      rr_str[(buf->data[buf->n_fetch - 1] >> 4) & 3]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_xrr_d (struct buffer *buf, disassemble_info * info, const char *txt)
{
//FIXME!!!
  info->fprintf_func (info->stream, "%s%s", txt,
		      rr_str[(buf->data[buf->n_fetch - 1] >> 4) & 3]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_n_n (struct buffer *buf, disassemble_info * info, const char *txt)
{
  char mytxt[TXTSIZ];
  int n;
  unsigned char *p;

  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = p[0];
      snprintf (mytxt, TXTSIZ, txt, n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_n (buf, info, mytxt);
}

static int
prt_dst_n (struct buffer *buf, disassemble_info * info, const char *txt)
{
  char mytxt[TXTSIZ];
  int n;
  unsigned char *p;

  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = p[0];
      snprintf (mytxt, TXTSIZ, txt, n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_n (buf, info, mytxt);
}

static int
prt_n_nn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  char mytxt[TXTSIZ];
  int n;
  unsigned char *p;

  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = p[0];
      snprintf (mytxt, TXTSIZ, txt, n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_nn (buf, info, mytxt);
}

static int
prt_nnn (struct buffer *buf, disassemble_info * info, const char *txt)
{
// FIXME!!!!!
  char mytxt[TXTSIZ];
  int n;
  unsigned char *p;

  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = p[0];
      snprintf (mytxt, TXTSIZ, txt, n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_n (buf, info, mytxt);
}

static int
prt_r_n (struct buffer *buf, disassemble_info * info, const char *txt)
{
  char mytxt[TXTSIZ];
  int r;

  r = (buf->data[buf->n_fetch - 1] >> 3) & 7;
  snprintf (mytxt, TXTSIZ, txt, r_str[r]);
  return prt_n (buf, info, mytxt);
}

static int
pop_rr (struct buffer *buf, disassemble_info * info, const char *txt)
{
  static char *rr_stack[] = { "bc","de","hl","af"};

  info->fprintf_func (info->stream, "%s %s", txt,
		      rr_stack[(buf->data[0] >> 4) & 3]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_cc_n (struct buffer *buf, disassemble_info * info, const char *txt)
{
  char mytxt[TXTSIZ];

  snprintf (mytxt,TXTSIZ,
	    "%s%s,0x%%04x", txt, cc_str[(buf->data[0] >> 3) & 7]);
  return prt_n (buf, info, mytxt);
}

static int
prt_cc_nn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  char mytxt[TXTSIZ];

  snprintf (mytxt,TXTSIZ,
	    "%s%s,0x%%04x", txt, cc_str[(buf->data[0] >> 3) & 7]);
  return prt_nn (buf, info, mytxt);
}

static int
print_insn_tlcs900_buf (struct buffer *buf, disassemble_info *info);

int
print_insn_tlcs900 (bfd_vma addr, disassemble_info * info);



/* Table to disassemble machine codes with reg.  */
static const struct tab_elt opc_reg[] =
{
  { 0x1c, 0xff, prt_n,    "djnz %s, PC + 0x%%02x", INSS_ALL },
  { 0xc8, 0xff, prt_n,    "add %s, 0x%%02x", INSS_ALL },
  { 0xc9, 0xff, prt_n,    "adc %s, 0x%%02x", INSS_ALL },
  { 0xca, 0xff, prt_n,    "sub %s, 0x%%02x", INSS_ALL },
  { 0xcb, 0xff, prt_n,    "sbc %s, 0x%%02x", INSS_ALL },
  { 0xcc, 0xff, prt_n,    "and %s, 0x%%02x", INSS_ALL },
  { 0xcd, 0xff, prt_n,    "xor %s, 0x%%02x", INSS_ALL },
  { 0xce, 0xff, prt_n,    "or %s, 0x%%02x", INSS_ALL },
  { 0xcf, 0xff, prt_n,    "cp %s, 0x%%02x", INSS_ALL },
};

static int
prt_regB (struct buffer *buf, disassemble_info *info,
         const char *txt ATTRIBUTE_UNUSED)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_reg; p->val != (buf->data[1] & p->mask) || !mach_inst (buf, p); ++p)
        ;
        {
          snprintf (mytxt,TXTSIZ, p->text, r_str[(buf->data[buf->n_fetch - 1] >> 3) & 7]);
          p->fp (buf, info, mytxt);
        }
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_regW (struct buffer *buf, disassemble_info *info,
         const char *txt ATTRIBUTE_UNUSED)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_reg; p->val != (buf->data[1] & p->mask) || !mach_inst (buf, p); ++p)
        ;
        {
          snprintf (mytxt,TXTSIZ, p->text, rr_str[(buf->data[buf->n_fetch - 1] >> 3) & 7]);
          p->fp (buf, info, mytxt);
        }
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_regL (struct buffer *buf, disassemble_info *info,
         const char *txt ATTRIBUTE_UNUSED)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_reg; p->val != (buf->data[1] & p->mask) || !mach_inst (buf, p); ++p)
        ;
        {
          snprintf (mytxt,TXTSIZ, p->text, xrr_str[(buf->data[buf->n_fetch - 1] >> 3) & 7]);
          p->fp (buf, info, mytxt);
        }
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

/* Table to disassemble machine codes with prefix 'dst'.  */
static const struct tab_elt opc_dst[] =
{
  { 0xF1, 0xFF, prt_dst_n,    "ld (%s), 0x%%02x", INSS_ALL },
};

static int
pref_dst (struct buffer *buf, disassemble_info *info,
          const char *txt ATTRIBUTE_UNUSED)
{
  const struct tab_elt *p;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_dst; p->val != (buf->data[1] & p->mask) || !mach_inst (buf, p); ++p)
        ;
      p->fp (buf, info, p->text);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

/* Table to disassemble machine codes without prefix.  */
static const struct tab_elt
opc_main[] =
{
  { 0x00, 0xff, prt,         "nop", INSS_ALL },
  { 0x01, 0xff, prt,         "normal", INSS_TLCS900 },
  { 0x02, 0xff, prt,         "push sr", INSS_ALL },
  { 0x03, 0xff, prt,         "pop sr", INSS_ALL },
  { 0x04, 0xff, prt,         "max", INSS_TLCS900 },
  { 0x04, 0xff, prt,         "min", INSS_TLCS900L }, /* TODO: review this one. */
  { 0x05, 0xff, prt,         "halt", INSS_ALL },
  { 0x06, 0xff, prt_n,       "ei 0x%02x", INSS_ALL },
  { 0x07, 0xff, prt,         "reti", INSS_ALL },
  { 0x08, 0xff, prt_n_n,     "ld (0x%02x), 0x%%02x", INSS_ALL },
  { 0x09, 0xff, prt_n,       "push 0x%02x", INSS_ALL },
  { 0x0a, 0xff, prt_n_nn,    "ld (0x%02x), 0x%%04x", INSS_ALL },
  { 0x0b, 0xff, prt_nn,      "pushw 0x%%04x", INSS_ALL },
  { 0x0c, 0xff, prt,         "incf", INSS_ALL },
  { 0x0d, 0xff, prt,         "decf", INSS_ALL },
  { 0x0e, 0xff, prt,         "ret", INSS_ALL },
  { 0x0f, 0xff, prt_nn,      "retd 0x%%04x", INSS_ALL },
  { 0x10, 0xff, prt,         "rcf", INSS_ALL },
  { 0x11, 0xff, prt,         "scf", INSS_ALL },
  { 0x12, 0xff, prt,         "ccf", INSS_ALL },
  { 0x13, 0xff, prt,         "zcf", INSS_ALL },
  { 0x14, 0xff, prt,         "push a", INSS_ALL },
  { 0x15, 0xff, prt,         "pop a", INSS_ALL },
  { 0x16, 0xff, prt,         "ex f, f'", INSS_ALL },
  { 0x17, 0xff, prt_n,       "ldf 0x%02x", INSS_ALL },
  { 0x18, 0xff, prt,         "push f", INSS_ALL },
  { 0x19, 0xff, prt,         "pop f", INSS_ALL },
  { 0x1a, 0xff, prt_nn,      "jp 0x%04x", INSS_ALL },
  { 0x1b, 0xff, prt_nnn,     "jp 0x%06x", INSS_ALL },
  { 0x1c, 0xff, prt_nn,      "call 0x%04x", INSS_ALL },
  { 0x1d, 0xff, prt_nnn,     "call 0x%06x", INSS_ALL },
  { 0x1e, 0xff, prt_nn,      "calr PC + 0x%04x", INSS_ALL },
  { 0x20, 0xf8, prt_r_n,     "ld %s, 0x%%02x", INSS_ALL },
  { 0x28, 0xf8, prt_rr,      "push %s", INSS_ALL },
  { 0x30, 0xf8, prt_rr_nn,   "ld %s, 0x%%04x", INSS_ALL },
  { 0x38, 0xf8, prt_xrr,     "push %s", INSS_ALL },
  { 0x40, 0xf8, prt_xrr_nnn, "ld %s, 0x%%06x", INSS_ALL },
  { 0x48, 0xf8, pop_rr,      "pop %s", INSS_ALL },
  { 0x58, 0xf8, prt_xrr,     "pop %s", INSS_ALL },
  { 0x60, 0xF0, prt_cc_n,    "jr %s, PC + 0x%02x", INSS_ALL },
  { 0x70, 0xF0, prt_cc_nn,   "jrl %s, PC + 0x%04x", INSS_ALL },
  { 0x80, 0xf8, prt_xrr,     "src.B (%s)", INSS_ALL },
  { 0x88, 0xf8, prt_xrr_d,   "src.B (%s + d)", INSS_ALL },
  { 0x90, 0xf8, prt_xrr,     "src.W (%s)", INSS_ALL },
  { 0x98, 0xf8, prt_xrr_d,   "src.W (%s + d)", INSS_ALL },
  { 0xa0, 0xf8, prt_xrr,     "src.L (%s)", INSS_ALL },
  { 0xa8, 0xf8, prt_xrr_d,   "src.L (%s + d)", INSS_ALL },
  { 0xb0, 0xf8, prt_xrr,     "dst (%s)", INSS_ALL },
  { 0xb8, 0xf8, prt_xrr_d,   "dst (%s + d)", INSS_ALL },
  { 0xc0, 0xff, prt,       "C0", INSS_ALL },
  { 0xc1, 0xff, prt,       "C1", INSS_ALL },
  { 0xc2, 0xff, prt,       "C2", INSS_ALL },
  { 0xc3, 0xff, prt,       "C3", INSS_ALL },
  { 0xc4, 0xff, prt,       "C4", INSS_ALL },
  { 0xc5, 0xff, prt,       "C5", INSS_ALL },
  { 0xc7, 0xff, prt,       "C7", INSS_ALL },
  { 0xc8, 0xf8, prt_regB,  "", INSS_ALL },
  { 0xd0, 0xff, prt,       "D0", INSS_ALL },
  { 0xd1, 0xff, prt,       "D1", INSS_ALL },
  { 0xd2, 0xff, prt,       "D2", INSS_ALL },
  { 0xd3, 0xff, prt,       "D3", INSS_ALL },
  { 0xd4, 0xff, prt,       "D4", INSS_ALL },
  { 0xd5, 0xff, prt,       "D5", INSS_ALL },
  { 0xd7, 0xff, prt,       "D7", INSS_ALL },
  { 0xd8, 0xf8, prt_regW,  "", INSS_ALL },
  { 0xe0, 0xff, prt,       "E0", INSS_ALL },
  { 0xe1, 0xff, prt,       "E1", INSS_ALL },
  { 0xe2, 0xff, prt,       "E2", INSS_ALL },
  { 0xe3, 0xff, prt,       "E3", INSS_ALL },
  { 0xe4, 0xff, prt,       "E4", INSS_ALL },
  { 0xe5, 0xff, prt,       "E5", INSS_ALL },
  { 0xe7, 0xff, prt,       "E7", INSS_ALL },
  { 0xe8, 0xf8, prt_regL,  "", INSS_ALL },
  { 0xf0, 0xff, prt,       "F0", INSS_ALL },
  { 0xf1, 0xff, pref_dst,  "pref_dst", INSS_ALL },
  { 0xf2, 0xff, prt,       "F2", INSS_ALL },
  { 0xf3, 0xff, prt,       "F3", INSS_ALL },
  { 0xf4, 0xff, prt,       "F4", INSS_ALL },
  { 0xf5, 0xff, prt,       "F5", INSS_ALL },
  { 0xf7, 0xff, prt_n_n,   "ldx (0x%02x), 0x%02x", INSS_ALL },
  { 0xf8, 0xff, prt,       "swi 0", INSS_ALL },
  { 0xf9, 0xff, prt,       "swi 1", INSS_ALL },
  { 0xfa, 0xff, prt,       "swi 2", INSS_ALL },
  { 0xfb, 0xff, prt,       "swi 3", INSS_ALL },
  { 0xfc, 0xff, prt,       "swi 4", INSS_ALL },
  { 0xfd, 0xff, prt,       "swi 5", INSS_ALL },
  { 0xfe, 0xff, prt,       "swi 6", INSS_ALL },
  { 0xff, 0xff, prt,       "swi 7", INSS_ALL },
} ;

int
print_insn_tlcs900 (bfd_vma addr, disassemble_info * info)
{
  struct buffer buf;

  buf.base = addr;
  buf.inss = 1 << info->mach;
  buf.nn_len = 2;
  info->bytes_per_line = 4; /* <ss pp oo nn mm MM> OR <pp oo nn mm> */

  return print_insn_tlcs900_buf (&buf, info);
}

static int
print_insn_tlcs900_buf (struct buffer *buf, disassemble_info *info)
{
  const struct tab_elt *p;

  buf->n_fetch = 0;
  buf->n_used = 0;
  if (! fetch_data (buf, info, 1))
    return -1;

  p = opc_main;

  for (; p->val != (buf->data[0] & p->mask) || !mach_inst (buf, p); ++p)
    ;
  p->fp (buf, info, p->text);

  return buf->n_used;
}

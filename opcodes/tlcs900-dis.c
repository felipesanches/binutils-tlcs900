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
  signed char data[8];
  long inss; /* instruction set bit mask, taken from bfd_mach */
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

#define TXTSIZ 32
/* Names of 8-bit registers.  */
static const char * r_str[]  = { "w", "a", "b", "c", "d", "e", "h", "l" };
/* Names of 16-bit registers.  */
static const char * rr_str[] = { "wa", "bc", "de", "hl", "ix", "iy", "iz", "sp" };
/* Names of 32-bit registers.  */
static const char * xrr_str[] = { "xwa", "xbc", "xde", "xhl", "xix", "xiy", "xiz", "xsp" };
/* Texts for condition codes.  */
static const char * cc_str[] = { "f", "lt", "le", "ule", "pe/ov", "m/mi", "z", "c",
                                 "t", "ge", "gt", "ugt", "po/nov", "p/pl", "nz", "nc" };

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

// Review: Is `prt_` the same as `prt`?
static int
prt_ (struct buffer *buf, disassemble_info * info, const char *txt)
{
  info->fprintf_func (info->stream, txt);
  buf->n_used = buf->n_fetch;

  return buf->n_used;
}

static int
prt_n (struct buffer *buf, disassemble_info * info, const char *txt)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      info->fprintf_func (info->stream, txt, p[0]);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_nn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;
  if (fetch_data (buf, info, 2))
    {
      info->fprintf_func (info->stream, txt, (p[1] << 8) | p[0]);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_nnn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;
  if (fetch_data (buf, info, 3))
    {
      info->fprintf_func (info->stream, txt, (p[2] << 16) | (p[1] << 8) | p[0]);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_nnnn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;
  if (fetch_data (buf, info, 4))
    {
      info->fprintf_func (info->stream, txt, (p[3] << 24) | (p[2] << 16) | (p[1] << 8) | p[0]);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_3bit (struct buffer *buf, disassemble_info * info, const char *txt)
{
  int n;
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  n = p[-1] & 7;
  info->fprintf_func (info->stream, txt, n);
  buf->n_used = buf->n_fetch;

  return buf->n_used;
}

static int
prt_n_offset (struct buffer *buf, disassemble_info * info, const char *txt)
{
  int n;
  char *p = (char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = buf->base + buf->n_fetch + p[0];
      info->fprintf_func (info->stream, txt, n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_nn_offset (struct buffer *buf, disassemble_info * info, const char *txt)
{
  int n;
  int offset;
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 2))
    {
      offset = (p[0] | p[1]<<8);
      n = buf->base + buf->n_fetch + offset;
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
prt_r (struct buffer *buf, disassemble_info * info, const char *txt)
{
  info->fprintf_func (info->stream, txt,
		      r_str[buf->data[buf->n_fetch - 1] & 7]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_rr (struct buffer *buf, disassemble_info * info, const char *txt)
{
  info->fprintf_func (info->stream, txt,
		      rr_str[buf->data[buf->n_fetch - 1] & 7]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_xrr (struct buffer *buf, disassemble_info * info, const char *txt)
{
  info->fprintf_func (info->stream, txt,
		      xrr_str[buf->data[buf->n_fetch - 1] & 7]);
  buf->n_used = buf->n_fetch;
  return buf->n_used;
}

static int
prt_xrr_nnnn (struct buffer *buf, disassemble_info * info, const char *txt)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 4))
    {
      info->fprintf_func (info->stream, txt,
                          xrr_str[p[-1] & 7],
                          (p[3] << 24) | (p[2] << 16) | (p[1] << 8) | p[0]);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;


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
prt_cc (struct buffer *buf, disassemble_info * info, const char *txt)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  info->fprintf_func (info->stream, txt, cc_str[p[-1] & 0xf]);
  buf->n_used = buf->n_fetch;

  return buf->n_used;
}


static int
prt_cc_n_offset (struct buffer *buf, disassemble_info * info, const char *txt)
{
  int n;
  int offset;
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      offset = p[0];
      n = buf->base + buf->n_fetch + offset;
      info->fprintf_func (info->stream, txt, cc_str[p[-1] & 0xf], n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_cc_nn_offset (struct buffer *buf, disassemble_info * info, const char *txt)
{
  int n;
  int offset;
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 2))
    {
      offset = (p[0] | p[1]<<8);
      n = buf->base + buf->n_fetch + offset;
      info->fprintf_func (info->stream, txt, cc_str[p[-1] & 0xf], n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
print_insn_tlcs900_buf (struct buffer *buf, disassemble_info *info);

int
print_insn_tlcs900 (bfd_vma addr, disassemble_info * info);



/* Table to disassemble machine codes with reg.  */
static const struct tab_elt opc_reg[] =
{
  //TODO: { 0x03, 0xff, prt_,         "ld %%s, %s", INSS_ALL },
  //TODO: { 0x04, 0xff, prt_,         "push %s", INSS_ALL },
  //TODO: { 0x05, 0xff, prt_,         "pop %s", INSS_ALL },
  //TODO: { 0x06, 0xff, prt_,         "cpl %s", INSS_ALL },
  //TODO: { 0x07, 0xff, prt_,         "neg %s", INSS_ALL },
  //TODO: { 0x08, 0xff, prt_,         "mul %%s, %s", INSS_ALL },
  //TODO: { 0x09, 0xff, prt_,         "muls %%s, %s", INSS_ALL },
  //TODO: { 0x0a, 0xff, prt_,         "div %%s, %s", INSS_ALL },
  //TODO: { 0x0b, 0xff, prt_,         "divs %%s, %s", INSS_ALL },
  //TODO: { 0x0c, 0xff, prt_,         "link %%s, %s", INSS_ALL },
  //TODO: { 0x0d, 0xff, prt_,         "unlk %s", INSS_ALL },
  //TODO: { 0x0e, 0xff, prt_,         "bs1f a, %s", INSS_ALL },
  //TODO: { 0x0f, 0xff, prt_,         "bs1b a, %s", INSS_ALL },
  //TODO: { 0x10, 0xff, prt_,         "daa %s", INSS_ALL },
  { 0x12, 0xff, prt_,         "extz %s", INSS_ALL },
  //TODO: { 0x13, 0xff, prt_,         "exts %s", INSS_ALL },
  //TODO: { 0x14, 0xff, prt_,         "paa %s", INSS_ALL },
  //TODO: { 0x16, 0xff, prt_,         "mirr %s", INSS_ALL },
  //TODO: { 0x19, 0xff, prt_,         "mula %s", INSS_ALL },
  { 0x1c, 0xff, prt_n_offset, "djnz %s, 0x%%06x", INSS_ALL },
  //TODO: { 0x20, 0xff, prt_?,         "andcf ?, %s", INSS_ALL },
  //TODO: { 0x21, 0xff, prt_?,         "orcf ?, %s", INSS_ALL },
  //TODO: { 0x22, 0xff, prt_?,         "xorcf ?, %s", INSS_ALL },
  //TODO: { 0x23, 0xff, prt_?,         "ldcf ?, %s", INSS_ALL },
  //TODO: { 0x24, 0xff, prt_?,         "stcf ?, %s", INSS_ALL },
  //TODO: { 0x28, 0xff, prt_?,         "andcf a, %s", INSS_ALL },
  //TODO: { 0x29, 0xff, prt_?,         "orcf a, %s", INSS_ALL },
  //TODO: { 0x2a, 0xff, prt_?,         "xorcf a, %s", INSS_ALL },
  //TODO: { 0x2b, 0xff, prt_?,         "ldcf a, %s", INSS_ALL },
  //TODO: { 0x2c, 0xff, prt_?,         "stcf a, %s", INSS_ALL },
  //TODO: { 0x2e, 0xff, prt_?,         "ldc cr, %s", INSS_ALL },
  //TODO: { 0x2f, 0xff, prt_?,         "ldc %s, cr", INSS_ALL },
  //TODO: { 0x30, 0xff, prt_?,         "res ?, %s", INSS_ALL },
  //TODO: { 0x31, 0xff, prt_?,         "set ?, %s", INSS_ALL },
  //TODO: { 0x32, 0xff, prt_?,         "chg ?, %s", INSS_ALL },
  //TODO: { 0x33, 0xff, prt_?,         "bit ?, %s", INSS_ALL },
  //TODO: { 0x34, 0xff, prt_?,         "tset ?, %s", INSS_ALL },
  //TODO: { 0x38, 0xff, prt_?,         "minc1 ?, %s", INSS_ALL },
  //TODO: { 0x39, 0xff, prt_?,         "minc2 ?, %s", INSS_ALL },
  //TODO: { 0x3a, 0xff, prt_?,         "minc4 ?, %s", INSS_ALL },
  //TODO: { 0x3c, 0xff, prt_?,         "mdec1 ?, %s", INSS_ALL },
  //TODO: { 0x3d, 0xff, prt_?,         "mdec2 ?, %s", INSS_ALL },
  //TODO: { 0x3e, 0xff, prt_?,         "mdec4 ?, %s", INSS_ALL },
  //TODO: { 0x40, 0xf8, prt_?,         "mul R, %s", INSS_ALL },
  //TODO: { 0x48, 0xf8, prt_?,         "muls R, %s", INSS_ALL },
  //TODO: { 0x50, 0xf8, prt_?,         "div R, %s", INSS_ALL },
  //TODO: { 0x58, 0xf8, prt_?,         "divs R, %s", INSS_ALL },
  { 0x60, 0xff, prt_,         "inc 8, %s", INSS_ALL },
  { 0x61, 0xff, prt_,         "inc 1, %s", INSS_ALL },
  { 0x62, 0xff, prt_,         "inc 2, %s", INSS_ALL },
  { 0x63, 0xff, prt_,         "inc 3, %s", INSS_ALL },
  { 0x64, 0xff, prt_,         "inc 4, %s", INSS_ALL },
  { 0x65, 0xff, prt_,         "inc 5, %s", INSS_ALL },
  { 0x66, 0xff, prt_,         "inc 6, %s", INSS_ALL },
  { 0x67, 0xff, prt_,         "inc 7, %s", INSS_ALL },
  { 0x68, 0xff, prt_,         "dec 8, %s", INSS_ALL },
  { 0x69, 0xff, prt_,         "dec 1, %s", INSS_ALL },
  { 0x6a, 0xff, prt_,         "dec 2, %s", INSS_ALL },
  { 0x6b, 0xff, prt_,         "dec 3, %s", INSS_ALL },
  { 0x6c, 0xff, prt_,         "dec 4, %s", INSS_ALL },
  { 0x6d, 0xff, prt_,         "dec 5, %s", INSS_ALL },
  { 0x6e, 0xff, prt_,         "dec 6, %s", INSS_ALL },
  { 0x6f, 0xff, prt_,         "dec 7, %s", INSS_ALL },
  { 0x70, 0xF0, prt_cc,       "scc %%s, %s", INSS_ALL },
//TODO  { 0x80, 0xf8, prt_?,        "add %%s, %s", INSS_ALL },
//TODO  { 0x88, 0xf8, prt_?,        "ld %%s, %s", INSS_ALL },
//TODO  { 0x90, 0xf8, prt_?,        "adc %%s, %s", INSS_ALL },
//TODO  { 0x98, 0xf8, prt_?,        "ld %s, %%s", INSS_ALL },
//TODO  { 0xa0, 0xf8, prt_?,        "sub %%s, %s", INSS_ALL },
  { 0xa8, 0xf8, prt_3bit,     "ld %s, 0x%%02x", INSS_ALL },
//TODO  { 0xb0, 0xf8, prt_?,        "sbc %%s, %s", INSS_ALL },
//TODO  { 0xb8, 0xf8, prt_?,        "ex %%s, %s", INSS_ALL },
//TODO  { 0xc0, 0xf8, prt_?,        "and %%s, %s", INSS_ALL },
  { 0xc8, 0xff, prt_n,        "add %s, 0x%%02x", INSS_ALL },
  { 0xc9, 0xff, prt_n,        "adc %s, 0x%%02x", INSS_ALL },
  { 0xca, 0xff, prt_n,        "sub %s, 0x%%02x", INSS_ALL },
  { 0xcb, 0xff, prt_n,        "sbc %s, 0x%%02x", INSS_ALL },
  { 0xcc, 0xff, prt_n,        "and %s, 0x%%02x", INSS_ALL },
  { 0xcd, 0xff, prt_n,        "xor %s, 0x%%02x", INSS_ALL },
  { 0xce, 0xff, prt_n,        "or %s, 0x%%02x", INSS_ALL },
  { 0xcf, 0xff, prt_n,        "cp %s, 0x%%02x", INSS_ALL },
//TODO  { 0xd0, 0xf8, prt_?,        "xor %%s, %s", INSS_ALL },
  { 0xd8, 0xf8, prt_3bit,     "cp %s, %%d", INSS_ALL },
//TODO  { 0xe0, 0xf8, prt_?,        "or %%s, %s", INSS_ALL },
  { 0xe8, 0xff, prt_n,        "rlc 0x%%02x, %s", INSS_ALL },
  { 0xe9, 0xff, prt_n,        "rrc 0x%%02x, %s", INSS_ALL },
  { 0xea, 0xff, prt_n,        "rl 0x%%02x, %s", INSS_ALL },
  { 0xeb, 0xff, prt_n,        "rr 0x%%02x, %s", INSS_ALL },
  { 0xec, 0xff, prt_n,        "sla 0x%%02x, %s", INSS_ALL },
  { 0xed, 0xff, prt_n,        "sra 0x%%02x, %s", INSS_ALL },
  { 0xee, 0xff, prt_n,        "sll 0x%%02x, %s", INSS_ALL },
  { 0xef, 0xff, prt_n,        "srl 0x%%02x, %s", INSS_ALL },
//TODO  { 0xf0, 0xf8, prt_?,        "cp %%s, %s", INSS_ALL },
  { 0xf8, 0xff, prt_,         "rlc a, %s", INSS_ALL },
  { 0xf9, 0xff, prt_,         "rrc a, %s", INSS_ALL },
  { 0xfa, 0xff, prt_,         "rl a, %s", INSS_ALL },
  { 0xfb, 0xff, prt_,         "rr a, %s", INSS_ALL },
  { 0xfc, 0xff, prt_,         "sla a, %s", INSS_ALL },
  { 0xfd, 0xff, prt_,         "sra a, %s", INSS_ALL },
  { 0xfe, 0xff, prt_,         "sll a, %s", INSS_ALL },
  { 0xff, 0xff, prt_,         "srl a, %s", INSS_ALL },
};

static int
prt_regB (struct buffer *buf, disassemble_info *info,
         const char *txt ATTRIBUTE_UNUSED)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];
  unsigned char *ptr = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_reg; p->val != (ptr[0] & p->mask); ++p) { }
      snprintf (mytxt,TXTSIZ, p->text, r_str[ptr[-1] & 7]);
      p->fp (buf, info, mytxt);
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
  unsigned char *ptr = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_reg; p->val != (ptr[0] & p->mask); ++p) { }
      snprintf (mytxt,TXTSIZ, p->text, rr_str[ptr[-1] & 7]);
      p->fp (buf, info, mytxt);
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
  unsigned char *ptr = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_reg; p->val != (ptr[0] & p->mask); ++p) { }
      snprintf (mytxt,TXTSIZ, p->text, xrr_str[ptr[-1] & 7]);
      p->fp (buf, info, mytxt);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}


/* Table to disassemble machine codes with prefix 'srcB'.  */
static const struct tab_elt opc_srcB[] =
{
  { 0x20, 0xF8, prt_r, "ld %%s, (%s)", INSS_ALL },
  { 0x38, 0xFF, prt_n, "add (%s), 0x%%02x", INSS_ALL },
  { 0x39, 0xFF, prt_n, "adc (%s), 0x%%02x", INSS_ALL },
  { 0x3a, 0xFF, prt_n, "sub (%s), 0x%%02x", INSS_ALL },
  { 0x3b, 0xFF, prt_n, "sbc (%s), 0x%%02x", INSS_ALL },
  { 0x3c, 0xFF, prt_n, "and (%s), 0x%%02x", INSS_ALL },
  { 0x3d, 0xFF, prt_n, "xor (%s), 0x%%02x", INSS_ALL },
  { 0x3e, 0xFF, prt_n, "or (%s), 0x%%02x", INSS_ALL },
  { 0x3f, 0xFF, prt_n, "cp (%s), 0x%%02x", INSS_ALL },
};

static int
prt_srcB (struct buffer *buf, disassemble_info *info,
          const char *src)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];

  unsigned char *ptr;
  ptr = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_srcB; p->val != (ptr[0] & p->mask); ++p) { }
      snprintf (mytxt,TXTSIZ, p->text, src);
      p->fp (buf, info, mytxt);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

/* Table to disassemble machine codes with prefix 'srcW'.  */
static const struct tab_elt opc_srcW[] =
{
  { 0x20, 0xF8, prt_rr, "ld %%s, (%s)", INSS_ALL },
  { 0x38, 0xFF, prt_nn, "add (%s), 0x%%04x", INSS_ALL },
  { 0x39, 0xFF, prt_nn, "adc (%s), 0x%%04x", INSS_ALL },
  { 0x3a, 0xFF, prt_nn, "sub (%s), 0x%%04x", INSS_ALL },
  { 0x3b, 0xFF, prt_nn, "sbc (%s), 0x%%04x", INSS_ALL },
  { 0x3c, 0xFF, prt_nn, "and (%s), 0x%%04x", INSS_ALL },
  { 0x3d, 0xFF, prt_nn, "xor (%s), 0x%%04x", INSS_ALL },
  { 0x3e, 0xFF, prt_nn, "or (%s), 0x%%04x", INSS_ALL },
  { 0x3f, 0xFF, prt_nn, "cp (%s), 0x%%04x", INSS_ALL },
};

static int
prt_srcW (struct buffer *buf, disassemble_info *info,
          const char *src)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];

  unsigned char *ptr;
  ptr = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_srcW; p->val != (ptr[0] & p->mask); ++p) { }
      snprintf (mytxt,TXTSIZ, p->text, src);
      p->fp (buf, info, mytxt);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

/* Table to disassemble machine codes with prefix 'srcL'.  */
static const struct tab_elt opc_srcL[] =
{
  { 0x20, 0xF8, prt_xrr, "ld %%s, (%s)", INSS_ALL },
  { 0x38, 0xFF, prt_nnnn, "add (%s), 0x%%08x", INSS_ALL },
  { 0x39, 0xFF, prt_nnnn, "adc (%s), 0x%%08x", INSS_ALL },
  { 0x3a, 0xFF, prt_nnnn, "sub (%s), 0x%%08x", INSS_ALL },
  { 0x3b, 0xFF, prt_nnnn, "sbc (%s), 0x%%08x", INSS_ALL },
  { 0x3c, 0xFF, prt_nnnn, "and (%s), 0x%%08x", INSS_ALL },
  { 0x3d, 0xFF, prt_nnnn, "xor (%s), 0x%%08x", INSS_ALL },
  { 0x3e, 0xFF, prt_nnnn, "or (%s), 0x%%08x", INSS_ALL },
  { 0x3f, 0xFF, prt_nnnn, "cp (%s), 0x%%08x", INSS_ALL },
};

static int
prt_srcL (struct buffer *buf, disassemble_info *info,
          const char *src)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];

  unsigned char *ptr;
  ptr = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_srcL; p->val != (ptr[0] & p->mask); ++p) { }
      snprintf (mytxt,TXTSIZ, p->text, src);
      p->fp (buf, info, mytxt);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_srcB_xrr (struct buffer *buf, disassemble_info * info,
              const char *txt ATTRIBUTE_UNUSED)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;
  buf->n_used = buf->n_fetch;

  return prt_srcB (buf, info, xrr_str[p[-1] & 7]);
}

static int
prt_srcB_n (struct buffer *buf, disassemble_info * info,
            const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nn;
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      nn = p[0];
      snprintf (src, TXTSIZ, "0x%02x", nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcB (buf, info, src);
}

static int
prt_srcB_nn (struct buffer *buf, disassemble_info * info,
             const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 2))
    {
      nn = (p[1] << 8) | p[0];
      snprintf (src, TXTSIZ, "0x%04x", nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcB (buf, info, src);
}

static int
prt_srcB_nnn (struct buffer *buf, disassemble_info * info,
              const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nnn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 3))
    {
      nnn = (p[2] << 16) | (p[1] << 8) | p[0];
      snprintf (src, TXTSIZ, "0x%06x", nnn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcB (buf, info, src);
}

static int
prt_srcW_n (struct buffer *buf, disassemble_info * info,
            const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nn;
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      nn = p[0];
      snprintf (src, TXTSIZ, "0x%02x", nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcW (buf, info, src);
}

static int
prt_srcW_nn (struct buffer *buf, disassemble_info * info,
             const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 2))
    {
      nn = (p[1] << 8) | p[0];
      snprintf (src, TXTSIZ, "0x%04x", nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcW (buf, info, src);
}

static int
prt_srcW_nnn (struct buffer *buf, disassemble_info * info,
              const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nnn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 3))
    {
      nnn = (p[2] << 16) | (p[1] << 8) | p[0];
      snprintf (src, TXTSIZ, "0x%06x", nnn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcW (buf, info, src);
}

static int
prt_srcL_n (struct buffer *buf, disassemble_info * info,
            const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nn;
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      nn = p[0];
      snprintf (src, TXTSIZ, "0x%02x", nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcL (buf, info, src);
}

static int
prt_srcL_nn (struct buffer *buf, disassemble_info * info,
             const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 2))
    {
      nn = (p[1] << 8) | p[0];
      snprintf (src, TXTSIZ, "0x%04x", nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcL (buf, info, src);
}

static int
prt_srcL_nnn (struct buffer *buf, disassemble_info * info,
              const char *txt ATTRIBUTE_UNUSED)
{
  char src[TXTSIZ];
  int nnn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 3))
    {
      nnn = (p[2] << 16) | (p[1] << 8) | p[0];
      snprintf (src, TXTSIZ, "0x%06x", nnn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_srcL (buf, info, src);
}

/* Table to disassemble machine codes with prefix 'dst'.  */
static const struct tab_elt opc_dst[] =
{
  { 0x00, 0xFF, prt_n,    "ld (%s), 0x%%02x", INSS_ALL },
  { 0x02, 0xFF, prt_nn,   "ld (%s), 0x%%04x", INSS_ALL },
//TODO: { 0x04, 0xFF, prt_?,    "pop.B (%s)", INSS_ALL },
//TODO: { 0x06, 0xFF, prt_?,    "pop.W (%s)", INSS_ALL },
//TODO: { 0x14, 0xFF, prt_?,    "ld.B (%s), (%s)", INSS_ALL },
//TODO: { 0x16, 0xFF, prt_?,    "ld.W (%s), (%s)", INSS_ALL },
  { 0x20, 0xF8, prt_rr,   "lda %%s, %s", INSS_ALL },
//TODO: { 0x28, 0xFF, prt_?,    "andcf a, (%s)", INSS_ALL },
//TODO: { 0x29, 0xFF, prt_?,    "orcf a, (%s)", INSS_ALL },
//TODO: { 0x2a, 0xFF, prt_?,    "xorcf a, (%s)", INSS_ALL },
//TODO: { 0x2b, 0xFF, prt_?,    "ldcf a, (%s)", INSS_ALL },
//TODO: { 0x2c, 0xFF, prt_?,    "stcf.B a, (%s)", INSS_ALL },
  { 0x30, 0xF8, prt_xrr,  "lda %%s, %s", INSS_ALL },
  { 0x40, 0xF8, prt_r,    "ld (%s), %%s", INSS_ALL },
  { 0x50, 0xF8, prt_rr,   "ld (%s), %%s", INSS_ALL },
  { 0x60, 0xF8, prt_xrr,  "ld (%s), %%s", INSS_ALL },
  { 0x80, 0xF8, prt_3bit, "andcf %%d, (%s)", INSS_ALL },
  { 0x88, 0xF8, prt_3bit, "orcf %%d, (%s)", INSS_ALL },
  { 0x90, 0xF8, prt_3bit, "xorcf %%d, (%s)", INSS_ALL },
  { 0x98, 0xF8, prt_3bit, "ldcf %%d, (%s)", INSS_ALL },
  { 0xA0, 0xF8, prt_3bit, "stcf %%d, (%s)", INSS_ALL },
  { 0xA8, 0xF8, prt_3bit, "tset %%d, (%s)", INSS_ALL },
  { 0xB0, 0xF8, prt_3bit, "res %%d, (%s)", INSS_ALL },
  { 0xB8, 0xF8, prt_3bit, "set %%d, (%s)", INSS_ALL },
  { 0xC0, 0xF8, prt_3bit, "chg %%d, (%s)", INSS_ALL },
  { 0xC8, 0xF8, prt_3bit, "bit %%d, (%s)", INSS_ALL },
  { 0xD0, 0xF0, prt_cc,   "jp %%s, %s", INSS_ALL },
  { 0xE0, 0xF0, prt_cc,   "call %%s, %s", INSS_ALL },
  { 0xF0, 0xF0, prt_cc,   "ret %%s", INSS_ALL },
};

static int
prt_dst (struct buffer *buf, disassemble_info *info,
         const char *dst)
{
  const struct tab_elt *p;
  char mytxt[TXTSIZ];

  unsigned char *ptr;
  ptr = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      for (p = opc_dst; p->val != (ptr[0] & p->mask); ++p) { }
      snprintf (mytxt,TXTSIZ, p->text, dst);
      p->fp (buf, info, mytxt);
    }
  else
    buf->n_used = -1;

  return buf->n_used;
}

static int
prt_dst_xrr (struct buffer *buf, disassemble_info * info,
              const char *txt ATTRIBUTE_UNUSED)
{
  unsigned char *p = (unsigned char*) buf->data + buf->n_fetch;
  buf->n_used = buf->n_fetch;

  return prt_dst (buf, info, xrr_str[p[-1] & 7]);
}

static int
prt_dst_n (struct buffer *buf, disassemble_info * info,
           const char *txt ATTRIBUTE_UNUSED)
{
  char dst[TXTSIZ];
  int n;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 1))
    {
      n = p[0];
      snprintf (dst, TXTSIZ, "0x%02x", n);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_dst (buf, info, dst);
}

static int
prt_dst_nn (struct buffer *buf, disassemble_info * info,
            const char *txt ATTRIBUTE_UNUSED)
{
  char dst[TXTSIZ];
  int nn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 2))
    {
      nn = (p[1] << 8) | p[0];
      snprintf (dst, TXTSIZ, "0x%04x", nn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_dst (buf, info, dst);
}

static int
prt_dst_nnn (struct buffer *buf, disassemble_info * info,
             const char *txt ATTRIBUTE_UNUSED)
{
  char dst[TXTSIZ];
  int nnn;
  unsigned char *p;
  p = (unsigned char*) buf->data + buf->n_fetch;

  if (fetch_data (buf, info, 3))
    {
      nnn = (p[2] << 16) | (p[1] << 8) | p[0];
      snprintf (dst, TXTSIZ, "0x%06x", nnn);
      buf->n_used = buf->n_fetch;
    }
  else
    buf->n_used = -1;

  return prt_dst (buf, info, dst);
}


/* Table to disassemble machine codes without prefix.  */
static const struct tab_elt
opc_main[] =
{
  { 0x00, 0xff, prt,              "nop", INSS_ALL },
  { 0x01, 0xff, prt,              "normal", INSS_TLCS900 },
  { 0x02, 0xff, prt,              "push sr", INSS_ALL },
  { 0x03, 0xff, prt,              "pop sr", INSS_ALL },
  { 0x04, 0xff, prt,              "max", INSS_TLCS900 },
  { 0x04, 0xff, prt,              "min", INSS_TLCS900L }, /* TODO: review this one. */
  { 0x05, 0xff, prt,              "halt", INSS_ALL },
  { 0x06, 0xff, prt_n,            "ei 0x%02x", INSS_ALL },
  { 0x07, 0xff, prt,              "reti", INSS_ALL },
  { 0x08, 0xff, prt_n_n,          "ld (0x%02x), 0x%%02x", INSS_ALL },
  { 0x09, 0xff, prt_n,            "push 0x%02x", INSS_ALL },
  { 0x0a, 0xff, prt_n_nn,         "ld (0x%02x), 0x%%04x", INSS_ALL },
  { 0x0b, 0xff, prt_nn,           "pushw 0x%04x", INSS_ALL },
  { 0x0c, 0xff, prt,              "incf", INSS_ALL },
  { 0x0d, 0xff, prt,              "decf", INSS_ALL },
  { 0x0e, 0xff, prt,              "ret", INSS_ALL },
  { 0x0f, 0xff, prt_nn,           "retd 0x%04x", INSS_ALL },
  { 0x10, 0xff, prt,              "rcf", INSS_ALL },
  { 0x11, 0xff, prt,              "scf", INSS_ALL },
  { 0x12, 0xff, prt,              "ccf", INSS_ALL },
  { 0x13, 0xff, prt,              "zcf", INSS_ALL },
  { 0x14, 0xff, prt,              "push a", INSS_ALL },
  { 0x15, 0xff, prt,              "pop a", INSS_ALL },
  { 0x16, 0xff, prt,              "ex f, f'", INSS_ALL },
  { 0x17, 0xff, prt_n,            "ldf 0x%02x", INSS_ALL },
  { 0x18, 0xff, prt,              "push f", INSS_ALL },
  { 0x19, 0xff, prt,              "pop f", INSS_ALL },
  { 0x1a, 0xff, prt_nn,           "jp 0x%04x", INSS_ALL },
  { 0x1b, 0xff, prt_nnn,          "jp 0x%06x", INSS_ALL },
  { 0x1c, 0xff, prt_nn,           "call 0x%04x", INSS_ALL },
  { 0x1d, 0xff, prt_nnn,          "call 0x%06x", INSS_ALL },
  { 0x1e, 0xff, prt_nn_offset,    "calr 0x%06x", INSS_ALL },
 // 0x1f, 0xff: invalid
  { 0x20, 0xf8, prt_r_n,          "ld %s, 0x%%02x", INSS_ALL },
  { 0x28, 0xf8, prt_rr,           "push %s", INSS_ALL },
  { 0x30, 0xf8, prt_rr_nn,        "ld %s, 0x%%04x", INSS_ALL },
  { 0x38, 0xf8, prt_xrr,          "push %s", INSS_ALL },
  { 0x40, 0xf8, prt_xrr_nnnn,     "ld %s, 0x%08x", INSS_ALL },
  { 0x48, 0xf8, pop_rr,           "pop %s", INSS_ALL },
 // 0x50, 0xf8: invalid
  { 0x58, 0xf8, prt_xrr,          "pop %s", INSS_ALL },
  { 0x60, 0xF0, prt_cc_n_offset,  "jr %s, 0x%06x", INSS_ALL },
  { 0x70, 0xF0, prt_cc_nn_offset, "jrl %s, 0x%06x", INSS_ALL },
  { 0x80, 0xf8, prt_srcB_xrr,     "", INSS_ALL },
  { 0x88, 0xf8, prt_xrr_d,        "src.B (%s + d)", INSS_ALL },
  { 0x90, 0xf8, prt_xrr,          "src.W (%s)", INSS_ALL },
  { 0x98, 0xf8, prt_xrr_d,        "src.W (%s + d)", INSS_ALL },
  { 0xa0, 0xf8, prt_xrr,          "src.L (%s)", INSS_ALL },
  { 0xa8, 0xf8, prt_xrr_d,        "src.L (%s + d)", INSS_ALL },
  { 0xb0, 0xf8, prt_dst_xrr,      "", INSS_ALL },
  { 0xb8, 0xf8, prt_xrr_d,        "dst (%s + d)", INSS_ALL },
  { 0xc0, 0xff, prt_srcB_n,       "", INSS_ALL },
  { 0xc1, 0xff, prt_srcB_nn,      "", INSS_ALL },
  { 0xc2, 0xff, prt_srcB_nnn,     "", INSS_ALL },
  { 0xc3, 0xff, prt,              "TODO: C3", INSS_ALL },
  { 0xc4, 0xff, prt,              "TODO: C4", INSS_ALL },
  { 0xc5, 0xff, prt,              "TODO: C5", INSS_ALL },
 // 0xc6, 0xff: invalid
  { 0xc7, 0xff, prt,              "TODO: C7", INSS_ALL },
  { 0xc8, 0xf8, prt_regB,         "", INSS_ALL },
  { 0xd0, 0xff, prt_srcW_n,       "", INSS_ALL },
  { 0xd1, 0xff, prt_srcW_nn,      "", INSS_ALL },
  { 0xd2, 0xff, prt_srcW_nnn,     "", INSS_ALL },
  { 0xd3, 0xff, prt,              "TODO: D3", INSS_ALL },
  { 0xd4, 0xff, prt,              "TODO: D4", INSS_ALL },
  { 0xd5, 0xff, prt,              "TODO: D5", INSS_ALL },
 // 0xd6, 0xff: invalid
  { 0xd7, 0xff, prt,              "TODO: D7", INSS_ALL },
  { 0xd8, 0xf8, prt_regW,         "", INSS_ALL },
  { 0xe0, 0xff, prt_srcL_n,       "", INSS_ALL },
  { 0xe1, 0xff, prt_srcL_nn,      "", INSS_ALL },
  { 0xe2, 0xff, prt_srcL_nnn,     "", INSS_ALL },
  { 0xe3, 0xff, prt,              "TODO: E3", INSS_ALL },
  { 0xe4, 0xff, prt,              "TODO: E4", INSS_ALL },
  { 0xe5, 0xff, prt,              "TODO: E5", INSS_ALL },
 // 0xe6, 0xff: invalid
  { 0xe7, 0xff, prt,              "TODO: E7", INSS_ALL },
  { 0xe8, 0xf8, prt_regL,         "", INSS_ALL },
  { 0xf0, 0xff, prt_dst_n,        "", INSS_ALL },
  { 0xf1, 0xff, prt_dst_nn,       "", INSS_ALL },
  { 0xf2, 0xff, prt_dst_nnn,      "", INSS_ALL },
  { 0xf3, 0xff, prt_dst,          "", INSS_ALL },
  { 0xf4, 0xff, prt_dst,          "", INSS_ALL },
  { 0xf5, 0xff, prt_dst,          "", INSS_ALL },
 // 0xf6, 0xff: invalid
  { 0xf7, 0xff, prt_n_n,          "ldx (0x%02x), 0x%02x", INSS_ALL },
  { 0xf8, 0xff, prt,              "swi 0", INSS_ALL },
  { 0xf9, 0xff, prt,              "swi 1", INSS_ALL },
  { 0xfa, 0xff, prt,              "swi 2", INSS_ALL },
  { 0xfb, 0xff, prt,              "swi 3", INSS_ALL },
  { 0xfc, 0xff, prt,              "swi 4", INSS_ALL },
  { 0xfd, 0xff, prt,              "swi 5", INSS_ALL },
  { 0xfe, 0xff, prt,              "swi 6", INSS_ALL },
  { 0xff, 0xff, prt,              "swi 7", INSS_ALL },
} ;

int
print_insn_tlcs900 (bfd_vma addr, disassemble_info * info)
{
  struct buffer buf;

  buf.base = addr;
  info->bytes_per_line = 7;
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

  for (; p->val != (buf->data[0] & p->mask); ++p)
    ;
  p->fp (buf, info, p->text);

  return buf->n_used;
}

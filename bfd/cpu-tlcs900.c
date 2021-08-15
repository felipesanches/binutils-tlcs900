/* BFD library support routines for the TLCS900 architecture.
   Copyright (C) 2005-2021 Free Software Foundation, Inc.
   Contributed by Felipe Correa da Silva Sanches <juca@members.fsf.org>

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "bfd.h"
#include "libbfd.h"

const bfd_arch_info_type bfd_tlcs900_arch;

/* This routine is provided two arch_infos and
   returns whether they'd be compatible.  */
/* FIXME! I need to review this carefully. ~FSanches */
static const bfd_arch_info_type *
compatible (const bfd_arch_info_type *a, const bfd_arch_info_type *b)
{
  if (a->arch != b->arch || a->arch != bfd_arch_tlcs900)
    return NULL;

  if (a->mach == b->mach)
    return a;

  return NULL;
}

#define N(name,print,bits,default,next)  \
 { 16, bits, 8, bfd_arch_tlcs900, name, "tlcs900", print, 0, default, \
   compatible, bfd_default_scan, bfd_arch_default_fill, next, 0 }

#define M(n) &arch_info_struct[n]

static const bfd_arch_info_type arch_info_struct[] =
{
  N (bfd_mach_tlcs900,	 "tlcs900",        16, true,  M(1)),
  N (bfd_mach_tlcs900l, "tlcs900l", 16, false, NULL)
};

const bfd_arch_info_type bfd_tlcs900_arch =
  N (bfd_mach_tlcs900,   "tlcs900",   16, true,  M(1));

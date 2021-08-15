/* TLCS900 ELF support for BFD.
   Copyright (C) 1999-2021 Free Software Foundation, Inc.
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
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _ELF_TLCS900_H
#define _ELF_TLCS900_H

#include "elf/reloc-macros.h"

/* Processor specific flags for the ELF header e_flags field.  */
#define EF_TLCS900_MACH_TLCS900      0x01
#define EF_TLCS900_MACH_TLCS900L     0x02
#define EF_TLCS900_MACH_MSK          0xff

/* Relocations.  */
START_RELOC_NUMBERS (elf_tlcs900_reloc_type)
     RELOC_NUMBER (R_TLCS900_NONE,		0)
     RELOC_NUMBER (R_TLCS900_8, 		1)
     RELOC_NUMBER (R_TLCS900_8_DIS,		2)
     RELOC_NUMBER (R_TLCS900_8_PCREL,	3)
     RELOC_NUMBER (R_TLCS900_16, 		4)
     RELOC_NUMBER (R_TLCS900_24, 		5)
     RELOC_NUMBER (R_TLCS900_32, 		6)
     RELOC_NUMBER (R_TLCS900_BYTE0,		7)
     RELOC_NUMBER (R_TLCS900_BYTE1,		8)
     RELOC_NUMBER (R_TLCS900_BYTE2,		9)
     RELOC_NUMBER (R_TLCS900_BYTE3,		10)
     RELOC_NUMBER (R_TLCS900_WORD0,		11)
     RELOC_NUMBER (R_TLCS900_WORD1,		12)
     RELOC_NUMBER (R_TLCS900_16_BE,		13)
END_RELOC_NUMBERS (R_TLCS900_max)

#endif /* _ELF_TLCS900_H */

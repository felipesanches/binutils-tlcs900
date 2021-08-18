/* this is tc-tlcs900.h
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

   Contributed by Felipe Correa da Silva Sanches <juca@members.fsf.org>

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of .the GNU General Public License as published by
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

#ifndef TC_TLCS900
#define TC_TLCS900

#define TARGET_ARCH   bfd_arch_tlcs900
#ifndef OBJ_COFF
#define TARGET_FORMAT "elf32-tlcs900"
#endif
#define BFD_ARCH      TARGET_ARCH
#define COFF_MAGIC    0x5A80
#define TARGET_BYTES_BIG_ENDIAN  0

/* If you define this macro, GAS will warn about the
   use of nonstandard escape sequences in a string.  */
#define ONLY_STANDARD_ESCAPES

/* GAS will call this function for any expression that can not be
   recognized.  When the function is called, `input_line_pointer'
   will point to the start of the expression.  */
#define md_operand(x)

/* This should just call either `number_to_chars_bigendian' or
   `number_to_chars_littleendian', whichever is appropriate.  On
   targets like the MIPS which support options to change the
   endianness, which function to call is a runtime decision.  On
   other targets, `md_number_to_chars' can be a simple macro.  */
#define md_number_to_chars number_to_chars_littleendian

#define TC_COUNT_RELOC(x) 1

#define TC_COFF_FIX2RTYPE(fixP) tc_coff_fix2rtype (fixP)
#define md_convert_frag(b,s,f)   as_fatal ("convert_frag called\n")
#define md_estimate_size_before_relax(f,s) \
  (as_fatal (_("estimate_size_before_relax called")), 1)

/* Define some functions to be called by generic code.  */
#define md_end               tlcs900_md_end
#define md_start_line_hook() { if (tlcs900_start_line_hook ()) continue; }
#define TC_CONS_FIX_NEW(f,w,s,e,r)  tlcs900_cons_fix_new ((f), (w), (s), (e))

extern void tlcs900_md_end (void);
extern int tlcs900_start_line_hook (void);
extern void tlcs900_cons_fix_new (fragS *, int, int, expressionS *);

#define WORKING_DOT_WORD

/* If you define this macro, it means that `tc_gen_reloc' may return
   multiple relocation entries for a single fixup.  In this case, the
   return value of `tc_gen_reloc' is a pointer to a null terminated
   array.  */
#undef RELOC_EXPANSION_POSSIBLE

/* No shared lib support, so we don't need to ensure
   externally visible symbols can be overridden.  */
#define EXTERN_FORCE_RELOC 0

/* Values passed to md_apply_fix3 don't include the symbol value.  */
#define MD_APPLY_SYM_VALUE(FIX) 0

#define LISTING_WORD_SIZE 1

/* A single '=' is accepted as a comparison operator.  */
#define O_SINGLE_EQ O_eq

/* A '$' is used to refer to the current location or as a hex. prefix.  */
#define DOLLAR_DOT
#define DOLLAR_AMBIGU                1
#define LOCAL_LABEL_PREFIX           '.'
#define LOCAL_LABELS_FB              1
#define LOCAL_LABELS_DOLLAR          1
#define LITERAL_PREFIXPERCENT_BIN
#define NUMBERS_WITH_SUFFIX          1
#define NO_PSEUDO_DOT                1
/* We allow single quotes to delimit character constants as
   well, but it is cleaner to handle that in tc-tlcs900.c.  */
#define SINGLE_QUOTE_STRINGS

#define LABELS_WITHOUT_COLONS (tlcs900_tc_labels_without_colon())
extern int tlcs900_tc_labels_without_colon (void);

/* An `.lcomm' directive with no explicit alignment parameter will
   use this macro to set P2VAR to the alignment that a request for
   SIZE bytes will have.  The alignment is expressed as a power of
   two.  If no alignment should take place, the macro definition
   should do nothing.  Some targets define a `.bss' directive that is
   also affected by this macro.  The default definition will set
   P2VAR to the truncated power of two of sizes up to eight bytes.  */
#define TC_IMPLICIT_LCOMM_ALIGNMENT(SIZE, P2VAR) (P2VAR) = 0

/* It does not make any sense to perform arithmetic on the numbers
   we use to identify registers.  */
#define md_register_arithmetic 0

#define TC_LABEL_IS_LOCAL tlcs900_tc_label_is_local
extern int tlcs900_tc_label_is_local (const char *name);

#define elf_tc_final_processing	tlcs900_elf_final_processing
extern void tlcs900_elf_final_processing (void);

/* Define the column that represents the PC.  */
#define DWARF2_DEFAULT_RETURN_COLUMN	5

/* The stack grows down, and is only byte aligned.  */
#define DWARF2_CIE_DATA_ALIGNMENT	-1

/* TLCS900 instructions are 1 or 4 bytes long.  */
#define DWARF2_LINE_MIN_INSN_LENGTH	1

/* 16 bits addresses are used on TLCS900.  */
#define DWARF2_ADDR_SIZE(bfd)		tlcs900_dwarf2_addr_size(bfd)
extern int tlcs900_dwarf2_addr_size (const bfd *abfd);

/* CFI hooks.  */
#define tc_cfi_frame_initial_instructions tlcs900_tc_frame_initial_instructions
extern void tlcs900_tc_frame_initial_instructions (void);

#define tc_regname_to_dw2regnum tlcs900_tc_regname_to_dw2regnum
extern int tlcs900_tc_regname_to_dw2regnum (const char *regname);

#endif

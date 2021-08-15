/* TOSHIBA TLCS900-specific support for 32-bit ELF
   Copyright (C) 1999-2021 Free Software Foundation, Inc.
   (Heavily copied from the S12Z port by Sergey Belyashov (sergey.belyashov@gmail.com))

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
#include "bfdlink.h"
#include "libbfd.h"
#include "elf-bfd.h"

#include "elf/tlcs900.h"

/* All users of this file have bfd_octets_per_byte (abfd, sec) == 1.  */
#define OCTETS_PER_BYTE(ABFD, SEC) 1

typedef const struct {
  bfd_reloc_code_real_type r_type;
  reloc_howto_type howto;
} bfd_howto_type;

#define BFD_EMPTY_HOWTO(rt,x) {rt, EMPTY_HOWTO(x)}
#define BFD_HOWTO(rt,a,b,c,d,e,f,g,h,i,j,k,l,m) {rt, HOWTO(a,b,c,d,e,f,g,h,i,j,k,l,m)}

static bfd_reloc_status_type
tlcs900_elf_16_be_reloc (bfd *abfd, arelent *reloc_entry, asymbol *symbol,
		     void *data, asection *input_section, bfd *output_bfd,
		     char **error_message);

static const
bfd_howto_type elf_tlcs900_howto_table[] =
{
  /* This reloc does nothing.  */
  BFD_HOWTO (BFD_RELOC_NONE,
	 R_TLCS900_NONE,		/* type */
	 0,			/* rightshift */
	 3,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_NONE",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 8 bit relocation */
  BFD_HOWTO (BFD_RELOC_8,
	 R_TLCS900_8,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm8",		/* name */
	 false,			/* partial_inplace */
	 0x00,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 8 bit index register displacement relocation */
  BFD_HOWTO (BFD_RELOC_TLCS900_DISP8,
	 R_TLCS900_8_DIS,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_off",		/* name */
	 false,			/* partial_inplace */
	 0x00,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 8 bit PC-rel relocation */
  BFD_HOWTO (BFD_RELOC_8_PCREL,
	 R_TLCS900_8_PCREL,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_jr",		/* name */
	 false,			/* partial_inplace */
	 0x00,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* An 16 bit absolute relocation */
  BFD_HOWTO (BFD_RELOC_16,
	 R_TLCS900_16,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm16",		/* name */
	 false,			/* partial_inplace */
	 0x00000000,		/* src_mask */
	 0x0000ffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* A 24 bit absolute relocation emitted by ADL mode operands */
  BFD_HOWTO (BFD_RELOC_24,
	 R_TLCS900_24,		/* type */
	 0,			/* rightshift */
	 5,			/* size (0 = byte, 1 = short, 2 = long) */
	 24,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm24",		/* name */
	 false,			/* partial_inplace */
	 0x00000000,		/* src_mask */
	 0x00ffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  BFD_HOWTO (BFD_RELOC_32,
	 R_TLCS900_32,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_imm32",		/* name */
	 false,			/* partial_inplace */
	 0x00000000,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* First (lowest) 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_TLCS900_BYTE0,
	 R_TLCS900_BYTE0,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte0",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* Second 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_TLCS900_BYTE1,
	 R_TLCS900_BYTE1,		/* type */
	 8,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte1",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* Third 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_TLCS900_BYTE2,
	 R_TLCS900_BYTE2,		/* type */
	 16,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte2",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* Fourth (highest) 8 bits of multibyte relocation */
  BFD_HOWTO (BFD_RELOC_TLCS900_BYTE3,
	 R_TLCS900_BYTE3,		/* type */
	 24,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_byte3",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* An 16 bit absolute relocation of lower word of multibyte value */
  BFD_HOWTO (BFD_RELOC_TLCS900_WORD0,
	 R_TLCS900_WORD0,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_word0",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* An 16 bit absolute relocation of higher word of multibyte value */
  BFD_HOWTO (BFD_RELOC_TLCS900_WORD1,
	 R_TLCS900_WORD1,		/* type */
	 16,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont,/* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "r_word1",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* An 16 bit big endian absolute relocation */
  BFD_HOWTO (BFD_RELOC_TLCS900_16_BE,
	 R_TLCS900_16_BE,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield,	/* complain_on_overflow */
	 tlcs900_elf_16_be_reloc,	/* special_function */
	 "r_imm16be",		/* name */
	 false,			/* partial_inplace */
	 0x00000000,		/* src_mask */
	 0x0000ffff,		/* dst_mask */
	 false),		/* pcrel_offset */
};

static reloc_howto_type *
tlcs900_reloc_type_lookup (bfd *abfd ATTRIBUTE_UNUSED,
		       bfd_reloc_code_real_type code)
{
  enum
    {
      table_size = sizeof (elf_tlcs900_howto_table) / sizeof (elf_tlcs900_howto_table[0])
    };
  unsigned int i;

  for (i = 0; i < table_size; i++)
    {
      if (elf_tlcs900_howto_table[i].r_type == code)
	  return &elf_tlcs900_howto_table[i].howto;
    }

  printf ("%s:%d Not found BFD reloc type %d\n", __FILE__, __LINE__, code);

  return NULL;
}

static reloc_howto_type *
tlcs900_reloc_name_lookup (bfd *abfd ATTRIBUTE_UNUSED, const char *r_name)
{
  enum
    {
      table_size = sizeof (elf_tlcs900_howto_table) / sizeof (elf_tlcs900_howto_table[0])
    };
  unsigned int i;

  for (i = 0; i < table_size; i++)
    {
      if (elf_tlcs900_howto_table[i].howto.name != NULL
	  && strcasecmp (elf_tlcs900_howto_table[i].howto.name, r_name) == 0)
	return &elf_tlcs900_howto_table[i].howto;
    }

  printf ("%s:%d Not found ELF reloc name `%s'\n", __FILE__, __LINE__, r_name);

  return NULL;
}

static reloc_howto_type *
tlcs900_rtype_to_howto (bfd *abfd, unsigned r_type)
{
  enum
    {
      table_size = sizeof (elf_tlcs900_howto_table) / sizeof (elf_tlcs900_howto_table[0])
    };
  unsigned int i;

  for (i = 0; i < table_size; i++)
    {
      if (elf_tlcs900_howto_table[i].howto.type == r_type)
	  return &elf_tlcs900_howto_table[i].howto;
    }

  /* xgettext:c-format */
  _bfd_error_handler (_("%pB: unsupported relocation type %#x"),
		      abfd, r_type);
  return NULL;
} 

/* Set the howto pointer for an tlcs900 ELF reloc.  */

static bool
tlcs900_info_to_howto_rela (bfd *abfd, arelent *cache_ptr, Elf_Internal_Rela *dst)
{
  unsigned int  r_type = ELF32_R_TYPE (dst->r_info);
  reloc_howto_type *howto = tlcs900_rtype_to_howto (abfd, r_type);
  if (howto != NULL)
    {
      cache_ptr->howto = howto;
      return true;
    }
  bfd_set_error (bfd_error_bad_value);
  return false;
}

static bfd_reloc_status_type
tlcs900_elf_final_link_relocate (unsigned long r_type,
			     bfd *input_bfd,
			     bfd *output_bfd ATTRIBUTE_UNUSED,
			     asection *input_section ATTRIBUTE_UNUSED,
			     bfd_byte *contents,
			     bfd_vma offset,
			     bfd_vma value,
			     bfd_vma addend,
			     struct bfd_link_info *info ATTRIBUTE_UNUSED,
			     asection *sym_sec ATTRIBUTE_UNUSED,
			     int is_local ATTRIBUTE_UNUSED)
{
  bool r;
  reloc_howto_type *howto;

  switch (r_type)
    {
    case R_TLCS900_16_BE:
      value += addend;
      bfd_put_8 (input_bfd, value >> 8, contents + offset + 0);
      bfd_put_8 (input_bfd, value >> 0, contents + offset + 1);
      return bfd_reloc_ok;
    }

  howto = tlcs900_rtype_to_howto (input_bfd, r_type);
  if (howto == NULL)
    return bfd_reloc_notsupported;

  r = _bfd_final_link_relocate (howto, input_bfd, input_section, contents,
				offset, value, addend);
  return r ? bfd_reloc_ok : bfd_reloc_notsupported;
}

static int
tlcs900_elf_relocate_section (bfd *output_bfd,
			  struct bfd_link_info *info,
			  bfd *input_bfd,
			  asection *input_section,
			  bfd_byte *contents,
			  Elf_Internal_Rela *relocs,
			  Elf_Internal_Sym *local_syms,
			  asection **local_sections)
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  Elf_Internal_Rela *rel, *relend;

  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (input_bfd);

  rel = relocs;
  relend = relocs + input_section->reloc_count;
  for (; rel < relend; rel++)
    {
      unsigned int r_type;
      unsigned long r_symndx;
      Elf_Internal_Sym *sym;
      asection *sec;
      struct elf_link_hash_entry *h;
      bfd_vma relocation;

      /* This is a final link.  */
      r_symndx = ELF32_R_SYM (rel->r_info);
      r_type = ELF32_R_TYPE (rel->r_info);
      h = NULL;
      sym = NULL;
      sec = NULL;
      if (r_symndx < symtab_hdr->sh_info)
	{
	  sym = local_syms + r_symndx;
	  sec = local_sections[r_symndx];
	  relocation = _bfd_elf_rela_local_sym (output_bfd, sym, &sec, rel);
	}
      else
	{
	  bool unresolved_reloc, warned, ignored;

	  RELOC_FOR_GLOBAL_SYMBOL (info, input_bfd, input_section, rel,
				   r_symndx, symtab_hdr, sym_hashes,
				   h, sec, relocation,
				   unresolved_reloc, warned, ignored);
	}

      if (sec != NULL && discarded_section (sec))
	{
	  /* For relocs against symbols from removed linkonce sections,
	     or sections discarded by a linker script, we just want the
	     section contents cleared.  Avoid any special processing.  */
	  reloc_howto_type *howto;
	  howto = tlcs900_rtype_to_howto (input_bfd, r_type);
	  RELOC_AGAINST_DISCARDED_SECTION (info, input_bfd, input_section,
					   rel, 1, relend, howto, 0, contents);
	}

      if (bfd_link_relocatable (info))
	continue;


      tlcs900_elf_final_link_relocate (r_type, input_bfd, output_bfd,
				   input_section,
				   contents, rel->r_offset,
				   relocation, rel->r_addend,
				   info, sec, h == NULL);
    }

  return true;
}

/* The final processing done just before writing out a TLCS900 ELF object
   file.  This gets the TLCS900 architecture right based on the machine
   number.  */

static bool
tlcs900_elf_final_write_processing (bfd *abfd)
{
  unsigned long val = bfd_get_mach (abfd);

  switch (val)
    {
    default:
      _bfd_error_handler (_("%pB: unsupported bfd mach %#lx"),
			  abfd, val);
      /* fall through */
    case bfd_mach_tlcs900:
      val = EF_TLCS900_MACH_TLCS900;
      break;
    case bfd_mach_tlcs900l:
      val = EF_TLCS900_MACH_TLCS900L;
      break;
    }
  elf_elfheader (abfd)->e_machine = EM_TLCS900;
  elf_elfheader (abfd)->e_flags &= ~EF_TLCS900_MACH_MSK;
  elf_elfheader (abfd)->e_flags |= val;
  return _bfd_elf_final_write_processing (abfd);
}

/* Set the right machine number.  */
static bool
tlcs900_elf_object_p (bfd *abfd)
{
  unsigned int mach;

  if (elf_elfheader (abfd)->e_machine == EM_TLCS900)
    {
      int e_mach = elf_elfheader (abfd)->e_flags & EF_TLCS900_MACH_MSK;
      switch (e_mach)
	{
	default:
	  _bfd_error_handler (_("%pB: unsupported mach %#x"),
			      abfd, e_mach);
	  /* fall through */
	case EF_TLCS900_MACH_TLCS900:
	  mach = bfd_mach_tlcs900;
	  break;
	case EF_TLCS900_MACH_TLCS900L:
	  mach = bfd_mach_tlcs900l;
	  break;
	}
    }
  else
    {
      _bfd_error_handler (_("%pB: unsupported arch %#x"),
			  abfd, elf_elfheader (abfd)->e_machine);
      mach = bfd_mach_tlcs900;
    }
  return bfd_default_set_arch_mach (abfd, bfd_arch_tlcs900, mach);
}

static bool
tlcs900_is_local_label_name (bfd *	abfd ATTRIBUTE_UNUSED,
			 const char * name)
{
  return (name[0] == '.' && name[1] == 'L') ||
	 _bfd_elf_is_local_label_name (abfd, name);
}

static bfd_reloc_status_type
tlcs900_elf_16_be_reloc (bfd *abfd,
		     arelent *reloc_entry,
		     asymbol *symbol,
		     void *data,
		     asection *input_section,
		     bfd *output_bfd,
		     char **error_message)
{
  bfd_vma val;
  long x;
  bfd_size_type octets = (reloc_entry->address
			  * OCTETS_PER_BYTE (abfd, input_section));

  /* If this is a relocatable link (output_bfd test tells us), just
     call the generic function.  Any adjustment will be done at final
     link time.  */
  if (output_bfd != NULL)
    return bfd_elf_generic_reloc (abfd, reloc_entry, symbol, data,
				  input_section, output_bfd, error_message);

  /* Get symbol value.  */
  val = 0;
  if (!bfd_is_com_section (symbol->section))
    val = symbol->value;
  val += symbol->section->output_offset + input_section->output_offset;
  if (symbol->section->output_section)
    val += symbol->section->output_section->vma;

  val += reloc_entry->addend;
  if (reloc_entry->howto->partial_inplace)
    {
      x = bfd_get_8 (abfd, (bfd_byte *) data + octets + 0) * 0x100;
      x += bfd_get_8 (abfd, (bfd_byte *) data + octets + 1);
      x &= ~reloc_entry->howto->src_mask;
    }
  else
    x = 0;

  x |= val & reloc_entry->howto->dst_mask;
  if (x < -0x8000 || x >= 0x10000)
    return bfd_reloc_outofrange;

  bfd_put_8 (abfd, x >> 8, (bfd_byte *) data + octets + 0);
  bfd_put_8 (abfd, x >> 0, (bfd_byte *) data + octets + 1);
  return bfd_reloc_ok;
}

#define ELF_ARCH		bfd_arch_tlcs900
#define ELF_MACHINE_CODE	EM_TLCS900
#define ELF_MAXPAGESIZE		0x10000

#define TARGET_LITTLE_SYM		tlcs900_elf32_vec
#define TARGET_LITTLE_NAME		"elf32-tlcs900"

#define elf_backend_can_refcount		1
#define elf_backend_can_gc_sections		1
#define elf_backend_stack_align			1
#define elf_backend_rela_normal			1

#define elf_info_to_howto			tlcs900_info_to_howto_rela
#define elf_info_to_howto_rel			tlcs900_info_to_howto_rela

#define elf_backend_final_write_processing	tlcs900_elf_final_write_processing
#define elf_backend_object_p			tlcs900_elf_object_p
#define elf_backend_relocate_section		tlcs900_elf_relocate_section

#define bfd_elf32_bfd_reloc_type_lookup		tlcs900_reloc_type_lookup
#define bfd_elf32_bfd_reloc_name_lookup		tlcs900_reloc_name_lookup
#define bfd_elf32_bfd_is_local_label_name	tlcs900_is_local_label_name

#include "elf32-target.h"

#include <dwarf.h>
#include <libdwarf.h>
#include <imagehlp.h>

#define COFF_SYMBOL_SIZE 18

int get_section_info(void* obj, 
                     Dwarf_Half section_index,
                     Dwarf_Obj_Access_Section* return_section, 
                     int* error) 
{ LOADED_IMAGE *image = obj;
  IMAGE_SECTION_HEADER *section = image->Sections + section_index;

  return_section->addr = section->VirtualAddress;
  return_section->size = section->Misc.VirtualSize;

  if (section->Name[0] == '/')
  { return_section->name = 
         (char*)(image->MappedAddress + \
                 image->FileHeader->FileHeader.PointerToSymbolTable + \
                 image->FileHeader->FileHeader.NumberOfSymbols * COFF_SYMBOL_SIZE + \
                 atoi((char*)section->Name + 1));
  } else
  { return_section->name = (char*)section->Name;
  }

  return DW_DLV_OK;
}

Dwarf_Endianness get_byte_order(void* obj) 
{ return DW_OBJECT_LSB;
}

Dwarf_Small get_length_size(void* obj) 
{ return sizeof(void*);
}

Dwarf_Small get_pointer_size(void* obj) 
{ return sizeof(void*);
}

Dwarf_Unsigned get_section_count(void* obj) 
{ LOADED_IMAGE *image = obj;
  return image->NumberOfSections;
}

int load_section(void* obj,
                 Dwarf_Half section_index, 
                 Dwarf_Small** return_data, 
                 int* error) 
{ LOADED_IMAGE *image = obj;
  IMAGE_SECTION_HEADER *section = image->Sections + section_index;
 
  *return_data = image->MappedAddress + section->PointerToRawData;

  return DW_DLV_OK;
}

int relocate_a_section(void* obj,
                       Dwarf_Half section_index, 
                       Dwarf_Debug dbg, 
                       int* error) 
{ /* not required for basic stacktraces */
  return DW_DLV_ERROR;
}


int dwarf_function_symbol(Dwarf_Debug dbg, Dwarf_Addr address, Dwarf_Die die, char **symbol, int *symbol_offset)
{ Dwarf_Die next_die;
  Dwarf_Error error;
  Dwarf_Half tag;
  Dwarf_Bool has_lowpc, has_highpc;
  Dwarf_Addr lowpc, highpc;

  if (die == NULL)
    return DW_DLV_ERROR;

  if ( (dwarf_tag(die, &tag, &error) == DW_DLV_OK) &&
       (tag == DW_TAG_subprogram || tag == DW_TAG_inlined_subroutine) &&
       (dwarf_hasattr(die, DW_AT_low_pc, &has_lowpc, &error) == DW_DLV_OK) &&
       (dwarf_hasattr(die, DW_AT_high_pc, &has_highpc, &error) == DW_DLV_OK) &&
       (has_lowpc) && (has_highpc) &&
       (dwarf_lowpc(die, &lowpc, &error) == DW_DLV_OK) &&
       (dwarf_highpc(die, &highpc, &error) == DW_DLV_OK) &&
       (address >= lowpc) && (address <= highpc) )
  { char *diename;
    if (dwarf_diename(die, &diename, &error) == DW_DLV_OK)
    { strcpy(*symbol, diename);
      *symbol_offset = address-lowpc;
      dwarf_dealloc(dbg, diename, DW_DLA_STRING);
    }

    dwarf_dealloc(dbg, die, DW_DLA_DIE);
    return DW_DLV_OK;
  }

  if (dwarf_child(die, &next_die, &error) == DW_DLV_OK)
  { if (dwarf_function_symbol(dbg, address, next_die, symbol, symbol_offset) == DW_DLV_OK)
    { dwarf_dealloc(dbg, die, DW_DLA_DIE);         
      return DW_DLV_OK;
    }
  }

  if (dwarf_siblingof(dbg, die, &next_die, &error) == DW_DLV_OK)
  { if (dwarf_function_symbol(dbg, address, next_die, symbol, symbol_offset) == DW_DLV_OK)
    { dwarf_dealloc(dbg, die, DW_DLA_DIE);         
      return DW_DLV_OK;
    }
  }
 
  dwarf_dealloc(dbg, die, DW_DLA_DIE);
  return DW_DLV_NO_ENTRY;
}


static Dwarf_Obj_Access_Methods dwarf_access_methods =
  {
    get_section_info,
    get_byte_order,
    get_length_size,
    get_pointer_size,
    get_section_count,
    load_section,
    relocate_a_section
  };
static Dwarf_Obj_Access_Interface dwarf_access_interface = {0};


void dwarf_src_line(Dwarf_Debug dbg, Dwarf_Addr address, Dwarf_Die die, char **srcline)
{ Dwarf_Error error;
  Dwarf_Line *linebuf;
  Dwarf_Signed linecount;
  Dwarf_Line line = NULL;
  Dwarf_Addr line_addr = 0;
  int i;

  if (dwarf_srclines(die, &linebuf, &linecount, &error) == DW_DLV_OK)
  { for (i=0; i < linecount; i++)
    { Dwarf_Addr cur_line_addr;
      if (dwarf_lineaddr(linebuf[i], &cur_line_addr, &error) == DW_DLV_OK)
      { if ( (address >= cur_line_addr) && (cur_line_addr > line_addr) )
        { line = linebuf[i];
          line_addr = cur_line_addr;
        }
      }
    }

    if (line)
    { char *linesrc;
      Dwarf_Unsigned lineno;
      if (dwarf_linesrc(line, &linesrc, &error) == DW_DLV_OK)
      { dwarf_lineno(line, &lineno, &error);
        sprintf(*srcline, "%s:%d", linesrc, (int)lineno);
        dwarf_dealloc(dbg, linesrc, DW_DLA_STRING);
      }
    }

    dwarf_srclines_dealloc(dbg, linebuf, linecount);
  }
}


int dwarf_die_from_address(LOADED_IMAGE *image, Dwarf_Addr address, Dwarf_Debug *dbg, Dwarf_Die *die)
{ Dwarf_Error error = NULL;
  Dwarf_Arange *aranges = NULL;
  Dwarf_Signed aranges_count = 0;
  Dwarf_Arange arange = NULL;
  Dwarf_Off die_offset = 0;
  int i, ret = 0;

  dwarf_access_interface.object = image;
  dwarf_access_interface.methods = &dwarf_access_methods;
   
  if (dwarf_object_init(&dwarf_access_interface,
                        NULL,
                        NULL,
                        dbg, 
                        &error) != DW_DLV_OK)
    goto out;

  if (dwarf_get_aranges(*dbg,
                        &aranges,
                        &aranges_count,
                        &error) != DW_DLV_OK)
    goto out;

  if (dwarf_get_arange(aranges,
                       aranges_count,
                       address,
                       &arange,
                       &error) != DW_DLV_OK)
    goto out;

  if (dwarf_get_cu_die_offset(arange,
                              &die_offset,
                              &error) != DW_DLV_OK)
    goto out;

  if (dwarf_offdie(*dbg,
                   die_offset,
                   die,
                   &error) != DW_DLV_OK)
    goto out;

  ret = 1;

out:
  if (aranges)
  { for (i=0; i < aranges_count; i++)
    { dwarf_dealloc(*dbg, aranges[i], DW_DLA_ARANGE);
    }
    dwarf_dealloc(*dbg, aranges, DW_DLA_LIST);
  }
   
  return ret;
}


int dwarf_sym_from_addr(IMAGEHLP_MODULE *module, DWORD64 address, char** symbol)
{ Dwarf_Debug dbg = NULL;
  Dwarf_Error error = NULL;
  Dwarf_Die die = NULL;
  LOADED_IMAGE *image;
  DWORD64 mapped_address;
  int symbol_offset;
  int ret = 0;

  image = ImageLoad(module->ImageName, NULL);
  if ( !image ) return 0;
  mapped_address = image->FileHeader->OptionalHeader.ImageBase + \
                   (address - module->BaseOfImage);

  if ( !dwarf_die_from_address(image, mapped_address, &dbg, &die) )
    goto out;

  if (dwarf_function_symbol(dbg,
                            mapped_address,
                            die,
                            symbol,
                            &symbol_offset) != DW_DLV_OK)
    goto out;

  ret = 1;

out:
  if (dbg) dwarf_object_finish(dbg, &error);
  ImageUnload(image);

  return ret;
}


int dwarf_addr2line(IMAGEHLP_MODULE *module, DWORD64 address, char** srcline)
{ Dwarf_Debug dbg = NULL;
  Dwarf_Error error = NULL;
  Dwarf_Die die = NULL;
  LOADED_IMAGE *image;
  DWORD64 mapped_address;
  int ret = 0;

  image = ImageLoad(module->ImageName, NULL);
  if ( !image ) return 0;
  mapped_address = image->FileHeader->OptionalHeader.ImageBase + \
                   (address - module->BaseOfImage);

  if ( !dwarf_die_from_address(image, mapped_address, &dbg, &die) )
    goto out;

  dwarf_src_line(dbg, mapped_address, die, srcline);
  ret = 1;

out:
  if (dbg) dwarf_object_finish(dbg, &error);
  ImageUnload(image);

  return ret;
}

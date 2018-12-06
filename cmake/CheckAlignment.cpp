                      /*******************************************************
                       * Create ascii pattern to match in order to find
                       *         alignment at compile time
                       *******************************************************/

// Compile this program and match
// the produced executable against:
// INT64_ALIGNMENT=<code>
// VOIDP_ALIGNMENT=<code>
// DOUBLE_ALIGNMENT=<code>
//
// to get the alignment used by the (cross)compiler.
//
// <code> is the alignment as an ascii digit.

#include <stdint.h>

#if __cplusplus >= 201103L
#define ALIGNOF(type) (alignof(type) + 48) // Ascii '1' for 1, '4' for 4, '8' for 8
#else
#define ALIGNOF(type) (sizeof(type) + 48) // Safe fallback
#endif

int prevent_optimization(unsigned char*p, int size) {
   unsigned char *d;

   //Prevent optimizer from eliminating the constants in main()
   unsigned char dummy[size];
   d = dummy;
   for (int i = 0; i < size; ++i) {
     *d++ = *p++;
   }
   return dummy[size-1];
}


int main() {
   const int int64_pat_sz = 18;
   const int voidp_pat_sz = 18;
   const int double_pat_sz = 19;

   static const unsigned char int64_alignment[int64_pat_sz] = {
         'I', 'N','T','6','4','_','A','L','I','G','N','M','E','N','T','=',
         ALIGNOF(int64_t), 0x0
   };

   static const unsigned char voidp_alignment[voidp_pat_sz] = {
         'V', 'O','I','D','P','_','A','L','I','G','N','M','E','N','T','=',
         ALIGNOF(void*), 0x0
   };

   static const unsigned char double_alignment[double_pat_sz] = {
         'D', 'O','U','B','L','E','_','A','L','I','G','N','M','E','N','T','=',
         ALIGNOF(double), 0x0
   };

   //Not used, prevent optimization
   return prevent_optimization((unsigned char*)int64_alignment, int64_pat_sz) +
          prevent_optimization((unsigned char*)voidp_alignment, voidp_pat_sz) +
          prevent_optimization((unsigned char*)double_alignment, double_pat_sz);
}

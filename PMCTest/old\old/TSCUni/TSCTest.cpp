//                       TSCTest.cpp                    © 2010-12-25 Agner Fog
//
// Test program to measure how many clock cycles a little piece of code takes.
// Author: Agner Fog
// Created date:  2006-06-01
// Last modified: 2011-08-15
// URL: www.agner.org/optimize/testp.zip
//
// The program will execute the code to test NUMTESTS times and then print
// out a list of clock counts.
//
// The project consists only of this file
// 
// Instructions:
// Insert the code to test at the place marked "### Test code here ###"
// Compile and link for console mode under Windows, Linux or Mac OS X 
// on x86 platform, 32 or 64 bits.
//
// © 2005-2011 GNU General Public License www.gnu.org/licenses
//////////////////////////////////////////////////////////////////////////////

#include <stdio.h>

// ###################### DEFINE CONSTANTS ######################

// set number of times to run test
const int NUMTESTS = 20;

// any declarations that your test code needs may go here:
int xx = 0;

// ###################### END DEFINE CONSTANTS ######################

// define type int64_t for 64 bit integer
#if defined(__GNUC__) || (defined(_MSC_VER) && _MSC_VER >= 1600)
  // Compilers supporting C99 or C++0x have stdint.h defining integer types
  #include <stdint.h>
  #define INT64_SUPPORTED // Remove this if the compiler doesn't support 64-bit integers
#elif defined(_MSC_VER)
  // Older Microsoft compilers have their own definition
  typedef __int64 int64_t;
#else
  // This works with many compilers
  typedef long long int64_t;
#endif

// Define ReadTSC function
#ifdef USE_ALIB    // use alibXXX.lib
// for compilers with insufficient inline assembly support, use external
// library for function ReadTSC()
#include "alib.h"

#else
// for compilers that understand the inline assembly code you can define
// the function ReadTSC() here:

// Read time stamp counter
// The return value is the internal clock count
int64_t ReadTSC() {
   unsigned reslo, reshi;                   // store 64 bit result here
   
   #if defined(__GNUC__) && !defined(__INTEL_COMPILER)
   // Inline assembly in AT&T syntax
      __asm__ __volatile__  (               // serialize (save ebx)
      "xorl %%eax,%%eax \n cpuid \n"
       ::: "%eax", "%ebx", "%ecx", "%edx");
      __asm__ __volatile__  (               // read TSC, store edx:eax in res
      "rdtsc\n"
       : "=a" (reslo), "=d" (reshi) );
      __asm__ __volatile__  (               // serialize again
      "xorl %%eax,%%eax \n cpuid \n"
       ::: "%eax", "%ebx", "%ecx", "%edx");
   #else
   // Inline assembly in MASM syntax
      __asm {
         xor eax, eax
         cpuid                              // serialize
         rdtsc                              // read TSC
         mov dword ptr reslo, eax           // store low dword in res[0]
         mov dword ptr reshi, edx           // store high dword in res[1]
         xor eax, eax
         cpuid                              // serialize again
      };
   #endif   // __GNUC__
   
   return ((int64_t)reshi << 32) | reslo;   // return result
}

#endif   // USE_ALIB


void ClockTest (int64_t clocks[]) {
   int64_t before, overhead;
   int i;

   // dummy test loop without test code to measure overhead
   for (i = 0; i < NUMTESTS; i++) {
      before = ReadTSC();                   // clock count before
      // no test code here, measure overhead only
      clocks[i] = ReadTSC() - before;       // clock count after
   }

   // find minimum overhead
   overhead = clocks[0];
   for (i = 0; i < NUMTESTS; i++) {
      if (clocks[i] < overhead) overhead = clocks[i];
   }

   // loop to measure clock cycles of test code
   for (i = 0; i < NUMTESTS; i++) {
      before = ReadTSC();                   // clock count before

// ###################### Test code here ######################


      xx *= 100;  // Replace this with your C++ test code

/* Inline assembly example:
   Use this code if you need inline assembly with the Gnu compiler:

   __asm__ __volatile__ (
   " .intel_syntax noprefix \n"  // Switch to Intel syntax
   " .rept 100              \n"  // Repeat if needed

   " shr eax,1              \n"  // Instruction to test

   " .endr                  \n"  // End of repeat
   " .att_syntax prefix     \n"  // Switch back to AT&T syntax
   );
*/
      
// ###################### Test code end  ######################

      clocks[i] = ReadTSC() - before;       // clock count after
   }

   // subtract overhead from clock counts
   for (i = 0; i < NUMTESTS; i++) {
      clocks[i] -= overhead;
   }
}


int main() {
   // list of clock counts
   int64_t clocklist[NUMTESTS];
   int i;

   // run tests
   ClockTest (clocklist);

   // print results
   printf ("\n  test     clock cycles");
   for (i = 0; i < NUMTESTS; i++) {
      printf ("\n%6i  %14G", i+1, double(clocklist[i]));
   }

   printf ("\n");
   return 0;
}

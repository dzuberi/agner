/*              TSCTestA.cpp                             © 2005-07-16 Agner Fog


Test program to measure how many clock cycles a little piece of code takes.

The program will execute the code to test NUMTESTS times and then print
out a list of clock counts.

The project consists of 
TSCTestA.cpp and TSCTestB32.asm for 32 bit mode, or
TSCTestA.cpp and TSCTestB64.asm for 64 bit mode.

Instructions:

Insert the code to test at the place marked "### Test code here ###"
in TSCTestB32.asm or TSCTestB64.asm

Compile and link for Windows, console mode, 32 or 64 bits.

© 2005 GNU General Public License www.gnu.org/copyleft/gpl.html
*/

#include <stdio.h>
#include <process.h>
#include <windows.h>


extern "C" int ClockTest (__int64 clocks[]);
extern "C" int NumberOfTests;

int main() {

   // define maximum value for number of tests.
   // must be bigger than NUMTESTS, defined in .asm file:
	const int MAXLIST = 100;

   // list of clock counts
	__int64 clocklist[MAXLIST];
	int i, n;

   // check array size
   if (NumberOfTests > MAXLIST) {
      printf("\nError: NumberOfTests > MAXLIST\n");
      exit(1);
   }

   // set high priority to avoid interrupts during test
	HANDLE hProcess = GetCurrentProcess();
   SetPriorityClass(hProcess, REALTIME_PRIORITY_CLASS);

   // run tests
	n = ClockTest (clocklist);

   // set priority back normal
   SetPriorityClass(hProcess, NORMAL_PRIORITY_CLASS);

   // print results
	printf ("\n  test     clock cycles");
	for (i = 0; i < n; i++) {
		printf ("\n%6i  %10I64i", i+1, clocklist[i]);
	}


	printf ("\n");
	return 0;
}

int _tmain(){
	return main();
}

#                    lg32.sh                          2005-07-17 Agner Fog
#
# shell script to compile TSC test program on 32 bit Linux 
# using Gnu compiler

if [ $HOSTTYPE = x86_64 ]; then
   g++ -m32 TSCTest.cpp
else
   g++ TSCTest.cpp
fi
if [ $? = 0 ]; then
./a.out
fi


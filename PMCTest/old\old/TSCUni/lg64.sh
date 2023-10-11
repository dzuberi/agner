#                    lg64.sh                          2005-07-17 Agner Fog
#
# shell script to compile TSC test program on 64 bit Linux 
# using Gnu compiler

g++ -m64 TSCTest.cpp
if [ $? = 0 ]; then
./a.out
fi


#                    li64.sh                          2005-07-17 Agner Fog
#
# shell script to compile TSC test program on 64 bit Linux 
# using Intel compiler

/opt/intel/cce/9.0/bin/icpc -use-msasm -i-static TSCTest.cpp
if [ $? = 0 ]; then
./a.out
fi


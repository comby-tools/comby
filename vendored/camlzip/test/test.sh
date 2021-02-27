#!/bin/bash

../_build/default/test/minizip_on_linux.exe t master.zip > got.txt
diff got.txt expect.txt
echo $?

rm got.txt

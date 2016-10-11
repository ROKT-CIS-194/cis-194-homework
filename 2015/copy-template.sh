#!/bin/bash

ME=$1
if [[ "X$ME" == "X" ]]; then
   echo "Supply XXX from your src/CIS194/XXX directory name"
   exit
fi

echo "Copying src/CIS194/Template to src/CIS194/$ME"

mkdir -p src/CIS194/$ME
cp -nr src/CIS194/Template/* src/CIS194/$ME/
sed -i "s/Template/$ME/" $(find src/CIS194/$ME/ -type f)
git add src/CIS194/$ME
# sed -i "s/Template/$ME/" src/Main.hs

#!/bin/bash

# A list of wxWidgets versions that can be handled by this version of wxHaskell, preferred first
COMPATIBLE_VERSIONS=("3.2" "3.1")

for V in ${COMPATIBLE_VERSIONS[@]}; do
	if [[ $(wx-config --version=$V --version-full) = $V* ]]; then
		VERSION=$V
		break
	fi
done

if [[ -n $VERSION ]]; then
	#TODO this is only correct if COMPATIBLE_VERSIONS entries are all 3 chars long with a dot in the middle
	VERSION_FORMATTED=$(echo $VERSION | sed 's/\.//g')00
	echo "#define wxVERSION_NUMBER $VERSION_FORMATTED" > include/wxc/wxc_def.h
else
	echo "This version of wxc requires one of the following wxWidgets versions to be available:"
	for value in ${COMPATIBLE_VERSIONS[@]}; do
		echo "  $value"
	done
	exit 1
fi

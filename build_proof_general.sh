#!/bin/bash

[ -d prfgnrl/ ] || git submodule add https://github.com/ProofGeneral/PG prfgnrl
(cd prfgnrl && make clean && make)

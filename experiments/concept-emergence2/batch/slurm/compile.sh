#!/bin/bash

module purge
module load SBCL/2.4.1-GCCcore-12.3.0
sbcl --noinform --eval "(ql:quickload :cle)" --quit
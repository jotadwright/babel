#!/bin/bash

module purge
module load SBCL/2.4.1-GCCcore-12.3.0
module load binutils/2.40-GCCcore-12.3.0 # required for distributions package
sbcl --noinform --eval "(ql:quickload :cle)" --quit
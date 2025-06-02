#!/bin/bash

module purge
module load SBCL/2.5.3-GCCcore-13.3.0
module load binutils/2.42-GCCcore-13.3.0 # required for distributions package
sbcl --noinform --eval "(ql:quickload :cle)" --quit
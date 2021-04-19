#!/bin/bash

# SBATCH --job-name=TestRunCheck
# SBATCH —partition=day-long-cpu

## This puts all output files in a separate directory.
## SBATCH --output=Out/TestRunCheck.%A_%a.out
## SBATCH —error=Err/TestRunCheck.%A_%a.err

## Submitting 100 instances of srun commands listed below
## SBATCH —array=0-100

## For notification purposes. Use your Emory email address only!
#SBATCH --mail-user=carol.liu@emory.edu
#SBATCH --mail-type=END,FAIL

module purge

module load R

Rscript /home/cliu369/safegraph/agg_statesplit_clus.R
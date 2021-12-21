#!/bin/bash

#SBATCH -N 15
#SBATCH --ntasks-per-node=1
#SBATCH -t 60:00:00       
#SBATCH -p normal_q              
#SBATCH -A bpda

module purge
module load intel/17.0
module load mkl
module load fftw/3.3.5
module load gsl/2.4
module load R/3.4.1

#set number of cores used by each r process
export MKL_NUM_THREADS=2

#number of r processes to run
ncopies=30

echo "$( date ): Starting simulation"

for i in $( seq 1 $ncopies ); do 
  R CMD BATCH "--args seed=$i" simulation.R SIM_${i}.Rout &
done
wait

echo "$( date ): Finished simulation"




echo "$( date ): Starting collecting"
 R CMD BATCH simulation_collect.R simulation_collect_R.out
echo "$( date ): Finished collecting"

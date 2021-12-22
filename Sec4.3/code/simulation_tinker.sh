#!/bin/sh

#SBATCH --time=144:00:00
#SBATCH --ntasks-per-node=30
#SBATCH -A bdpa
#SBATCH -p normal_q
#SBATCH --exclusive
#SBATCH -N 1
#SBATCH --mail-user=zhengzhi@vt.edu
#SBATCH --mail-type=FAIL,END
##SBATCH --requeue

module reset
module load R
#set number of cores used by each r process
export MKL_NUM_THREADS=4

#number of r processes to run
ncopies=30

echo "$( date ): Starting simulation"

for i in $( seq 1 $ncopies ); do 
  R CMD BATCH "--args seed=$i" simulation.R SIM_${i}.Rout &
done
wait

echo "$( date ): Finished simulation"




echo "$( date ): Starting collecting"
 R CMD BATCH simulation_collect.R simulation_collect.Rout
echo "$( date ): Finished collecting"

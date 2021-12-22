#!/bin/sh

#SBATCH --time=144:00:00
##SBATCH --ntasks-per-node=100
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
export MKL_NUM_THREADS=2

#number of r processes to run
ncopies=100
k=10
m=7
n=5

echo "$( date ): Starting norm"

for i in $( seq 1 $ncopies ); do 
  R CMD BATCH "--args seed=$i K=$k m=$m n=$n" norm.R norm_${m}_${k}_${i}.Rout &
done
wait

echo "$( date ): Finished norm"

echo "$( date ): Starting collection"
 R CMD BATCH norm_collect.R norm_collect.Rout
echo "$( date ): Finished collection"


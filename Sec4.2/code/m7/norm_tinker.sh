#!/bin/sh

#SBATCH --time=144:00:00
##SBATCH --ntasks-per-node=100
#SBATCH -A bdpa
#SBATCH -p normal_q
#SBATCH --exclusive
#SBATCH -N 5
#SBATCH --mail-user=zhengzhi@vt.edu
#SBATCH --mail-type=FAIL,END
##SBATCH --requeue

module reset
module load R
module load parallel

scontrol show hostname $SLURM_NODELIST > node.list

#number of r processes to run
ncopies=500
k=10
m=7
n=5

nparallel=160
echo "$( date ): Starting norm"

seq 1 $ncopies | parallel -j $nparallel --workdir $PWD --joblog process.log --sshloginfile $(find node.list) "module reset
module load R
module load parallel
R CMD BATCH \"--args seed={} K=$k m=$m n=$n \" norm.R norm_${m}_${k}_{}.Rout"


echo "$( date ): Finished norm"

echo "$( date ): Starting collection"
 R CMD BATCH norm_collect.R norm_collect.Rout
echo "$( date ): Finished collection"


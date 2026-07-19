#!/bin/bash -
#SBATCH --mail-type BEGIN,END,FAIL
#SBATCH --mail-user jonas.bruno@unil.ch
##SBATCH --chdir /work/FAC/...
#SBATCH --job-name job_r
#SBATCH --output job_r_%a.out
#SBATCH --partition cpu
#SBATCH --nodes 1
#SBATCH --ntasks 1
#SBATCH --cpus-per-task 1
#SBATCH --mem 16G
#SBATCH --time 01:00:00
#SBATCH --export NONE
#SBATCH --array=1-128

module load r-light


dates=("2000 Q1" "2000 Q2")

for date in "${dates[@]}"; do
  echo "Executing: ${date}"
  Rscript main_cluster.R -d "${date}" -m okun -f "cluster_okun_trial_run"
done

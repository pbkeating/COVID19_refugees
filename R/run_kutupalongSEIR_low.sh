#!/bin/bash -l

#SBATCH --job-name=kutupalong_camp_low
#SBATCH --time=48:00:00
#SBATCH --nodes=1 # 1 tasks
#SBATCH --ntasks=1 # 1 tasks
#SBATCH --cpus-per-task=10 # 10 core per task
#SBATCH --mem-per-cpu=4000MB
#SBATCH --mail-type=end
#SBATCH --mail-user=shauntruelove@jhu.edu
#SBATCH --partition=shared
#SBATCH --account=struelo1
#SBATCH --error=outputJob_low.err
#SBATCH --output=outputJob_low.out


module purge
module load MARCC
module load gcc/5.5.0
module load R/3.5.1
module list 


current_dir=${PWD##*/}
echo "Current dir: $current_dir"
echo
pwd
echo

#### Make new directory for the log files
dirname=Run_Logs
echo $dirname
mkdir -p $dirname



#### execute code and write output to an .Rout file.
R -n 10 CMD BATCH --args ' R0_val="low" n.Rs=20 n.sims=100 cores=10 ' ./R/RunSEIR_server.R ./"${dirname}"/kutupalong_low.Rout 

echo "Kutupalong Low"
#!/bin/bash -l

#SBATCH --job-name=kutupalong_camp
#SBATCH --time=24:00:00
#SBATCH -N 3 # 3 nodes
#SBATCH -n 3 # 3 tasks
#SBATCH -c 20 # 20 core per task
#SBATCH --mem-per-cpu=12G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=shauntruelove@jhu.edu

#### set home directory to the scratch drive
#cd projects/nCoV/nCoV_refugees

#### Make new directory for the log files
dirname=Run_Logs
echo $dirname
mkdir -p $dirname

#### execute code and write output to an .Rout file.
R -N 1 -n 20 CMD BATCH --quiet --no-save --no-restore '--args R0_val="low" n.Rs=20 n.sims=100 cores=20 ' ./R/RunSEIR_server.R ./"${dirname}"/kutupalong_low.Rout   &
R -N 1 -n 20 CMD BATCH --quiet --no-save --no-restore '--args R0_val="mid" n.Rs=20 n.sims=100 cores=20 ' ./R/RunSEIR_server.R ./"${dirname}"/kutupalong_mid.Rout   &
R -N 1 -n 20 CMD BATCH --quiet --no-save --no-restore '--args R0_val="high" n.Rs=20 n.sims=100 cores=20 ' ./R/RunSEIR_server.R ./"${dirname}"/kutupalong_high.Rout &  
wait
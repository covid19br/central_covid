## É preciso ter um clone do repo SEADE em ../../../clone_repo_seade_SP
## Na sua cópia local do metarepo central covid digite: git clone git@github.com:seade-R/dados-covid-sp.git clone_repo_seade_SP/

## Cria arquivo csv com hash e data de cada commit
system("cd ../../../clone_repo_seade_SP; git pull, git log --pretty=format:'%h,%ad' --date=short > ../scripts_R_genericos/plano_SP/nowcasting_seade/hashes_seade.csv") ## note que há mais de um commit por data, usar duplicates para pegar a mais recente de cada dia?
hashes <- read.table("hashes_seade.csv", sep=",", header = FALSE)
names(hashes) <- c("hash", "data")
hashes$data <- as.Date(hashes$data)
## Retem o ultimo commit de cada dia
hashes.last <- hashes[!duplicated(hashes$data),]

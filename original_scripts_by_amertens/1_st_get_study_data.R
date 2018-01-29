
#devtools::install_github("HBGDki/ghap")

library(ghap)


set_git_base_path("U:/git")
get_git_base_path()

#studies <- get_study_list()

astudies <- get_study_list_anthro()
astudies<-as.data.frame(astudies)

print(astudies[,-c(1,3,4)])

table(astudies$intervention_type)
table(astudies$notes)




head(astudies)
tail(astudies[, c("study_id", "short_id", "short_description")], 3)


#Write list of studies
write.table(astudies, "U:/astudies.txt", sep="\t")




#Get all studies' data
#(Note: takes a long time: run overnight)
astudies <- get_study_list_anthro()
for (id in astudies$short_id){
  tmp <- use_study(id)
}

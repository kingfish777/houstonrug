no.yes <- c("No", "Yes")
smoking <- gl(2,1,8,no.yes)
obesity <- gl(2,2,8,no.yes)
snoring <- gl(2,4,8,no.yes)
n.tot <- c(60,17,8,2,187,85,51,23)
n.hyp <- c(5,2,1,0,35,13,15,8)
data.frame(smoking,obesity,snoring,n.tot,n.hyp)
# as you can see, 
df <- data.frame(smoking,obesity,snoring,n.tot,n.hyp)
df
write.csv(df, file="hypdf.txt") #, append=FALSE, quote=TRUE) # saving 
system("cat hypdf.txt”) # see contents — this is what the data should look like
hyp.tbl <- cbind(n.hyp,n.tot-n.hyp)
print(hyp.tbl)
glm.hyp <- glm(hyp.tbl~smoking+obesity+snoring,family=binomial("logit"))
summary(glm.hyp)


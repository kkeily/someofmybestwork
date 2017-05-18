#Kylene Keily and Kaitlyn Marines
#Project 1
rm(list = ls())

load("ncaaf_2010.rdata")

#Create a matrix 120x120 w/ NAs
A<-matrix(data=rep(NA,120*120),nrow=120,ncol=120)
View(A)

#Diagonals
gamesplayed=c(df$wins+df$losses)

for(i in 1:120){
   A[i,i]=(2+gamesplayed[i]) 
}

#Non-Diagonal values
for(i in 1:120){
  for (j in 1:120){
    if (i==j)
    {next}
      A[i,j]=(-1*length(df$opponents[[j]][df$opponents[[j]]==i])) #take length of row j and subet row j where i appears in j
              }
}

#Check all rows sum to value of 2
for(i in 1:120){
  sumA=sum(A[i,])
  print(sumA)
}

#ith vector
ith=c(1+(df$wins-df$losses)/2)
View(ith)

#Solve system
x=solve(A,ith)

#SOLUTIONS: Create a new dataframe with solutions values for teams
Solutions=data.frame(df$teams,x)
View(Solutions)

#Reorder lowest rank to highest rank
Ranking=Solutions[order(x),]
View(Ranking)
LowestRanked=head(Ranking)
HighestRanked=tail(Ranking)
View(HighestRanked) 
View(LowestRanked)

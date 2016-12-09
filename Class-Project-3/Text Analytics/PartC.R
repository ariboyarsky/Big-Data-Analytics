library(tm)
data(acq)

# search for a needle (word) in a hay stack (corpora)
searchCorpora <- function(hay, needle){
  for(a in hay){
    
  }
}

for(a in seq(acq)){
  if(length(grep("any future", acq[[a]]$content)) > 0){
    print(a)
    x <- grep("any future", acq[[a]]$content)
    x
  }
}

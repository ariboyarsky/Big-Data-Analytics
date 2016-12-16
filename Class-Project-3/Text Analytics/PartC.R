install.packages("quanteda")
library(quanteda)

doc_search <- function(data, search){
  mycorpus <- corpus(data)
  result <- kwic(mycorpus, search, window = 3)
  result[[1]] <- gsub("reut-000", "", result[[1]])  
  result[[1]] <- gsub(".xml", "", result[[1]]) 
  doc_number <- as.numeric(result[[1]]) # document number
  word_index <- result[[2]] # word index in document
  word <- result[[4]][[1]] # word searching for
  line_index <- list()
  docs <- unique(doc_number)
  out<-capture.output(cat(mycorpus[docs]))
  line <- grep(search, out, ignore.case = T)
  line_index <- list(line_index, as.numeric(line))
  df = data.frame(doc_number,word_index, line)
  word_used = cat("Searching documents for:", word, "\n")
  return(df)
}

doc_search(acq, "because")
doc_search(acq, "Mattress")
doc_search(acq, "appraisals")
 
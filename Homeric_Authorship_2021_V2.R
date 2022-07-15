
## Another distance 

## One or Many Homers? Using Quantitative Authorship Analysis to Study the Homeric Question ##

# The code employed here is broken down into several parts:
  # 1. A working directory where the text files used were stored.
  # 2. A list of R packages used in the analysis.
  # 3. Functions used for reading in files.
  # 4. A connection to a text file listing all types in the text files of the Iliad and Odyssey 
    # that begin with a capital letter and which probably constitute forms of personal or place names.
  # 5. Functions used for preparing initial raw feature sets from individual files.
  # 6. Functions used for gathering initial raw feature sets from directories containing a group of files.
  # 7. Removing personal and place names from feature sets with word n-grams
      ## = Executable one-line sequences at the command line used to prepare raw feature sets for analysis.
  # 8. Removing duplicate columns from feature sets of word bigrams.
      ## = Executable one-line sequences at the command line used to prepare raw feature sets for analysis.
  # 9. Function for creating co-association matrices as per Layton 2011.
  # 10. Function for calculating cosine distance.
  # 11. Final workflow for generating dendrograms using word n-grams (bigrams specifically).
      ## Requires execution of 7. and 8.
  # 12. Functions for the Source-Code Author Profiling (SCAP) method. To be used only with character n-grams.
  # 13. Generating a dataframe of document distances using SCAP
  # 14. Final workflow for generating dendrograms using character n-grams and SCAP (trigrams specifically).
  # END: Sample workflow. Users interested in recreating the work of the analyses in the paper can set their own working directory,
  # add the functions in 1.-XXX to their global environment, then execute appropriately modified versions of the code.



## 1. Working Directory ##
    ## Be certain to set your working directory to a directory containing all of the documents to be processed.
setwd("~/Documents/Dropbox/Homeric Authorship/Greek Texts/TLG_Homerus/Homer_All")

## 2. R Packages ##
library(tidyverse)
library(stringi)
library(plyr)
library(dplyr)
library(cluster)
library(ngram)


## 3. Reading in files##

  # The function "prepare.document" reads in a UTF-8 encoded file, performs Unicode normalization (ensuring that sequences with
  # combining diacritics become glyphs), and creates a vector in which each element in the vector corresponds to one word broken
  # on whitespace.
prepare.document <- function(document, remove.upper=FALSE){
  #library(stringi)
  document.raw <-stri_trans_nfc(scan(file=document, what="char", sep="\n", fileEncoding = "UTF-8")) # read in file, perform unicode normalization
  document.raw <- document.raw[which(document.raw != " ")]
  
  document.list<-strsplit(document.raw, "\\W+") # break the words on whitespace
  document.vector<-unlist(document.list) # convert the resulting list to a vector
  document.vector<-document.vector[which(document.vector!="")] # eliminate any items in the vector that are empty

  return(document.vector)
}


## 4. Creating a list of types beginning with a capital letter. Link to manually curated list of personal and place names ##


get.upperase.items <- function(directory = getwd(), outfile_name = "Homerus_uppercase_items.txt"){ 
  uppercase_items <- c()
  for(file in 1:length(list.files())){
    uppercase_temp <- prepare.document(list.files(path = directory)[file])
    uppercase_items <- append(uppercase_items, uppercase_temp)
  }
  uppercase_table <- table(uppercase_items)
  uppercase_table <- sort(uppercase_table, decreasing = TRUE)
  uppercase_table_frame <- as.data.frame(uppercase_table)
  write.table(uppercase_table_frame, outfile_name, quote = FALSE, sep = "\t", row.names = FALSE)
}

  # Outside of R, manually: the resulting list was manually curated, removing all forms that clearly do not represent 
    # personal or place names. Renamed file: Homerus_uppercase_items_no_freq.txt

  # Note that the items of the list are made lowercase, since the functions in 5. covert all words to lowercase.
uppercase_items <- tolower(stri_trans_nfc(scan("~/Documents/Dropbox/Homeric Authorship/List junks/Homerus_uppercase_items_no_freq.txt", 
                                               what = "char", sep= "\n", fileEncoding = "UTF-8")))


## 5. Functions for preparing initial raw feature sets.

  # These functions are designed to provide the option of 1) removing features containing (part of) a personal or place name
  # and 2) collapsing all vowel characters with diaritics with the corresponding characters without diacritics.

  # Both functions here produce table objects as their output, and therefore generally need to be wrapped in

# Collect word n-grams
get.word.ngrams <- function(document, gram.size = 2, remove.upper=FALSE, remove.accents=FALSE){
  document <- prepare.document(document, remove.upper = remove.upper)
  
  document.lc <- tolower(document) # make lower case
  
  document.concat <- concatenate(document.lc) # concatenate vector of words
  document.word.bigrams.table <- sort(table(ngram_asweka(document.concat, min=gram.size, max=gram.size, sep=" ")), decreasing=TRUE) # collect n-gram frequencies
  
  # remove any n-grams that contain any of the original uppercase words
  if(remove.upper == TRUE){
    uppercase_items_to_remove <- c()
    for(i in 1:length(uppercase_items)){
      temp_hits <- grep(uppercase_items[i], names(document.word.bigrams.table))
      uppercase_items_to_remove <- append(uppercase_items_to_remove, temp_hits)
    }
    document.word.bigrams.table <- document.word.bigrams.table[-c(uppercase_items_to_remove)]
  }
  
  if(remove.accents == TRUE){
    names(document.word.bigrams.table) <- gsub("ἀ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἁ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἄ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἅ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἂ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἃ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἆ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἇ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ά", "α", names(document.word.bigrams.table))# Alpha with oxia, not tonos!
    names(document.word.bigrams.table) <- gsub("ὰ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ᾶ", "α", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἐ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἑ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἔ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἕ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἒ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἓ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἕ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("έ", "ε", names(document.word.bigrams.table)) # Epsilon with oxia, not tonos!
    names(document.word.bigrams.table) <- gsub("ὲ", "ε", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἠ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἡ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἤ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἥ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἢ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἣ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἦ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἧ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ή", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὴ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ῆ", "η", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἰ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἱ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἴ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἵ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἲ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἳ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἶ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ἷ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ί", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὶ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ῖ", "ι", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὀ", "ο", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὁ", "ο", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὄ", "ο", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὅ", "ο", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὂ", "ο", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὃ", "ο", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ό", "ο", names(document.word.bigrams.table))#Omicron with acute, not tonos!
    names(document.word.bigrams.table) <- gsub("ὸ", "ο", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὐ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὑ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὔ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὕ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὒ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὓ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὖ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὗ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ύ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὺ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ῦ", "υ", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὠ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὡ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὤ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὥ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὢ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὣ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὦ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὧ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ώ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ὼ", "ω", names(document.word.bigrams.table))
    names(document.word.bigrams.table) <- gsub("ῶ", "ω", names(document.word.bigrams.table))
    
    # Merge and eliminate resulting duplicates
    duplicated_names <- names(document.word.bigrams.table[duplicated(names(document.word.bigrams.table)) == TRUE])
    duplicates.to.remove <- c()
    for(i in 1:length(duplicated_names)){
      temp.duplicates <- which(names(document.word.bigrams.table) == duplicated_names[i])
      document.word.bigrams.table[temp.duplicates[1]] <- sum(document.word.bigrams.table[c(temp.duplicates)])
      duplicates.to.remove <- append(duplicates.to.remove, temp.duplicates[2:length(temp.duplicates)])
    }
    document.word.bigrams.table <- document.word.bigrams.table[-c(duplicates.to.remove)]
    document.word.bigrams.table <- sort(document.word.bigrams.table, decreasing = TRUE)
  }

  return(document.word.bigrams.table)
}


## # Collect character n-grams 
get.character.ngrams <- function(document, gram.size=3, remove.upper=FALSE, remove.accents=FALSE){
  library(stringi)
  document.raw <-stri_trans_nfc(scan(file=document, what="char", sep="\n", fileEncoding = "UTF-8")) # read in file
  document.raw <- document.raw[which(document.raw != " ")]
  
  document.list<-strsplit(document.raw, "\\W+") # break the words on whitespace
  document.vector<-unlist(document.list) # convert the resulting list to a vector
  document.vector<-document.vector[which(document.vector!="")] # eliminate any items in the vector that are empty
  
  ## Remove any word containing an upper-case letter, to exclude topical similarities
  ## Or replace any word containing an upper-case letter with an "x"
  if(remove.upper == TRUE){ # Users should make sure that the object uppercase_items is available in their global environment
   #uppercase_items <- tolower(stri_trans_nfc(scan("~/Documents/Dropbox/Homeric Authorship/List junks/Homerus_uppercase_items_no_freq.txt", what = "char", sep= "\n", fileEncoding = "UTF-8")))
    document.vector <- tolower(document.vector)
    for(i in 1:length(uppercase_items)){
      if(uppercase_items[i] %in% document.vector){
        document.vector <- gsub(uppercase_items[i], "x", document.vector, perl = TRUE)
      }
    }
  }

  
  document.lc <- tolower(document.vector) # make lower case

  if(remove.accents == TRUE){
    document.lc <- gsub("ἀ", "α", document.lc)
    document.lc <- gsub("ἁ", "α", document.lc)
    document.lc <- gsub("ἄ", "α", document.lc)
    document.lc <- gsub("ἅ", "α", document.lc)
    document.lc <- gsub("ἂ", "α", document.lc)
    document.lc <- gsub("ἃ", "α", document.lc)
    document.lc <- gsub("ἆ", "α", document.lc)
    document.lc <- gsub("ἇ", "α", document.lc)
    document.lc <- gsub("ά", "α", document.lc)# Alpha with oxia, not tonos!
    document.lc <- gsub("ὰ", "α", document.lc)
    document.lc <- gsub("ᾶ", "α", document.lc)
    document.lc <- gsub("ἐ", "ε", document.lc)
    document.lc <- gsub("ἑ", "ε", document.lc)
    document.lc <- gsub("ἔ", "ε", document.lc)
    document.lc <- gsub("ἕ", "ε", document.lc)
    document.lc <- gsub("ἒ", "ε", document.lc)
    document.lc <- gsub("ἓ", "ε", document.lc)
    document.lc <- gsub("ἕ", "ε", document.lc)
    document.lc <- gsub("έ", "ε", document.lc) # Epsilon with oxia, not tonos!
    document.lc <- gsub("ὲ", "ε", document.lc)
    document.lc <- gsub("ἠ", "η", document.lc)
    document.lc <- gsub("ἡ", "η", document.lc)
    document.lc <- gsub("ἤ", "η", document.lc)
    document.lc <- gsub("ἥ", "η", document.lc)
    document.lc <- gsub("ἢ", "η", document.lc)
    document.lc <- gsub("ἣ", "η", document.lc)
    document.lc <- gsub("ἦ", "η", document.lc)
    document.lc <- gsub("ἧ", "η", document.lc)
    document.lc <- gsub("ή", "η", document.lc)
    document.lc <- gsub("ὴ", "η", document.lc)
    document.lc <- gsub("ῆ", "η", document.lc)
    document.lc <- gsub("ἰ", "ι", document.lc)
    document.lc <- gsub("ἱ", "ι", document.lc)
    document.lc <- gsub("ἴ", "ι", document.lc)
    document.lc <- gsub("ἵ", "ι", document.lc)
    document.lc <- gsub("ἲ", "ι", document.lc)
    document.lc <- gsub("ἳ", "ι", document.lc)
    document.lc <- gsub("ἶ", "ι", document.lc)
    document.lc <- gsub("ἷ", "ι", document.lc)
    document.lc <- gsub("ί", "ι", document.lc)
    document.lc <- gsub("ὶ", "ι", document.lc)
    document.lc <- gsub("ῖ", "ι", document.lc)
    document.lc <- gsub("ΐ", "ι", document.lc)
    document.lc <- gsub("ὀ", "ο", document.lc)
    document.lc <- gsub("ὁ", "ο", document.lc)
    document.lc <- gsub("ὄ", "ο", document.lc)
    document.lc <- gsub("ὅ", "ο", document.lc)
    document.lc <- gsub("ὂ", "ο", document.lc)
    document.lc <- gsub("ὃ", "ο", document.lc)
    document.lc <- gsub("ό", "ο", document.lc)#Omicron with acute, not tonos!
    document.lc <- gsub("ὸ", "ο", document.lc)
    document.lc <- gsub("ὐ", "υ", document.lc)
    document.lc <- gsub("ὑ", "υ", document.lc)
    document.lc <- gsub("ὔ", "υ", document.lc)
    document.lc <- gsub("ὕ", "υ", document.lc)
    document.lc <- gsub("ὒ", "υ", document.lc)
    document.lc <- gsub("ὓ", "υ", document.lc)
    document.lc <- gsub("ὖ", "υ", document.lc)
    document.lc <- gsub("ὗ", "υ", document.lc)
    document.lc <- gsub("ύ", "υ", document.lc)
    document.lc <- gsub("ὺ", "υ", document.lc)
    document.lc <- gsub("ῦ", "υ", document.lc)
    document.lc <- gsub("ΰ","υ", document.lc)
    document.lc <- gsub("ὠ", "ω", document.lc)
    document.lc <- gsub("ὡ", "ω", document.lc)
    document.lc <- gsub("ὤ", "ω", document.lc)
    document.lc <- gsub("ὥ", "ω", document.lc)
    document.lc <- gsub("ὢ", "ω", document.lc)
    document.lc <- gsub("ὣ", "ω", document.lc)
    document.lc <- gsub("ὦ", "ω", document.lc)
    document.lc <- gsub("ὧ", "ω", document.lc)
    document.lc <- gsub("ώ", "ω", document.lc)
    document.lc <- gsub("ὼ", "ω", document.lc)
    document.lc <- gsub("ῶ", "ω", document.lc)
  }
  document.concat <- concatenate(document.lc)
  document.char.split <- splitter(document.concat, split.char=TRUE, split.space=FALSE)
  document.char.ngrams.table <- sort(table(ngram_asweka(document.char.split, min=gram.size, max=gram.size)), decreasing=TRUE) ## Make a table of character n-grams, sorted from greatest to least
  if(remove.upper == TRUE){
    ngrams.to.remove <- grep("x", names(document.char.ngrams.table))
    document.char.ngrams.table <- document.char.ngrams.table[-c(ngrams.to.remove)]
  }
  return(document.char.ngrams.table)
}

## 6.  Functions used for gathering initial raw feature sets from directories containing a group of files.

  # Remember: this function assumes that the user has already set the working directory in the global environment.
  # With word bigrams, it is in practice considerably faster to remove the items with uppercase after collecting the frequencies
  # of the entire set of documents. See below at 7. I do not recommend using remove.upper = TRUE and remove.accents = TRUE when the gram type is "word".
  # It is highly recommended to normalize frequencies based on the relative size of the documents -- otherwise, the feature set will be biased towards longer documents.
make.ngrams.data.frame <- function(gram.size = 2, gram.type = c("word", "character"), remove.upper = FALSE, remove.accents = FALSE, normalize.freq = TRUE){
  
  if(gram.type == "word"){
    for(file in 1:length(list.files())){
      if(file == 1){
        first.frame <- as.data.frame(get.word.ngrams(list.files()[1], gram.size = gram.size, remove.upper = remove.upper, remove.accents = remove.accents))
        colnames(first.frame) <- c("Var1", list.files()[1])
      }
      else if(file == 2){
        second.frame <- as.data.frame(get.word.ngrams(list.files()[2], gram.size = gram.size, remove.upper = remove.upper, remove.accents = remove.accents))
        colnames(second.frame) <- c("Var1", list.files()[2])
        ngrams.big.frame <- join(first.frame, second.frame, by="Var1", type="full") # full or inner
        ngrams.big.frame[is.na(ngrams.big.frame)] <- 0
      }
      else{
        current.frame <- as.data.frame(get.word.ngrams(list.files()[file], gram.size=2, remove.upper = remove.upper, remove.accents = remove.accents))
        colnames(current.frame) <- c("Var1", list.files()[file])
        ngrams.big.frame <- join(ngrams.big.frame, current.frame, by="Var1", type="full")
        ngrams.big.frame[is.na(ngrams.big.frame)] <- 0
      }
    }
    
    
  }
  else if(gram.type == "character"){
    for(file in 1:length(list.files())){
      if(file == 1){
        first.frame <- as.data.frame(get.character.ngrams(list.files()[1], gram.size = gram.size, remove.upper = remove.upper, remove.accents = remove.accents))
        colnames(first.frame) <- c("Var1", list.files()[1])
      }
      else if(file == 2){
        second.frame <- as.data.frame(get.character.ngrams(list.files()[2], gram.size = gram.size, remove.upper = remove.upper, remove.accents = remove.accents))
        colnames(second.frame) <- c("Var1", list.files()[2])
        ngrams.big.frame <- join(first.frame, second.frame, by="Var1", type="full") # perform a full join on the two tables
        ngrams.big.frame[is.na(ngrams.big.frame)] <- 0 # replace all NAs with 0
      }
      else{
        current.frame <- as.data.frame(get.character.ngrams(list.files()[file], gram.size = gram.size, remove.upper = remove.upper, remove.accents = remove.accents))
        colnames(current.frame) <- c("Var1", list.files()[file])
        ngrams.big.frame <- join(ngrams.big.frame, current.frame, by="Var1", type="full") # perform a full join on the master table and the one new table
        ngrams.big.frame[is.na(ngrams.big.frame)] <- 0 # replace all NAs with 0
      }
    }
  }
  else{
    print("Not a known gram type!")
  }
  
  ## Proceed to modification and rotation of the big.frame object
  
  ngrams.big.frame2 <- ngrams.big.frame # Copy the original dataframe
  names <- c(as.character(ngrams.big.frame2[ ,1])) # Store the feature labels in a vector
  rownames(ngrams.big.frame2) <- names # rename the rows to the feature labels
  ngrams.big.frame2 <- ngrams.big.frame2[, -c(1)] # remove the column with the feature lables
  ngrams.big.frame3 <- as.data.frame(t(ngrams.big.frame2)) # transpose the data frame
  ngrams.big.frame3 <- ngrams.big.frame3[, order(colSums(ngrams.big.frame3), decreasing=TRUE)] # sort the data frame by frequency of columns
  
  # Normalize the frequencies by dividing by the number of words in each document.
  if(normalize.freq == TRUE){
    document.lengths <- c()
    for(file in list.files()){
      temp.document.length <- length(prepare.document(file))
      document.lengths <- append(document.lengths, temp.document.length)
    }
    ngrams.big.frame3 <- (ngrams.big.frame3)/document.lengths
    ngrams.big.frame4 <- ngrams.big.frame3[, order(colSums(ngrams.big.frame3), decreasing=TRUE)]
    return(ngrams.big.frame4)
  }
  else{
    return(ngrams.big.frame3)
  }
}


## 7. Removing personal and place names from feature sets of word n-grams

  ## With word bigrams, it is in practice considerably faster to remove the items with uppercase after collecting the frequencies
  ## of the entire set of documents. 
  
  # First, it is presumed that a data frame with features of word n-grams has been created using make.ngrams.data.frame().

homerus_word_bigrams <- make.ngrams.data.frame(gram.size = 2, gram.type = "word")

## Read in the list of uppercase items and convert it to lowercase. Change the working directory accordingly

uppercase_items <- tolower(stri_trans_nfc(scan("~/Documents/Dropbox/Homeric Authorship/List junks/Homerus_uppercase_items_no_freq.txt", what = "char", sep= "\n", fileEncoding = "UTF-8")))
uppercase_items_to_remove <- c()

# This runs a bit slowly: it is a large object to grep! It takes about 5 minutes or so.
for(i in 1:length(uppercase_items)){
  temp_hits <- grep(uppercase_items[i], colnames(homerus_word_bigrams))
  uppercase_items_to_remove <- append(uppercase_items_to_remove, temp_hits)
}

uppercase_items_to_remove <- unique(uppercase_items_to_remove)
homerus_word_bigrams.no.upper <- homerus_word_bigrams[, -c(uppercase_items_to_remove)]

## 8. Removing duplicate columns from feature sets of word bigrams

  ## Due to differences in the editions of Allen and von der Mühll, some differences in accentuation, especially of the
  ## definite article, are found. There are thus some frequent n-grams that (almost) never occur in one text or the other,
  ## which should be merged. 

  ## First, a list of features with suspiciously low frequencies in one or the other group of documents (Iliad vs. Odyssey)
  ## can be compiled. I limit the search to the top 1000 ngrams, since less frequent ngrams are unlikely to be used in later analysis.
# Low frequencies in Iliad 
low_freq_iliad <- as.data.frame(which(colSums(homerus_word_bigrams.no.upper[1:24,1:1000]) < 0.001))
# Low frequencies in Odyssey
low_freq_odyssey <- as.data.frame(which(colSums(homerus_word_bigrams.no.upper[25:48,1:1000]) < 0.001))

  ## From manual inspection of these lists up to frequency rank 500, 12 pairs of duplicates can be identified.
  ## The small utility function fix_duplicates adds the frequencies of two columns in a data frame, placing the new column at the end.
fix_duplicates <- function(input_frame, column1, column2){
  temp <- as.data.frame(input_frame[, column1] + input_frame[,column2])
  input_frame <- cbind(input_frame, temp)
  return(input_frame)
}

homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,6,8)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,28,7)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,40,91)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,64,79)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,102,72)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,135,167)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,172,69)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,180,509)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,217,505)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,315,433)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,450,77)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,402,483)
homerus_word_bigrams.no.upper <- fix_duplicates(homerus_word_bigrams.no.upper,104,86)

## The newly created columns then require names.
new_names <- c("οἳ δ", "ὃ δ", "ἣ δ","οἳ μὲν", "ὃ μὲν","ἔφαθ οἳ",  "οἳ δὲ", "αἳ δ",  "ἣ μὲν",  "ἣ δὲ", "ὃ δὲ", "ὃ δέ", "αὐτὰρ ὃ")
colnames(homerus_word_bigrams.no.upper) <- c(colnames(homerus_word_bigrams.no.upper)[1:(length(homerus_word_bigrams.no.upper) - 13)], new_names)

## Remove the original duplicate columns and order the data frame by column sums.
#columns_to_remove <- c(6,8,28,7,38,88,64,79,102,72,142,166,172,79,181,508,221,504,325,436,459,77,476,482) -- old, wrong!
columns_to_remove <- c(6,8,28,7,40,91,64,79,102,72,135,167,172,69,180,509,217,505,315,433,450,77,402,483, 104, 86)
homerus_word_bigrams.no.upper.dupl.removed <- homerus_word_bigrams.no.upper[, -c(columns_to_remove)]

homerus_word_bigrams.no.upper.dupl.removed  <- homerus_word_bigrams.no.upper.dupl.removed[, order(colSums(homerus_word_bigrams.no.upper.dupl.removed), decreasing=TRUE)]



### 9. Method of Layton (2011) for finding clusters

# A "Co-association Matrix", in Layton 2011, is a record of how often two documents are clustered together using k-means clustering
#The method employs k-means clustering to cluster documents based on a set of features, randomly choosing a value for k between a min and max based on the 
# total number of documents.
make.co.association <- function(factors_frame, n=150, feature.sampling=FALSE, k.set=FALSE, k=2){
  co.association <- data.frame(matrix(0, nrow=nrow(factors_frame), ncol=nrow(factors_frame)))
  colnames(co.association) <- row.names(factors_frame)
  row.names(co.association) <- row.names(factors_frame)
  lower <- max(min(nrow(factors_frame)/4, 10), 2) # lower limit of number of clusters: between 2 and 10
  upper <- max(min(nrow(factors_frame)/2, 30), lower+1) # upper limit of number of clusters: between 3 and 30
  for(i in 1:n){ # Layton (2011) applies an iteration of 150. I suggest 1000. 
    if(k.set == FALSE){
      k <- sample(c(lower:upper), 1) # Layton randomly picks a k between 2 and 30
    }
    if(feature.sampling == TRUE){
      current.sample <- sample((1:(length(colnames(factors_frame)))), (length(colnames(factors_frame))*.2))
    }
    else{
      current.sample <- c(1:length(colnames(factors_frame)))
    }
    factors_frame.kmeans <- kmeans(factors_frame[, current.sample], k) # do clustering
    for(i in 1:nrow(factors_frame)){ # iterate over number of documents
      current.cluster <- factors_frame.kmeans$cluster[i] # find the cluster to which the current document belongs
      shared.documents <- as.numeric(which(factors_frame.kmeans$cluster  == current.cluster)) # find all the documents that also belong to that cluster
      for(j in 1:length(shared.documents)){ # iterate over the list of documents belonging to the same cluster
        current.shared.document <- shared.documents[j] # pick out the current document
        co.association[i, current.shared.document] <- co.association[i, current.shared.document] + 1 # add one for each shared value
      }
    }
  }

  return(co.association)
}


### 10. Function for calculating cosine distance.
    ## Cosine distance is recommended in preference to Euclidean or most other distance measures when working with frequencies drawn from natural-language text.

    ## Note that this function must be applied to a matrix object. Therefore, a dataframe of features must be wrapped in as.matrix() before calling cosineDist() on it.
cosineDist <- function(x){
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}

cosineDist_pair <- function(x, y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  (x*y)/(sqrt(rowSums(x^2))*sqrt(rowSums(y^2)))
}

### 10b. Function for calculating minmax distance
  ## This assumes a single dataframe object containing all features, x.
  ## Returns a dataframe with the minmax distance between each document represented in the dataframe.
minmaxDist_df <- function(x){
  distances <- data.frame(matrix(0, nrow=nrow(x), ncol=nrow(x)))
  for(i in 1:nrow(x)){
    for(j in 1:nrow(x)){
      vector_x <- x[i, ]
      vector_y <- y[j, ]
      minima <- 0
      maxima <- 0
      for(m in 1:ncol(x)){
        minima <- minima + min(vector_x[, m], vector_y[, m])
        maxima <- maxima + max(vector_x[, m], vector_y[, m])
      }
      similarity_x_y <- minima/maxima
      distances[i, j] <- similarity_x_y
    }
  }
  return(distances)
}

minmaxDist_pair <- function(x, y){
      minima <- 0
      maxima <- 0
      for(m in 1:ncol(x)){
        minima <- minima + min(x[, m ], y[, m])
        maxima <- maxima + max(x[, m], y[, m])
      }
      similarity_x_y <- minima/maxima
      return(similarity_x_y)
}


### 11. Final workflow to produce dendrograms with word bigram features

    ## Among the highest-frequency word bigrams, it is also recommended to remove several more items manually, because they contain pronouns or parts of epithets
    # These are columns: 25, 51, 64, 76, 90, 105.

top_100_word_bigrams_corrected <- homerus_word_bigrams.no.upper.dupl.removed[, c(1:24, 26:50, 52:63, 65:75, 77:90, 92:104, 106)]
top_200_word_bigrams_corrected <- homerus_word_bigrams.no.upper.dupl.removed[, c(1:24, 26:50, 52:63, 65:75, 77:90, 92:104, 106, 107:206)]

  ## Recall that the object with features is named homerus_word_bigrams.no.upper.dupl.removed.
word_bigram_coassociation.40 <- make.co.association(top_100_word_bigrams_corrected[, 1:40], n = 1000, feature.sampling = FALSE)
word_bigram_coassociation.50 <- make.co.association(top_100_word_bigrams_corrected[, 1:50], n = 1000, feature.sampling = FALSE)
word_bigram_coassociation.60 <- make.co.association(top_100_word_bigrams_corrected[, 1:60], n = 1000, feature.sampling = FALSE)
word_bigram_coassociation.70 <- make.co.association(top_100_word_bigrams_corrected[, 1:70], n = 1000, feature.sampling = FALSE)
word_bigram_coassociation.100 <- make.co.association(top_100_word_bigrams_corrected[, 1:100], n = 1000, feature.sampling = FALSE)
word_bigram_coassociation.200 <- make.co.association(top_200_word_bigrams_corrected, n = 1000, feature.sampling = FALSE)

  ## With 50 most-frequent word bigrams. The user is encouraged to apply the same clustering functions to the 100 most-frequent word bigrams as well.
word_bigram_coassociation.50_cosine <- cosineDist(as.matrix(word_bigram_coassociation.50))
complete.clust.50.cosine <- hclust(as.dist(word_bigram_coassociation.50_cosine), "complete")
plot(complete.clust.50.cosine)
avg.clust.50.cosine <- hclust(as.dist(word_bigram_coassociation.50_cosine), "average")
plot(avg.clust.50.cosine)



## 12. Functions for the Source-Code Author Profiling (SCAP) Method


  ## shared.table.elements() is called by SCAP() below. This function returns all of the common column names found in two tables.
shared.table.elements <- function(table1, table2){
  if(length(table1) < length(table2)){
    smaller.table <- table1
    larger.table <- table2
  }
  else{
    smaller.table <- table2
    larger.table <- table1
  }
  shared <- names(smaller.table[which(names(smaller.table) %in% names(larger.table))])
  return(shared)
}

  # SCAP takes the top N features from a table corresponding to a document (or a row from a larger table, where that row corresponds to a document), 
  # and calculates the distance between the two documents as 1 - (proportion shared features), where the proportion of shared features is obtained using shared.table.elements() above.
SCAP <- function(table1, table2, top.ngrams=500){
  table1 <- table1[, 1:top.ngrams]
  table2 <- table2[, 1:top.ngrams]
  shared.top <- shared.table.elements(table1, table2)
  distance <- 1-(length(shared.top)/top.ngrams)
  return(distance)
}

### 13. Generating a dataframe of document distances using SCAP

  ## First, generate a dataframe of trigram frequencies
  character.trigrams.complete <-  make.ngrams.data.frame(gram.size = 3, gram.type = "character")

  ## This function assumes that a dataframe of features has been generated using the function make.ngrams.data.frame(), i.e., where the number of rows in the 
  ## dataframe is equal to the number of documents.
make.SCAP.matrix <- function(input_frame, SCAPSize = 500){
  # Initialize an empty data frame with a number of rows and columns equal to the number of documents.
  all.data.SCAP <- data.frame(matrix(0, nrow = nrow(input_frame), ncol = nrow(input_frame)))
  # Set row and column names based on the row names of the original dataframe.
  row.names(all.data.SCAP) <- row.names(input_frame)
  colnames(all.data.SCAP) <- row.names(input_frame)
  
  # The the iteration below is extremely slow (O2). Keep a counter to print out progress to the console.
  file1.counter <- 1
  
  for(i in 1:nrow(input_frame)){
    file1.trigrams <- input_frame[i, order(colSums(input_frame[i, ]), decreasing = TRUE)]
    #file2.counter <- 1 
    ## Get SCAP similarity 
    for(j in 1:nrow(input_frame)){
      file2.trigrams <- input_frame[j, order(colSums(input_frame[j, ]), decreasing = TRUE)]
      temp.SCAP <- SCAP(file1.trigrams, file2.trigrams, top.ngrams = SCAPSize)
      #Store the distance is the corresponding cells of the SCAP frame
      all.data.SCAP[i, j] <- temp.SCAP
      all.data.SCAP[j, i] <- temp.SCAP
    }
    file1.counter <- file1.counter + 1
    print(file1.counter) # Print out progress 
  }
  return(all.data.SCAP)
}

  ## Note: should you happen to set a SCAPSize larger than the total number of distinct ngram types in the document with the smallest number of distinct ngram types,
  ## R will generate an error. Based on the Homer data set, a maximum size of 1000 has been tested.
SCAP_500 <- make.SCAP.matrix(character.trigrams.complete, SCAPSize = 500)

## 14. Final workflow for generating dendrograms using character n-grams and SCAP (trigrams specifically). ##

    ## Note: in the case of character trigrams using the Homer data set, it may be worthwhile to generate new feature sets removing uppercase letters and/or diacritics.
    ## Note 2: Inspecting the feature set and removing features containing clear traces of pronouns (esp. 1st person) may be desirable.

    ## This workflow is essentially identical to 11. above.

char_trigram_SCAP_500.coassociation <- make.co.association(SCAP_500_iliad, n = 1000, feature.sampling = FALSE)
##word_bigram_coassociation.100 <- make.co.association(top_100_word_bigrams_corrected[, 1:100], n = 1000, feature.sampling = FALSE)


char_trigram_SCAP_500.coassociation_cosine <- cosineDist(as.matrix(char_trigram_SCAP_500.coassociation))
complete.clust.SCAP_500 <- hclust(as.dist(char_trigram_SCAP_500.coassociation_cosine), "complete")
plot(complete.clust.SCAP_500)
avg.clust.SCAP_500 <- hclust(as.dist(char_trigram_SCAP_500.coassociation_cosine), "average")
plot(avg.clust.SCAP_500)





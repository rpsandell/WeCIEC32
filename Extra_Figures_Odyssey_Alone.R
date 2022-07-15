
# Bozzone & Sandell, "One or Many Homers?", Proceedings of WeCIEC 32: Extra figures

# The code below produces additional dendrograms not found in the printed version of the article. 
# The availability of additional dendrograms is referenced at fn. 20 and 34 in the article. 

# The two additional dendrograms that can be generated with the code below paralllel figures 8 and 9 in the printed article:
# they constitute dendrograms based on the 100 most-frequent word bigrams or the SCAP method, respectively, for the text of the Odyssey.

# FIGURE 8A: Top 100 Word Bigrams in the Odyssey

# The following sequence of code generates a .txt file "Odyssey_Top_100_Word_Bigrams.txt", which is available in the Github repository, and which can then be used
# to generate a co-association matrix and dendrogram.

# Generate word bigram frequencies using make.ngrams.data.frame()
setwd("~/Documents/Dropbox/Homeric Authorship/Greek Texts/TLG_Homerus/Odyssey")
odyssey_word_bigrams <- make.ngrams.data.frame(gram.size = 2, gram.type = "word")

## Read in the list of uppercase items and convert it to lowercase. Change the working directory accordingly. The .txt file "Homerus_uppercase_items_no_freq.txt" is 
#available in the Github repository.

uppercase_items <- tolower(stri_trans_nfc(scan("~/Documents/Dropbox/Homeric Authorship/List junks/Homerus_uppercase_items_no_freq.txt", what = "char", sep= "\n", fileEncoding = "UTF-8")))
uppercase_items_to_remove <- c()

# Remove bigrams containing an uppercase word. This runs slowly. Allow at least five minutes. 
for(i in 1:length(uppercase_items)){
  temp_hits <- grep(uppercase_items[i], colnames(odyssey_word_bigrams))
  uppercase_items_to_remove <- append(uppercase_items_to_remove, temp_hits)
}

uppercase_items_to_remove <- unique(uppercase_items_to_remove)
odyssey_word_bigrams.no.upper <- odyssey_word_bigrams[, -c(uppercase_items_to_remove)]

# Reduce the set to the 150 most-frequent items; identify topical features by hand and remove them, then reduce the feature set to the 100 most-frequent items.
odyssey_150_word_bigrams.no.upper <- odyssey_word_bigrams.no.upper[, 1:150]
# Many of the features removed here in fact involve personal pronouns
odyssey_word_bigrams.no.upper_corrected <- odyssey_150_word_bigrams.no.upper[-c(16, 20, 41,48, 49, 50, 89, 91, 101, 107)]
odyssey_word_bigrams.no.upper_corrected_100 <- odyssey_word_bigrams.no.upper_corrected[,1:100]

# Read in "Odyssey_Top_100_Word_Bigrams.txt" if you are unable to generate the data from raw texts.
odyssey_word_bigrams.no.upper_corrected_100 <- read.table("Iliad_Top_100_Word_Bigrams.txt", header=T, sep="\t")

# Create co-association matrix
odyssey_word_bigram_coassociation.100_corrected <- make.co.association(odyssey_word_bigrams.no.upper_corrected_100, n = 1000, feature.sampling = FALSE)

# Calculate cosine distance and create dendrogram using average-linkage clustering
odyssey_word_bigram_coassociation.100_corrected_cosine <- cosineDist(as.matrix(odyssey_word_bigram_coassociation.100_corrected))
odyssey_avg.clust.100_corrected.cosine <- hclust(as.dist(odyssey_word_bigram_coassociation.100_corrected_cosine), "average")
# Plot the dendrogram, add boxes for four clusters
plot(odyssey_avg.clust.100_corrected.cosine)
rect.hclust(odyssey_avg.clust.100_corrected.cosine, k=4)



# FIGURE 9A: ## FIGURE 9: SCAP 500 trigrams, Odyssey only
setwd("~/Documents/Dropbox/Homeric Authorship/Greek Texts/TLG_Homerus/Odyssey")

# Use the function make.ngrams.data.frame() from "Homeric_Authorship_2021_V2.R" to gather frequencies of character trigrams
character.trigrams.complete.odyssey <-  make.ngrams.data.frame(gram.size = 3, gram.type = "character")

# Use the function make.SCAP.matrix() to gather SCAP distances between documents. Note: the runtime of this function is EXTREMELY slow. Allow for ca. 30 minutes.
SCAP_500_Odyssey <- make.SCAP.matrix(character.trigrams.complete.odyssey, SCAPSize = 500)
# The resulting data frame is also available as "SCAP_500_Odyssey.txt" in this Github repository.

# Generate co-association matrix using the function make.co.association from "Homeric_Authorship_2021_V2.R"
char_trigram_SCAP_500.coassociation <- make.co.association(SCAP_500_Odyssey, n = 1000, feature.sampling = FALSE)


# Calculate cosine distance between the elements of the co-association matrix, create a dendrogram using average-linkage clustering, plot the dendrogram
char_trigram_SCAP_500.coassociation_cosine <- cosineDist(as.matrix(char_trigram_SCAP_500.coassociation))
avg.clust.SCAP_500 <- hclust(as.dist(char_trigram_SCAP_500.coassociation_cosine), "average")
plot(avg.clust.SCAP_500)

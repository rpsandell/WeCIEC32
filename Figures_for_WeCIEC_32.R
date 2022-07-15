
# Bozzone & Sandell, "One or Many Homers?", Proceedings of WeCIEC 32: In-article figures

# The code below generates each of the major dendrogram figures seen in the article, i.e., Figures 2, 6, 7, 8, and 9.
  # To find the code relevant to a particular figure, simply use the search function in your text editor or IDE, and search for, e.g. "## FIGURE 2"

# Where possible, files with processed data generated from the raw texts are provided.
# Where figures were generated based on raw texts extracted from the TLG, the relevant code is provided here, but the texts are not.
    # A separate file explains the procedure for extracting texts from the TLG (assuming the reader owns a copy) using the Classical Language Toolkit in Python 3.

# Load the following necessary packages:
library(stringi)
library(cluster)
library(ngram)
library(dplyr)

# Refer to the file "Homeric_Authorship_2021_V2.R" for other necessary functions used for creating the figures. 

## FIGURE 2
  # Note that Figures 3 and 4 constitute zoomed-in sections of Figure 2
# Figure 2 was generated from a corpus of Greek texts as described in the article using functions provided by the R package stylo().

# load package stylo()
library(stylo)
# read in the corpus
big.raw.corpus <- load.corpus(files = "all", corpus.dir = "~/Documents/Dropbox/Homeric Authorship/Greek Texts/Impostors_2", encoding = "UTF-8")
# tokenize the corpus
big.corpus.tokenized <- txt.to.words.ext(big.raw.corpus, preserve.case = FALSE, corpus.lang = "Greek")

# Replace all characters with diacritics with corresponding simple characters
  # This procedure is carried out by iterating over each tokenized document in the corpus and applying a global character substitution
for(i in 1:length(big.corpus.tokenized)){
  big.corpus.tokenized[[i]] <- gsub("ἀ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἁ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ά", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἄ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἅ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἂ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἃ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἆ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἇ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ά", "α", big.corpus.tokenized[[i]])# Alpha with oxia, not tonos!
  big.corpus.tokenized[[i]] <- gsub("ὰ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ᾶ", "α", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("έ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἐ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἑ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἔ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἕ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἒ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἓ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἕ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("έ", "ε", big.corpus.tokenized[[i]]) # Epsilon with oxia, not tonos!
  big.corpus.tokenized[[i]] <- gsub("ὲ", "ε", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ή", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἠ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἡ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἤ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἥ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἢ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἣ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἦ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἧ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ή", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὴ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ῆ", "η", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἰ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἱ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ί", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἴ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἵ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἲ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἳ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἶ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ἷ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ί", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὶ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ῖ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ΐ", "ι", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὀ", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὁ", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὄ", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὅ", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὂ", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὃ", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ό", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ό", "ο", big.corpus.tokenized[[i]])#Omicron with acute, not tonos!
  big.corpus.tokenized[[i]] <- gsub("ὸ", "ο", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ύ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὐ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὑ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὔ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὕ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὒ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὓ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὖ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὗ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ύ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὺ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ῦ", "υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ΰ","υ", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὠ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὡ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὤ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὥ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὢ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὣ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὦ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὧ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ώ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ὼ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ῶ", "ω", big.corpus.tokenized[[i]])
  big.corpus.tokenized[[i]] <- gsub("ώ", "ω", big.corpus.tokenized[[i]])
}

# Feature set will be word bigrams. Convert the texts to sets of word bigram features.
big.corpus.word.2.grams <- txt.to.features(big.corpus.tokenized, ngram.size = 2, features = "w")

# Make frequency tables and cull the feature set at 50%.
big.corpus.2000.freq.bigrams <- make.frequency.list(big.corpus.word.2.grams, head = 2000)
big.corpus.2000.freq.bigrams.freqs <- make.table.of.frequencies(big.corpus.word.2.grams, features = big.corpus.2000.freq.bigrams)
big.corpus.culled.freqs.bigrams <- perform.culling(big.corpus.2000.freq.bigrams.freqs, culling.level =50)

# Use the stylo graphical user interface to change font size, height, and width of output: Under "Output", set Plot height and Plot width to 11, Font size to 1
# Under "Features, set MFW SETTINGS, Maximum to 53
# To save output, check "PDF" under "Output"
stylo(gui = TRUE, frequencies = big.corpus.culled.freqs.bigrams, write.png = FALSE, mfw.min = 53)

## FIGURE 6: SCAP (Source-Code Author Profiling), 500 character trigrams, cosine distance

# Set working directory to a directory with 48 documents corresponding to books of the Homeric corpus.
setwd("~/Documents/Dropbox/Homeric Authorship/Greek Texts/TLG_Homerus/Homer_All")

# Use the function make.ngrams.data.frame() from "Homeric_Authorship_2021_V2.R" to gather frequencies of character trigrams
character.trigrams.complete <-  make.ngrams.data.frame(gram.size = 3, gram.type = "character")

# Use the function make.SCAP.matrix() to gather SCAP distances between documents. Note: the runtime of this function is EXTREMELY slow. Allow for ca. 30 minutes.
SCAP_500_Homer <- make.SCAP.matrix(character.trigrams.complete, SCAPSize = 500)
  # The resulting data frame is also available as "SCAP_500_Homer.txt" in this Github repository.

# Generate co-association matrix using the function make.co.association from "Homeric_Authorship_2021_V2.R"
char_trigram_SCAP_500.coassociation <- make.co.association(SCAP_500_Homer, n = 1000, feature.sampling = FALSE)

# Calculate cosine distance between the elements of the co-association matrix, create a dendrogram using average-linkage clustering, plot the dendrogram
char_trigram_SCAP_500.coassociation_cosine <- cosineDist(as.matrix(char_trigram_SCAP_500.coassociation))
avg.clust.SCAP_500 <- hclust(as.dist(char_trigram_SCAP_500.coassociation_cosine), "average")
plot(avg.clust.SCAP_500)

## FIGURE 7: Principal Components Analysis 

# A .txt file, "Homer_Top_100_Bigrams_for_PCA.txt" is provided in the Github repository. This contains the frequencies of the 100 most-frequent word bigrams,
# subject to both automatic cleaning (i.e., collapsing duplicate features) and manual cleaning (i.e., removing some "topical" features)

# The package "ggbiplot" is needed.
# NB: current version of ggbiplot not available through CRAN for current build of R
# Install using devtools: 
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

# Read in the data
homer_100_bigrams.cleaned_for_pcr <- read.table("Homer_Top_100_Bigrams_for_PCA.txt", header=T, sep="\t")

# Adjust names of data labels
homer.groups <- c(rep("Iliad", 24), rep("Odyssey", 24))

book_names_shortest <- as.vector(rownames(homer_100_bigrams.cleaned_for_pcr))
book_names_shortest <- gsub("Iliados_", "", book_names_shortest)
book_names_shortest <- gsub("Odusseias_", "", book_names_shortest)
book_names_shortest <- gsub("\\.txt", "", book_names_shortest)
ggbiplot(top_100_word_bigrams_pcr,var.axes = FALSE, labels = book_names_shortest, ellipse=TRUE, groups=homer.groups, alpha=0, labels.size = 3) + 
  scale_color_grey(start = 0.1, end = 0.7) + theme(text = element_text(family = "Times", size = 10))

## FIGURE 8: Top 100 Word Bigrams in the Iliad

# The following sequence of code generates a .txt file "Iliad_Top_100_Word_Bigrams.txt", which is available in the Github repository, and which can then be used
# to generate a co-association matrix and dendrogram.

# Generate word bigram frequencies using make.ngrams.data.frame()
setwd("~/Documents/Dropbox/Homeric Authorship/Greek Texts/TLG_Homerus/Iliad")
iliad_word_bigrams <- make.ngrams.data.frame(gram.size = 2, gram.type = "word")

## Read in the list of uppercase items and convert it to lowercase. Change the working directory accordingly. The .txt file "Homerus_uppercase_items_no_freq.txt" is 
#available in the Github repository.

uppercase_items <- tolower(stri_trans_nfc(scan("~/Documents/Dropbox/Homeric Authorship/List junks/Homerus_uppercase_items_no_freq.txt", what = "char", sep= "\n", fileEncoding = "UTF-8")))
uppercase_items_to_remove <- c()

# Remove bigrams containing an uppercase word. This runs slowly. Allow at least five minutes. 
for(i in 1:length(uppercase_items)){
  temp_hits <- grep(uppercase_items[i], colnames(iliad_word_bigrams))
  uppercase_items_to_remove <- append(uppercase_items_to_remove, temp_hits)
}

uppercase_items_to_remove <- unique(uppercase_items_to_remove)
iliad_word_bigrams.no.upper <- iliad_word_bigrams[, -c(uppercase_items_to_remove)]

# Reduce the set to the 150 most-frequent items; identify topical features by hand and remove them, then reduce the feature set to the 100 most-frequent items.
iliad_150_word_bigrams.no.upper <- iliad_word_bigrams.no.upper[, 1:150]
iliad_word_bigrams.no.upper_corrected <- iliad_150_word_bigrams.no.upper[-c(38,73,87,89)]
iliad_word_bigrams.no.upper_corrected_100 <- iliad_word_bigrams.no.upper_corrected[,1:100]

# Read in "Iliad_Top_100_Word_Bigrams.txt" if you are unable to generate the data from raw texts.
iliad_word_bigrams.no.upper_corrected_100 <- read.table("Iliad_Top_100_Word_Bigrams.txt", header=T, sep="\t")

# Create co-association matrix
iliad_word_bigram_coassociation.100_corrected <- make.co.association(iliad_word_bigrams.no.upper_corrected_100, n = 1000, feature.sampling = FALSE)

# Calculate cosine distance and create dendrogram using average-linkage clustering
iliad_word_bigram_coassociation.100_corrected_cosine <- cosineDist(as.matrix(iliad_word_bigram_coassociation.100_corrected))
iliad_avg.clust.100_corrected.cosine <- hclust(as.dist(iliad_word_bigram_coassociation.100_corrected_cosine), "average")
# Plot the dendrogram, add boxes for four clusters
plot(iliad_avg.clust.100_corrected.cosine)
rect.hclust(iliad_avg.clust.100_corrected.cosine, k=4)



## FIGURE 9: SCAP 500 trigrams, Iliad only
# Procedure is the same as for Iliad and Odyssey together above

setwd("~/Documents/Dropbox/Homeric Authorship/Greek Texts/TLG_Homerus/Iliad")

# Use the function make.ngrams.data.frame() from "Homeric_Authorship_2021_V2.R" to gather frequencies of character trigrams
character.trigrams.complete.iliad <-  make.ngrams.data.frame(gram.size = 3, gram.type = "character")

# Use the function make.SCAP.matrix() to gather SCAP distances between documents. Note: the runtime of this function is EXTREMELY slow. Allow for ca. 30 minutes.
SCAP_500_Iliad <- make.SCAP.matrix(character.trigrams.complete.iliad, SCAPSize = 500)
  # The resulting data frame is also available as "SCAP_500_Iliad.txt" in this Github repository.

# Generate co-association matrix using the function make.co.association from "Homeric_Authorship_2021_V2.R"
char_trigram_SCAP_500.coassociation <- make.co.association(SCAP_500_Iliad, n = 1000, feature.sampling = FALSE)
  

# Calculate cosine distance between the elements of the co-association matrix, create a dendrogram using average-linkage clustering, plot the dendrogram
char_trigram_SCAP_500.coassociation_cosine <- cosineDist(as.matrix(char_trigram_SCAP_500.coassociation))
avg.clust.SCAP_500 <- hclust(as.dist(char_trigram_SCAP_500.coassociation_cosine), "average")
plot(avg.clust.SCAP_500)



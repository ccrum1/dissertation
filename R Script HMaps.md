#load the required packages 
> library(readtext)
> library(gplots)
> library(tm)
> library(stringdist)

#create an object called folder
> folder <- "C:/Users/cmcru/OneDrive/Documents/RProjects/Heatmaps/1980"

#read texts from folder into legislative_texts
> legislative_texts <- readtext(folder)
> leg_country <- character()
> leg_year <- numeric()

#loop to extract country, year from each .txt file in the folder
> for (leg_name in legislative_texts$doc_id) {
+ leg_name <- gsub(".txt|Leg-","",leg_name)
+ country <- strsplit(leg_name, "_")[[1]][1]
+ leg_country <- c(leg_country,country) 
+ year <- as.numeric(strsplit(leg_name,"_")[[1]][2]) 
+ leg_year <- c(leg_year,year)
+ }
> legislative_texts <- cbind(legislative_texts,leg_country)
> legislative_texts <- cbind(legislative_texts,leg_year)
> legislative_texts <- legislative_texts[order(legislative_texts$leg_year),]

#preprocess 
> corpus <- VCorpus(VectorSource(legislative_texts$text))
> corpus <- tm_map(corpus, removePunctuation)
> corpus <- tm_map(corpus, content_transformer(tolower))
> corpus <- tm_map(corpus, removeNumbers)
> corpus <- tm_map(corpus, removeWords, stopwords("english"))

#create distance matrix of corpus 
> distance_matrix <- as.matrix(dtm, method="binary")
> distance_matrix_5gram <- stringdistmatrix(legislative_texts$text, legislative_texts$text, method = "jaccard", q = 5)

#visualize in heatmap format
> heatmap.2(distance_matrix_5gram, dendrogram = 'none',
+ Rowv = FALSE,
+ Colv = FALSE,
+ symm = TRUE,
+ trace = 'none',
+ density.info = 'none',
+ main = "1980 Leg Text Similarity",
+ labCol = paste(legislative_texts$leg_country, legislative_texts$leg_year, sep = "-"),
+ labRow = paste(legislative_texts$leg_country, legislative_texts$leg_year, sep = "-"),
+ cexRow = 0.6,
+ cexCol = 0.6)

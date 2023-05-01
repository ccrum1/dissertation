#this workflow has two options; option 2 is shorter but requires manual cleaning; it is the only option for scanned PDFs without text aspects linked to the file

#OPTION1
#batch convert PDF to .txt using "pdftools" package
#this step makes workflow much faster but eliminates control by eliminating some manual cleaning of the .txt file

> library(pdftools)
> library(tm)
> library(readtext)
> library(gplots)
> library(stringdist)

#create a vector of the PDF file names
> setwd("C:/shift right click and copy paste file path here, change backslashes to forward slashes")
> files <- list.files(pattern = "pdf$")

#create a corpus using the tm package composed of the text contents of the PDF files named in files
> corpus <- Corpus(URISource(files), readerControl = list(reader = readPDF))

#preprocess
> corpus <- tm_map(corpus, removePunctuation, ucp = TRUE)
> corpus <- tm_map(corpus, content_transformer(tolower))
> corpus <- tm_map(corpus, removeNumbers)
> corpus <- tm_map(corpus, removeWords, stopwords("english"))
> corpus <- tm_map(corpus, removeWords, c("article", "Article", "Chapter", "chapter"))

#using the for function to loop through the object, corpus, and extract content and then iteratively use the writeLines function to save the content as a plaint text file 
> for (i in seq_along(corpus)) {
+     content_text <- corpus[[i]]$content
+     file_name <- paste0("leg", i, ".txt")
+     writeLines(content_text, file_name)
+ }

#create an object called folder; this would be a folder of the .txt files saved in the previous step or a folder of the manually cleaned files
> folder <- "C:/file path here"

#read texts from folder into legislative_texts object
> legtext <- readtext(folder)
> leg_country <- character()
> leg_year <- numeric()

#loop to extract country, year from each .txt file in folder
> for (leg_name in legislative_texts$doc_id) {
+ leg_name <- gsub(".txt|Leg-","",leg_name)
+ country <- strsplit(leg_name, "_")[[1]][1]
+ leg_country <- c(leg_country,country) 
+ year <- as.numeric(strsplit(leg_name,"_")[[1]][2]) 
+ leg_year <- c(leg_year,year)
+ }

#use cbind function to add metadata to legislative_texts
> legtext <- cbind(legtext,leg_country)
> legtext <- cbind(legtext,leg_year)

#check your values to make sure symmetrical and correct
> show(leg_year)
> show(leg_country)

#create distance matrix of corpus (if desired for analysis purposes)
> dtm <- DocumentTermMatrix(corpus)
> distance_matrix <- as.matrix(dtm, method="binary")

#create 5-gram distance matrix from legislative text
> distance_matrix_5gram <- stringdistmatrix(legtext$text, legtext$text, method = "jaccard", q = 5)

#visualize in heatmap format
> heatmap.2(distance_matrix_5gram, 
+ dendrogram = 'none',
+ Rowv = FALSE,
+ Colv = FALSE,
+ symm = TRUE,
+ trace = 'none',
+ density.info = 'none',
+ main = "1980 Leg Text Similarity",
+ labCol = paste(legtext$leg_country, legtext$leg_year, sep = "-"),
+ labRow = paste(legtext$leg_country, legtext$leg_year, sep = "-"),
+ cexRow = 0.6,
+ cexCol = 0.6)

##Rowv and Colv can be changed to TRUE if hierarchical ordering by similarity is desired

#OPTION2 (short version with pre-cleaned texts)
#load the required packages 
> library(readtext)
> library(gplots)
> library(tm)
> library(stringdist)

#create an object called folder; this would be a folder of .txt files
> folder <- "C:/file path here"

#read texts from folder into legislative_texts
> legtext <- readtext(folder)
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

#use cbind function to add metadata to legtext object
> legtext <- cbind(legtext,leg_country)
> legtext <- cbind(legtext,leg_year)


#check your values to make sure symmetrical and correct
> show(leg_year)
> show(leg_country)

#create 5-gram string distance matrix from manually cleaned legislative text
> distance_matrix_5gram <- stringdistmatrix(legtext$text, legtext$text, method = "jaccard", q = 5)

#visualize in heatmap format
> heatmap.2(distance_matrix_5gram, 
+ dendrogram = 'none',
+ Rowv = FALSE,
+ Colv = FALSE,
+ symm = TRUE,
+ trace = 'none',
+ density.info = 'none',
+ main = "1980 Leg Text Similarity",
+ labCol = paste(legtext$leg_country, legtext$leg_year, sep = "-"),
+ labRow = paste(legtext$leg_country, legtext$leg_year, sep = "-"),
+ cexRow = 0.6,
+ cexCol = 0.6)

##Rowv and Colv can be changed to TRUE if hierarchical ordering by similarity is desired
##In all cases, it is a good idea to maximize your plot window in R in order to show all pixels
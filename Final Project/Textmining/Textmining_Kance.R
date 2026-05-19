#load the data/direction so we can pick the data from the folder.

#loading the different libraries needed for the code.
library(dplyr)
library(stringr)
library(pdftools)
library(ggplot2)
library(tidyr)

#setting the WD so we can grab the pdf-files from the correct folder.
setwd("data/") #in case your Working Directory isn't set.
files <- list.files(pattern = ".pdf$") #here we do so it only looks for the .pdf, files.


#Using the 'head' command to check if our data is loaded properly and that it checks out.
head(files)

#now we define a new element as our keywords which we will look for in the textmining.
keywords <- 
  c(
    "konge",
    "brev",
    "befal",
    "beskat",
    "bisp",
    "sogn",
    "retterting",
    "herreddag",
    "ting",
    "len",
    "københavn",
    "kbhvn",
    "kansler",
    "hof",
    "rentemest",
    "rigsraad",
    "indkomst",
    "foged",
    "told",
    "konsumption",
    "accise",
    "cise",
    "landsting",
    "stænderforsamling",
    "landkommissær",
    "landgilde",
    "godsdrift",
    "herlighed"
  )
#total: 28 keywords.


filelength <- length(files)
wordlength <- length(keywords)

word_count <- seq(1,filelength*wordlength)
dim(word_count) <- c(filelength,wordlength)

j <- 1

for (j in 1:length(files)) {
  P1 <- pdftools::pdf_text(pdf = files[j]) %>%
    str_to_lower() %>%
    str_replace_all("\\t", "") %>%
    str_replace_all("\n", " ") %>%
    str_replace_all("      ", " ") %>%
    str_replace_all("    ", " ") %>%
    str_replace_all("   ", " ") %>%
    str_replace_all("  ", " ") %>%
    str_replace_all("[:digit:]", "") %>%
    str_replace_all("[:punct:]", "") %>%
    str_trim()
  
  for (i in 1:length(keywords)) {
    word_count[j,i] <- P1 %>% str_count(keywords[i]) %>% sum()
  }
  
}

head(word_count)
```

View(j) #test

P1[1]


###
word_count <- as.data.frame(word_count)
rownames(word_count) <- files
colnames(word_count) <- keywords

######
# Add a year collum:
word_count$year <- as.numeric(str_extract(rownames(word_count), "\\d{4}"))

# Create the plot:
ggplot(word_count, aes(x = year, y = befal)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Forekomster af 'konge' i Kancelliets breve",
    x = "År",
    y = "Antal forekomster"
  ) +
  theme_minimal()



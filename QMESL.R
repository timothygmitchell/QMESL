# Quantitative Methods of Evaluating Song Lyrics
# Timothy Mitchell with ideas from Dr. Gregory Matthews, Ph.D.

library(NLP); library(cld3) # NLP
library(FSA); library(sjstats); library(effsize) # effect sizes
library(ggplot2); library(ggthemes); library(ggridges); library(gridExtra) # plots

################################ IMPORT DATA, PRELIMINARY DATA CLEANING ################################

library(ggplot2) # graphics
library(ggthemes) # additional graphics themes
library(ggridges) # ridge plots
library(gridExtra) # arranging graphical objects

library(NLP) # as.String() function
library(cld3) # language detection

library(FSA) # nonparametric multiple comparisons
library(sjstats) # epsilon squared (effect size)
library(effsize) # Cliff's delta (effect size)

# read in data from https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics/data
 
lyrics <- read.csv("~/Documents/R/Research/lyrics.csv", stringsAsFactors = FALSE)

lyrics <- subset(lyrics, lyrics != "") # remove 95680 songs with no lyrics

lyrics$lyrics <- gsub("\\n", " ", lyrics$lyrics) # remove newline characters

bad.words <- c("verse", "chorus", "bridge", "refrain") # words to remove
for (i in 1:length(bad.words)){
  lyrics$lyrics <- gsub(paste0('\\b', bad.words[i], '\\b'), "", lyrics$lyrics) # filter words
}

# rm punctuation (GOOD: goin' = goin; BAD: we're (we are) = were (past tense of be))

# Grouping symbols, periods, and question marks are regex characters; these require double 
# brackets [] for compatibility with the gsub function. The double quote is a special 
# chararacter in R and needs to be escaped with a backslash \

bad.chars <- c(",", "'", ";", ":", "-", "+", "%", "!", "\"", "[?]", "[.]", 
               "[[]", "[]]", "[(]", "[)]", "[{]", "[}]", ">", "<") 

for (i in 1:length(bad.chars)){
  lyrics$lyrics <- gsub(bad.chars[i], "", lyrics$lyrics) # filter punctuation
}

lyrics$lyrics <- gsub("[ ]+", " ", lyrics$lyrics) # remove duplicate spaces

# A peculiar entry with unicode errors (null characters); might have to read in the binary
lyrics[lyrics$index == "188254",]; # disappears during a later data cleaning step

# inspect non-English entries
# cld3 package uses Google's neural network model for language identification
# source: https://cran.r-project.org/web/packages/cld3/cld3.pdf
# remove non-English entries (mostly true positives)
lyrics <- subset(lyrics, detect_language(lyrics) == "en") # remove 24179 additional songs
  
# change to lowercase
lyrics$lyrics <- tolower(lyrics$lyrics)

# saveRDS(lyrics, file="~/Documents/R/Research/lyrics_clean.csv") # save this object to directory

############################### SOME PRELIMINARY DATA VISUALIZATIONS ###################################

lyrics <- readRDS(file="~/Documents/R/Research/lyrics_clean.csv") # retrieve data

counts <- as.numeric(table(lyrics$genre)) # count the number of songs in each genre
names(counts) <- names(table(lyrics$genre)) # retrieve names
  
options(scipen=999) # disable scientific notation in graphical output 
  
# visualize the number of songs in each genre (ggplot2)
  
df <- data.frame(genre = names(counts), val = counts)
  
df$genre <- factor(df$genre, levels = df$genre[order(df$val, decreasing=TRUE)]) # enforce order
  
ggplot(data = df, aes(x = genre, y = val)) +
    theme_bw() +
    geom_bar(stat="identity", position=position_dodge(), fill="black")+
    scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,110000))+
    ggtitle("Number of Unique Songs per Genre", subtitle = "235,638 Total Songs")+
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), axis.text=element_text(colour="black"))+
    xlab("Genre")+
    ylab("Number of Unique Songs")

# visualize the number of artists in each genre (ggplot2)

df_art <- aggregate(artist ~ genre, lyrics, function(x)length(unique(x)))
df_art$genre <- factor(df_art$genre, levels = df_art$genre[order(df_art$artist, decreasing=TRUE)])

ggplot(data = df_art,
       aes(x = genre, y = artist)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(), fill="black")+
  scale_y_continuous(breaks=scales::pretty_breaks(n=7), expand=c(0,0),limits=c(0,3700))+
  ggtitle("Number of Unique Artists per Genre", subtitle = "12,674 Artists")+
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), axis.text=element_text(colour="black"))+
  xlab("Genre")+
  ylab("Number of Unique Artists")

df_spa <- merge(df, df_art, by = "genre")
df_spa$SPA <- df_spa$val/df_spa$artist
df_spa$genre <- factor(df_spa$genre, levels = df_spa$genre[order(df_spa$SPA, decreasing=TRUE)])

ggplot(data = df_spa,
       aes(x = genre, y = SPA)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(), fill="black")+
  scale_y_continuous(breaks=scales::pretty_breaks(n=7), expand=c(0,0),limits=c(0,45))+
  ggtitle("Mean Number of Songs per Artist by Genre")+
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), axis.text=element_text(colour="black"))+
  xlab("Genre")+
  ylab("Mean Number of Songs per Artist")

# Remove genres 'not available' and 'other'

lyrics <- subset(lyrics, genre != "Not Available" & genre != "Other") # subset data

counts <- as.numeric(table(lyrics$genre)) # count the number of songs in each genre
names(counts) <- names(table(lyrics$genre)) # retrieve names

# visualize the number of songs in each genre (ggplot2)

df <- data.frame(genre = names(counts), val = counts)

df$genre <- factor(df$genre, levels = df$genre[order(df$val, decreasing=TRUE)]) # enforce order

ggplot(data = df, aes(x = genre, y = val)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(), fill="black")+
  scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,110000))+
  ggtitle("Unique Songs, By Genre", subtitle = "Total: 214,452 Songs\n")+
  theme(plot.title=element_text(hjust=0.5, size = 32), 
        plot.subtitle=element_text(hjust=0.5, size = 28), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30))+
  xlab("\nGenre")+
  ylab("Number of Unique Songs\n")

# visualize the number of artists in each genre (ggplot2)

df_art <- aggregate(artist ~ genre, lyrics, function(x)length(unique(x)))
df_art$genre <- factor(df_art$genre, levels = df_art$genre[order(df_art$artist, decreasing=TRUE)])

ggplot(data = df_art,
       aes(x = genre, y = artist)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(), fill="black")+
  scale_y_continuous(breaks=scales::pretty_breaks(n=7), expand=c(0,0),limits=c(0,3700))+
  ggtitle("Unique Artists, By Genre", subtitle = "Total: 9,102 Artists\n")+
  theme(plot.title=element_text(hjust=0.5, size = 32), 
        plot.subtitle=element_text(hjust=0.5, size = 28), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30))+
  xlab("\nGenre")+
  ylab("Number of Unique Artists\n")

df_spa <- merge(df, df_art, by = "genre")
df_spa$SPA <- df_spa$val/df_spa$artist
df_spa$genre <- factor(df_spa$genre, levels = df_spa$genre[order(df_spa$SPA, decreasing=TRUE)])

ggplot(data = df_spa,
       aes(x = genre, y = SPA)) +
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(), fill="black")+
  scale_y_continuous(breaks=scales::pretty_breaks(n=7), expand=c(0,0),limits=c(0,45))+
  ggtitle("Average Number of Unique Songs per Artist, By Genre\n")+
  theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30))+
  xlab("\nGenre")+
  ylab("Average Number of Songs per Artist\n")

################################### WORD COUNTS FUNCTIONS ##############################################

lyrics <- readRDS(file="~/Documents/R/Research/lyrics_clean.csv") # retrieve data
lyrics <- subset(lyrics, genre != "Not Available" & genre != "Other") # subset data
  
# example: count total words
  
lengths(strsplit(lyrics[1,6], " ")); # 433 words
strsplit(lyrics[1,6], " ") 
  
# count unique words
  
length(unique(unlist(strsplit(lyrics[1,6], " ")))); # 137 words
unique(unlist(strsplit(lyrics[1,6], " ")))
  
# vector of total number of words for the first 30 songs
  
sapply(head(lyrics[, 6], n=30), function(x) lengths(strsplit(x, " ")), USE.NAMES = FALSE)
  
# vector of number of unique words for the first 30 songs
  
sapply(head(lyrics[, 6], n=30),  function(x) length(unique(unlist(strsplit(x, " ")))), USE.NAMES = FALSE)
  
cor.test(sapply(head(lyrics[, 6], n=300), function(x) lengths(strsplit(x, " ")), USE.NAMES = FALSE),
        sapply(head(lyrics[, 6], n=300),  function(x) length(unique(unlist(strsplit(x, " ")))), USE.NAMES = FALSE))

# Total number of words is correlated to the number of unique words (duh!), corr = 0.78.
# We will see that this ratio varies somewhat as a function of genre
  
# define a function to count total words
# define a function to count unique words
  
totalwc <- function(songlyrics) {
    numberofwords <- lengths(strsplit(as.String(songlyrics), " "))
    return(numberofwords)
  }
  
uniquewc <- function(songlyrics) {
    numberofwords <- length(unique(unlist(strsplit(as.String(songlyrics), " "))))
    return(numberofwords)
  }
  
# use the wordcount functions
  
totalwords <- sapply(lyrics[, 6], totalwc, USE.NAMES = FALSE)
  
uniquewords <- sapply(lyrics[, 6], uniquewc, USE.NAMES = FALSE)
  
percentunique <- uniquewords/totalwords*100
  
lyrics <- cbind(lyrics, totalwords, uniquewords, percentunique)

# saveRDS(lyrics, file="~/Documents/R/Research/lyrics_calculations.csv") # save this object to directory

################################### WORD COUNT DATA VISUALIZATION ######################################

# retrieve data
lyrics <- readRDS(file="~/Documents/R/Research/lyrics_calculations.csv")

# vizualize distributions
  
g2 <- ggplot(data = lyrics, aes(x=totalwords, fill=genre)) +
    theme_bw() + 
    geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', bins=50) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,11000))+
  theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26))+
    labs(fill="")+
    xlab("\nNumber of Words")+
    ylab("Number of Songs\n")+
    ggtitle("Total Words per Song, By Genre\n")+
    xlim(c(0, 1000)); g2
  
g2scale <- ggplot(data = lyrics, aes(x=totalwords, fill=genre)) +
    theme_bw() + 
    geom_density(color="#e9ecef", alpha=0.5) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,0.0065))+
    theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26))+
    labs(fill="")+
    xlab("\nNumber of Words")+
    ylab("Density\n")+
    ggtitle("Total Words per Song, By Genre (Scaled)\n")+
    xlim(c(0, 1500)); g2scale
  
g3 <- ggplot(data = lyrics, aes(x=uniquewords, fill=genre)) +
    theme_bw() + 
    geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', bins=60) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,600))+
    theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26))+
    labs(fill="")+
    xlab("\nNumber of Unique Words")+
    ylab("Number of Songs\n")+
    ggtitle("Unique Words per Song, By Genre\n")+
    xlim(c(0, 700)); g3
  
g3scale <- ggplot(data = lyrics, aes(x=uniquewords, fill=genre)) +
    theme_bw() + 
    geom_density(color="#e9ecef", alpha=0.5) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,0.017))+
    theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26))+
    labs(fill="")+
    xlab("\nNumber of Unique Words")+
    ylab("Density\n")+
    ggtitle("Unique Words per Song, By Genre (Scaled)\n")+
    xlim(c(0, 600)); g3scale
  
# compare
  
grid.arrange(g2, g3, ncol = 1)
grid.arrange(g2scale, g3scale, ncol = 1)
  
# ugly histogram
  
g4 <- ggplot(data = lyrics, aes(x=percentunique, fill=genre)) +
    theme_bw() + 
    geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', bins=20) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,600))+
    theme(plot.title=element_text(hjust=0.5), axis.text=element_text(colour="black"))+
    labs(fill="")+
    xlab("Percentage")+
    ylab("Number of Songs")+
    ggtitle("Percentage of Words That Are Unique in Different Genres")+
    xlim(c(0, 100)); g4
  
# better: ridge plot
# optional: order by median

# median_percentunique <- aggregate(percentunique ~ genre, lyrics, median)
# ridge_order <- median_percentunique[order(median_percentunique$percentunique), ]$genre
# lyrics$genre <- factor(lyrics$genre, levels = ridge_order, ordered = TRUE)

g5 <- ggplot(lyrics, aes(x = percentunique, y = genre, fill = genre)) +
      geom_density_ridges() +
      theme_ridges(center= TRUE) + 
      theme(legend.position = "none") +
      theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26))+
      xlab("\nPercentage")+
      ylab("Genre\n")+
      ggtitle("Percentage of Words That Are Unique, By Genre\n")+
      xlim(c(0, 100)); g5

################################# WORD COUNT STATISTICAL ANALYSES ######################################

options(scipen = 0) # restore default scientific notation options
lyrics <- readRDS(file="~/Documents/R/Research/lyrics_calculations.csv") # retrieve data

### TOTAL WORDS ###

# ANOVA
fit1 <- aov(lyrics$totalwords ~ factor(lyrics$genre)) 
# Summary of the analysis
summary(fit1)
# Post-hoc test
TukeyHSD(fit1) 
signif <- TukeyHSD(fit1)$`factor(lyrics$genre)`[, 4] < 0.05
names(which(signif == F)) # FIVE NONSIGNIFICANT PAIRINGS
pairwise.t.test(lyrics$totalwords, factor(lyrics$genre), 
                  p.adj = "bonf")[[3]] > 0.05 # SIX NONSIGNIFICANT PAIRINGS
length(which(pairwise.t.test(lyrics$totalwords, factor(lyrics$genre), 
                             p.adj = "bonf")[[3]] > 0.05)) # SIX NONSIGNIFICANT PAIRINGS
# Assess equal variances
bartlett.test(lyrics$totalwords ~ factor(lyrics$genre)) # equal variances assumption is violated
# Assess normality assumption
genres <- c("Folk", "R&B", "Indie") # shapiro.test() cannot handle sample sizes > 5000
# but we can investigate normality for a subset of genres
for (i in 1:length(genres)){
  print(paste("Normality test for:", genres[i]))
  subset(lyrics, genre == genres[i])$totalwords %>% shapiro.test() %>% print() # normality violated
}
# Kruskal-Wallis F-Test
kruskal.test(lyrics$totalwords ~ factor(lyrics$genre))
# Zar (2010) states that the Dunn test
# is appropriate for groups with unequal numbers of observations.
dunnTest(lyrics$totalwords ~ factor(lyrics$genre), method="bonferroni") # SEVEN NONSIGNIFICANT PAIRINGS
sum(dunnTest(lyrics$totalwords ~ factor(lyrics$genre), method="bonferroni")[[2]][,4] > 0.05) # 7
# Dr. Matthews recommends that a rank-based post-hoc test accompany the Kruskal-Wallis test
pairwise.wilcox.test(lyrics$totalwords, factor(lyrics$genre))[[3]] > 0.05 # SEVEN NONSIGNIFICANT PAIRINGS
length(which(pairwise.wilcox.test(lyrics$totalwords, factor(lyrics$genre))[[3]] > 0.05)) # 7
epsilon_sq(fit1) # genre has a relatively strong effect on total words (0.347)
  
### UNIQUE WORDS ###

# ANOVA
fit2 <- aov(lyrics$uniquewords ~ factor(lyrics$genre)) 
# Summary of the analysis
summary(fit2)
# Post-hoc test
TukeyHSD(fit2) 
signif2 <- TukeyHSD(fit2)$`factor(lyrics$genre)`[, 4] < 0.05
names(which(signif2 == F)) # TEN NONSIGNIFICANT PAIRINGS
pairwise.t.test(lyrics$uniquewords, factor(lyrics$genre), 
                  p.adj = "bonf")[[3]] > 0.05 # TEN NONSIGNIFICANT PAIRINGS
length(which(pairwise.t.test(lyrics$uniquewords, factor(lyrics$genre), 
                             p.adj = "bonf")[[3]] > 0.05)) # TEN NONSIGNIFICANT PAIRINGS
# Assess assumptions
bartlett.test(lyrics$uniquewords ~ factor(lyrics$genre)) # equal variances violated
# Kruskal-Wallis F-Test
kruskal.test(lyrics$uniquewords ~ factor(lyrics$genre))
# Zar (2010) states that the Dunn test
# is appropriate for groups with unequal numbers of observations.
dunnTest(lyrics$uniquewords ~ factor(lyrics$genre), method="bonferroni") # 8 NONSIGNIFICANT PAIRINGS
sum(dunnTest(lyrics$uniquewords ~ factor(lyrics$genre), method="bonferroni")[[2]][,4] > 0.05) # 8
# Dr. Matthews recommends that a rank-based post-hoc test accompany the Kruskal-Wallis test
pairwise.wilcox.test(lyrics$uniquewords, factor(lyrics$genre))[[3]] > 0.05 # FIVE NONSIGNIFICANT PAIRINGS
length(which(pairwise.wilcox.test(lyrics$uniquewords, factor(lyrics$genre))[[3]] > 0.05)) # 5
epsilon_sq(fit2) # genre has a relatively strong effect on unique words (0.39)
  
### Cliff's Delta as a pairwise measure of effect size
  
### TOTAL WORDS ###
  
pairings <- expand.grid(unique(lyrics$genre), unique(lyrics$genre)) # all genre pairings
  
cliff.est1 <- list() # total words
cliff.est2 <- list() # unique words
cliff.est3 <- list() # percent unique
  
for (i in 1:nrow(pairings)){
  cliff.est1[[i]] <- cliff.delta(subset(lyrics, genre == pairings[i, 1])$totalwords, 
                                subset(lyrics, genre == pairings[i, 2])$totalwords)$estimate
  }; cliff.est1
  
### UNIQUE WORDS ###
  
for (i in 1:nrow(pairings)){
  cliff.est2[[i]] <- cliff.delta(subset(lyrics, genre == pairings[i, 1])$uniquewords, 
                                subset(lyrics, genre == pairings[i, 2])$uniquewords)$estimate
  }; cliff.est2
  
### PERCENT UNIQUE WORDS ###
  
for (i in 1:nrow(pairings)){
  cliff.est3[[i]] <- cliff.delta(subset(lyrics, genre == pairings[i, 1])$percentunique, 
                                  subset(lyrics, genre == pairings[i, 2])$percentunique)$estimate
  }; cliff.est3
  
effectsizes <- cbind(pairings, unlist(cliff.est1), unlist(cliff.est2), unlist(cliff.est3))
  
# An effect size of +1.0 or -1.0 indicates the absence of overlap between two vectors 
# or distributions, whereas 0.0 means group distributions overlap completely.
# Taking the absolute value gives a statistic ranging from 0 to 1.
  
names(effectsizes) <- c("Genre1", "Genre2", "CliffsDelTotal", 
                          "CliffsDelUnique", "CliffsDelPercentUnique")
  
g6 <- ggplot(data = effectsizes, aes(Genre2, Genre1, fill = abs(CliffsDelTotal)))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "firebrick2", mid = "white", 
                         midpoint = 0, limit = c(0,1), space = "Lab", 
                         name="Magnitude\nof Cliff's\nDelta")+
    ggtitle("Difference in Number\nof Words by Genre\n")+
    theme_minimal()+xlab("\nGenre")+ylab("Genre\n")+
    theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26),
        legend.title=element_text(colour="black", size = 26))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    coord_fixed(); g6
  
g7 <- ggplot(data = effectsizes, aes(Genre2, Genre1, fill = abs(CliffsDelUnique)))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "firebrick2", mid = "white", 
                         midpoint = 0, limit = c(0,1), space = "Lab", 
                         name="Magnitude\nof Cliff's\nDelta")+
    ggtitle("Difference in Number\nof Unique Words by Genre\n")+
    theme_minimal()+xlab("\nGenre")+ylab("Genre\n")+
   theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26),
        legend.title=element_text(colour="black", size = 26))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    coord_fixed(); g7
  
g8 <- ggplot(data = effectsizes, aes(Genre2, Genre1, fill = abs(CliffsDelPercentUnique)))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "firebrick2", mid = "white", 
                         midpoint = 0, limit = c(0,1), space = "Lab", 
                         name="Magnitude\nof Cliff's\nDelta")+
    ggtitle("Difference in Percentage of\nWords that are Unique by Genre\n")+
    theme_minimal()+xlab("\nGenre")+ylab("Genre\n")+
  theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26),
        legend.title=element_text(colour="black", size = 26))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    coord_fixed(); g8
  
grid.arrange(g6, g7, g8, ncol = 3) # compare

################################# TOKENIZE, CHANGE TO NUMERIC ##########################################
  
# Split into tokens
lyrics <- readRDS(file="~/Documents/R/Research/lyrics_calculations.csv") # retrieve data
cons <- lyrics$lyrics # 'cons' for consonance
cons <- as.vector(gsub("[0-9]", "", cons)); cons[[4]] # remove numerals
{stopwords <- tolower(c("the","of","to","and","a","in","is","it","you","that","he","was","for","on","are","with","as","I","his","they","be","at","one","have","this","from","or","had","by","not","word","but","what","some","we","can","out","other","were","all","there","when","up","use","your","how","said","an","each","she",
                 "which","do","their","time","if","will","way","about","many","then","them","write","would","like","so","these","her","long","make","thing","see","him","two","has","look","more","day","could","go","come","did","number","sound","no","most","people","my","over","know","water","than","call","first","who","may","down","side","been","now","find"))} # declare list of 100 stopwords (these comprise half of all English communication)
for (i in 1:length(stopwords)){
  cons <- gsub(paste0('\\b', stopwords[i], '\\b'), "0", cons)} # rm stopwords (match whole words only)
cons <- as.vector(gsub("[ ]+", " ", cons)) # remove duplicate spaces
cons <- as.vector(strsplit(cons, " ")) # split into tokens
cons[[123456]]; lyrics[123456, 6] # compare

# Remove contiguous duplicated elements from temp
# This ensures repetitive motifs like "oh oh oh oh" will not be treated as consonance
#cons <- rapply(cons, function(cons)
#{cons[c(TRUE, !cons[-length(cons)] == cons[-1])]}, # remove contiguous duplicated words
#how="list")

# Remove ALL duplicated words, replace with 0

for(i in 1:length(cons)){
  cons[[i]][duplicated(cons[[i]])] <- "0"
}; cons[[123456]]

cons[[123456]]; lyrics[123456, 6] # compare

## Extract first letter
dat <- rapply(cons, function(cons){substring(cons, 1, 1)}, how="list"); dat[[4]]

# Encode a-z as 1-26
dat <- rapply(dat, function(dat){as.vector(gsub("a", "1", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("b", "2", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("c", "3", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("d", "4", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("e", "5", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("f", "6", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("g", "7", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("h", "8", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("i", "9", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("j", "10", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("k", "11", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("l", "12", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("m", "13", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("n", "14", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("o", "15", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("p", "16", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("q", "17", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("r", "18", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("s", "19", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("t", "20", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("u", "21", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("v", "22", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("w", "23", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("x", "24", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("y", "25", dat))}, how="list")
dat <- rapply(dat, function(dat){as.vector(gsub("z", "26", dat))}, how="list")

# Drop entries that are not encoded as digits
dat <- rapply(dat, function(dat){subset(dat, grepl("[0-9]", dat)==T)}, how="list")

# saveRDS(dat, file="~/Documents/R/Research/cons_remove_dups.csv")

####################### PROGRAMMATICALLY DETECT ALLITERATION ###########################################

dat <- readRDS(file="~/Documents/R/Research/cons_remove_dups.csv") # tokenized, changed to numeric
lyrics <- readRDS(file="~/Documents/R/Research/lyrics_calculations.csv") # retrieve data

# We removed stopwords and duplicate words: now we arbitrarily restrict
# analysis to songs with > 75 total words and > 30 unique words (to do: add poetry as a 'control')

dat <- dat[which(lyrics$uniquewords > 50 | lyrics$totalwords > 75)] # 204562 songs

# Elucide multiple instances of NLP elements as a matrix (EMINEM)

eminem <- function(N, songlyrics) {
  songlyrics <- as.numeric(songlyrics == N)
  rl <- rle(songlyrics == 0)
  i1 <- rl$lengths>=7 & rl$values
  conslst <- split(songlyrics, rep(cumsum(c(TRUE, i1[-length(i1)])), rl$lengths))
  conslst <- Filter(function(x) {sum(x) > 1}, conslst); conslst
  conslst <- lapply(conslst, function(x) x[seq_len(tail(which(x != 0), 1))])
  
  if (length(conslst) == 0){
    vec <- rep(0, 10) # change this number if needed
  } else {
    
    repeats <- list()
    for (i in 1:length(conslst)){
      x <- conslst[[i]]
      diffs <- x[-1L] != x[-length(x)]
      idx <- c(which(diffs), length(x))
      repeats[[i]] <- sort(subset(diff(c(0, idx)), 
                                  (x[idx]==0)==F), decreasing = T)
      remove(x); remove(diffs); remove(idx)
    }
    
    vec <- c(
      sum(repeats == "2"),                  # doublet
      sum(repeats == "3"),                  # triplet
      sum(repeats == "4"),                  # quartet
      sum(repeats == "c(1, 1)"),					  # doublet of singlet
      sum(repeats == "c(1, 1, 1)"), 				# triplet of singlet
      sum(repeats == "c(1, 1, 1, 1)"), 			# quartet of singlet
      sum(repeats == "c(2, 1)"), 					  # doublet with singlet
      sum(repeats == "c(2, 1, 1)"), 				# doublet with doublet of singlet
      sum(repeats == "c(3, 1)"), 				    # triplet with singlet
      sum(rapply(repeats, function(x){sum(unlist(x))}, how = "list") >= 5) # complex 			  
    )/length(songlyrics) # weighted by total words
  }
  return(vec)
}

# Counting different classes of alliteration, but keeping separate simple units (doublets, doublets of singlets)
# and more complex combinations of these

# There are 14/21 consonants in the English language that usually have only one sound no matter where they
# appear in a word, and a few more that usually only have one sound when positioned at the beginning of a word
# we will investigate a subset of these, omitting not-common letters (like "j")

b <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "b"), x))), byrow=T, nrow=length(dat)) # letter b
d <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "d"), x))), byrow=T, nrow=length(dat)) # letter d
f <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "f"), x))), byrow=T, nrow=length(dat)) # letter f
h <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "h"), x))), byrow=T, nrow=length(dat)) # letter h
l <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "l"), x))), byrow=T, nrow=length(dat)) # letter l
m <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "m"), x))), byrow=T, nrow=length(dat)) # letter m
n <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "n"), x))), byrow=T, nrow=length(dat)) # letter n
p <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "p"), x))), byrow=T, nrow=length(dat)) # letter p
r <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "r"), x))), byrow=T, nrow=length(dat)) # letter r
v <- matrix(unlist(lapply(dat, function(x) eminem(which(letters == "v"), x))), byrow=T, nrow=length(dat)) # letter v

#hmm <- cbind.data.frame(lyrics[which(lyrics$uniquewords > 50 | lyrics$totalwords > 75),]$genre, 
#                        b, d, f, h, l, m, n, p, r, t)
#colnames(hmm) <- c("genre", paste("V", 1:100, sep=""))
#fingerscrossed <- aggregate(. ~ genre, hmm, mean)
#pca <- prcomp(fingerscrossed[,2:101], scale=T, center=T, retx=T)
#plot(pca$x[,1:2], type = "n") # mysterious pca
#text(pca$x[,1], pca$x[,2], as.character(fingerscrossed[,1]))

avg <- (b + d + f + h + l + m + n + p + r + v)/10

avg[,1] <- avg[,1] + avg[,7] + avg[,8]
avg[,2] <- avg[,2] + avg[,9] 

avg <- cbind.data.frame(lyrics[which(lyrics$uniquewords > 50 | lyrics$totalwords > 75),]$index,
                        lyrics[which(lyrics$uniquewords > 50 | lyrics$totalwords > 75),]$artist,
                        lyrics[which(lyrics$uniquewords > 50 | lyrics$totalwords > 75),]$genre, avg)
colnames(avg) <- c("index", "artist", "genre", paste0("V", 1:10))
genremeans <- aggregate(. ~ genre, avg, mean)
artistmeans <- aggregate(. ~ artist, avg, mean)

# songs with high consonance scores

head(avg[order(avg$V10, decreasing = T), ], 10)
subset(lyrics, index == "322541") # blackalicious alphabetic aerobics
subset(lyrics, index == "322558") # blackalicious alphabetic aerobics remix
subset(lyrics, index == "308993") # short song
subset(lyrics, index == "273739") # a tirade against mcdonald's?
subset(lyrics, index == "293185")

killersongs <- cbind.data.frame(avg$index, avg$genre, avg$artist, rowSums(scale(avg[,4:13])))
colnames(killersongs) <- c("index", "genre", "artist", "cscore")
head(killersongs[order(killersongs$cscore, decreasing = TRUE), ], 10)

subset(lyrics, index == 38372) # I can't

for (i in 1:10){
  print(subset(lyrics, index == killersongs[order(killersongs$cscore, decreasing = TRUE)[i], ]$index))
}

genrecscore <- aggregate(cscore ~ genre, killersongs, mean)
genrecscore <- genrecscore[order(genrecscore$cscore, decreasing = TRUE), ]

genrecscore$genre <- factor(genrecscore$genre, levels = genrecscore$genre[order(genrecscore$cscore, decreasing=TRUE)]) # enforce order

ggplot(data = genrecscore,
       aes(x = genre, y = cscore)) +
  theme_bw()+
  geom_bar(stat="identity", position=position_dodge(), fill="black")+
  ggtitle("Consonance Score, By Genre", subtitle = "Consonance Score was Calculated by Adding Together \nScaled Measures of 10 Different Kinds of Consonance\n")+
  theme(plot.title=element_text(hjust=0.5, size = 42), 
        plot.subtitle=element_text(hjust=0.5, size = 34), 
        axis.text=element_text(colour="black", size = 32),
        axis.title=element_text(colour="black", size = 38))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  xlab("\nGenre")+
  ylab("Consonance Score\n")

artistcscore <- aggregate(cscore ~ artist, killersongs, mean)

metalcscore <- subset(killersongs, genre == "Metal")[order(subset(killersongs, genre == "Metal")$cscore, decreasing = T),]

for (i in 1:10){print(subset(lyrics, index == metalcscore$index[i]))}

hiphopcscore <- subset(killersongs, genre == "Hip-Hop")[order(subset(killersongs, genre == "Hip-Hop")$cscore, decreasing = T),]

for (i in 1:10){print(subset(lyrics, index == hiphopcscore$index[i]))}

ggplot(data = killersongs, aes(x=cscore, fill=genre)) +
  theme_bw() + 
  geom_density(color="#e9ecef", alpha=0.5, position = 'identity') +
  #scale_y_continuous(breaks=scales::pretty_breaks(n=10), expand=c(0,0),limits=c(0,11000))+
  theme(plot.title=element_text(hjust=0.5, size = 32), 
        axis.text=element_text(colour="black", size = 26),
        axis.title=element_text(colour="black", size = 30),
        legend.text=element_text(colour="black", size = 26))+
  labs(fill="")+
  xlab("\nNumber of Words")+
  ylab("Number of Songs\n")+
  ggtitle("Total Words per Song, By Genre\n")+
  xlim(c(0, 25))

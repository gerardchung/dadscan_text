# News Dads from Lexis
##########################

# Aim: To extract data from raw text files and convert into dataframe

# Note:
# Total 16075 documents
# Removed dups in titles: 365
# Removed dups in text: 2
# Final N: 15708

rm(list = ls())

#library(renv)
library(dplyr)
library(stringr)
library(textreadr)
library(janitor)
library(here)

getwd()


# Extract file names
####################
file.list = list.files("source/news/data", recursive = T)

file.list[1:10]


# Remove file names with _doclist.docx
str_view_all(file.list, regex(pattern = "_doclist.docx", ignore_case = T))
    # need ‘htmltools’ and "htmlwidgets"

n <- str_detect(file.list, regex(pattern = "_doclist.docx", ignore_case = T))
sum(n) # 161 because 161 folders with this file name
rm(n)

file.list <- str_subset(file.list, regex(pattern = "_doclist.docx", ignore_case = T), negate = T) 
    # remove _doclist 
n <- str_detect(file.list, regex(pattern = "_doclist.docx", ignore_case = T))
sum(n) # 0 because all removed
rm(n) 

# Folder number
str_view_all(file.list[100:120], regex(pattern = "^\\d{1,5}"))
foldernum <- str_extract(file.list, regex(pattern = "^\\d{1,5}"))
unique(foldernum)

# create full file paths 
file.list[1:5]
file.list2 <- paste("source/news/data", file.list, sep = "/")
file.list2[16077]

# Extract information 
#####################

# trial with one document
# ======================
trial = read_docx(file.list2[1])
trial
trial = read_docx(file.list2[3])
trial
file.list2[2]
file.list2[1]

str_detect(file.list2, pattern = "I did my best")
str_view_all(file.list2, pattern = "I did my best", match = T)

which(file.list2 == "source/news/data/150/'I did my best' The following is an extract from the book Lee Kuan Yew_ The Man And His Ideas, publi.docx")

file.list2[5700]
file.list2[5701]
file.list2[5702]
file.list2[5705]
trial = read_docx(file.list2[2000])
trial = read_docx(file.list2[5702]) 
    # this article unlike 2000 does not have classification
    # Thus it cannot grab the text from Body to Classification
    # The only indicator is Load-Date:
    # But Load-Date: has different dates after the : for different documents
    # Thus, in below, i have to use str_detect to detect Load-Date regardless of dates 


#  text body
# see that the body of news starts at line after "Body: and end at line before "Classification"
start.text = which(trial == "Body") + 1
    # end.text   = which(trial == "Classification") - 1


end.text = ifelse(length(trial[grepl("Classification",trial,fixed=T)]) >0,
                  which(trial == "Classification") - 1,
                  # which(trial == "Load-Date:*") - 1
                  which(str_detect(trial,"Load-Date:")) - 1) 
        # # Some articles do not have Classification (thus cannto extract body text)


text = paste(trial[start.text:end.text], collapse = "\n")
cat(text, "\n")



            

# Next, let's grab each of the options that has an explicit tag.

(section = gsub("Section:","",trial[grepl("Section:",trial,fixed=T)] ,fixed=T))
# grepl() will find in trial the pattern = "Section" and return a logical vector.
# If true, then trial[ ] will take out that entire string value
# gsub will remove remove from that string value "Section:" and replace with "" (blank)
(section = str_replace(trial[str_detect(trial, "Section:")], pattern = "Section:", replacement =""))
# Need to first detect because the trial is in a vector
(words = gsub("Length:","",trial[grepl("Length:",trial,fixed=T)] ,fixed=T))
(language = gsub("Language:","",trial[grepl("Language:",trial,fixed=T)],fixed=T))

(type = str_replace(trial[str_detect(trial, "Publication-Type:")], pattern = "Publication-Type:", replacement =""))
# trial[1] has a section on "Publication-Type:" 
# trial[3] does not have. So if just run codes withOUT ifelse, the type vector will have no data.
# This will become a problem in the loop later (the loop will stop because entering NO data into the dataframe)
# This ifelse code => if length is > 0, then ran the gsub. Else, input NA 
(type = ifelse(length(trial[grepl("Publication-Type:",trial,fixed=T)]) >0,
               gsub("Publication-Type:","",trial[grepl("Publication-Type:",trial,fixed=T)],fixed=T),
               NA))
(subject = gsub("Subject:","",trial[grepl("Subject:",trial,fixed=T)],fixed=T))
(industry = gsub("Industry:","",trial[grepl("Industry:",trial,fixed=T)],fixed=T))
(geographic = gsub("Geographic:","",trial[grepl("Geographic:",trial,fixed=T)],fixed=T))
# (load.date = gsub("Load-Date:","",trial[grepl("Load-Date:",trial,fixed=T)],fixed=T))

# These below are relational -> they should be the same position in every docu
(title = trial[1])
(source = trial[2])
(pub.date = trial[3])



# Step 1 here is to create the empty data frame.

news <- data.frame( title = rep(NA, length(file.list2)),
                       source = rep(NA, length(file.list2)),
                       pub.date = rep(NA, length(file.list2)),
                       section = rep(NA, length(file.list2)),
                       words = rep(NA, length(file.list2)),
                       language = rep(NA,length(file.list2)),
                       type = rep(NA,length(file.list2)),
                       subject = rep(NA,length(file.list2)),
                       industry = rep(NA,length(file.list2)),
                       geographic = rep(NA,length(file.list2)),
                       text = rep(NA,length(file.list2)),
                       stringsAsFactors = F
)


# Step 2 is to create the loop by copying down the code we know extracts what we want and has it input it into our data frame.
for(i in 1:length(file.list2)) {
    print(paste("Working on document", i, "of", length(file.list2)))
    temp.doc = read_docx(file.list2[i])
    
    news$title[i] = temp.doc[1]
    news$source[i] = temp.doc[2]
    news$pub.date[i] = temp.doc[3]
    
    #news$section[i] = gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T)
    # there are article(s) that do not have sections -> trial[8]
    news$section[i] = ifelse(length(temp.doc[grepl("Section:",temp.doc,fixed=T)]) >0,
                                gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T),
                                NA)
    news$words[i] = gsub("Length:","",temp.doc[grepl("Length:",temp.doc,fixed=T)] ,fixed=T)
    
   # news$language[i] = gsub("Language:","",temp.doc[grepl("Language:",temp.doc,fixed=T)] ,fixed=T)
    
    news$language[i] = ifelse(length(temp.doc[grepl("Language:",temp.doc,fixed=T)]) >0, 
                          gsub("Language:","",temp.doc[grepl("Language:",temp.doc,fixed=T)] ,fixed=T),
                          NA)
    
    
    # news$type[i] = gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T)
    # there are article(s) that do not have sections -> trial[3]    
    news$type[i] = ifelse(length(temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)]) >0, 
                             gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T),
                             NA)
    
    #news$subject[i] = gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T)
    news$subject[i] = ifelse(length(temp.doc[grepl("Subject:",temp.doc,fixed=T)] >0),
                                gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T),
                                NA)
    
    #news$industry[i] = gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T)
    # at least one article 4 does not have industry
    news$industry[i] = ifelse(length(temp.doc[grepl("Industry:",temp.doc,fixed=T)] >0),
                                 gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T),
                                 NA)
    
    # news$geographic[i] = gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T)
    news$geographic[i] = ifelse(length(temp.doc[grepl("Geographic:",temp.doc,fixed=T)] >0),
                                   gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T),
                                   NA)
    
    start.text = which(temp.doc == "Body") + 1
   # end.text   = which(temp.doc == "Classification") - 1

    end.text = ifelse(length(temp.doc[grepl("Classification",temp.doc,fixed=T)]) >0,
                      which(temp.doc == "Classification") - 1,
                      which(str_detect(temp.doc,"Load-Date:")) - 1) 
    
       
#    end.text = ifelse(length(trial[grepl("Classification",trial,fixed=T)]) >0,
#                      which(trial == "Classification") - 1,
#                      # which(trial == "Load-Date:*") - 1
#                      which(str_detect(trial,"Load-Date:")) - 1) 
    # # Some articles do not have Classification (thus cannto extract body text)
    
    
    news$text[i] = paste(temp.doc[start.text:end.text], collapse = "\n")
}


# Add in the vars that denote folder number
news$foldernum <- foldernum
glimpse(news)

news <- 
    news %>% 
    relocate(foldernum, .before = title )
news$foldernum <- as.numeric(news$foldernum)

# Remove duplicates

## titles
#dup <- news_covidsg %>% duplicated()
dup <- news[duplicated(news$title), ] 
NROW(dup) # 365 duplicates in title

chk_dup <- news %>% select(foldernum, title) %>% get_dupes(title)

news <- news[!duplicated(news$title), ] # 16075 - 365 = 15710 

chk_dup <- news %>% select(foldernum, title) %>% get_dupes(title)


## text
dup <- news[duplicated(news$text), ] 
NROW(dup) # 2 duplicates in text
chk_dup_text <- news %>% select(foldernum, text) %>% get_dupes(text)

news <- news[!duplicated(news$text), ] # 15710 - 2 = 15708 
NROW(news) # N=15708


# Create identifier id1 
glimpse(news)

news <- 
    news %>% 
    mutate(id1 = row_number())

news <- news %>% 
    relocate(id1, .after = foldernum )

glimpse(news)
tabyl(news$id1)


# save as Rdata file
# ====================
getwd()
save(news, file = here("cr_data", "news", "news_extracted.RData")) 

#load( file = "cr_data/news_corporal_extracted.RData") 




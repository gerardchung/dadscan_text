# TEXT-MING NEWS FOR DADSCAN

# Note:
# Total 16075 documents
# Removed dups in titles: 365
# Removed dups in text: 2
# Final N: 15708 in the data loaded before cleaning

# During Cleaning:
# 3 articles removed for Dates (2 artickes in 1970 and 1 article has no date)
# Total N = 15708-3 = 15705

rm(list = ls())

# LOAD PACKAGES ####
pacman::p_load(dplyr,stringr, ggplot2, janitor, tidyverse, quanteda, tidytext, here) 


# LOAD DATASET ####

getwd()
load(file = here("cr_data", "news", "news_extracted.RData")) 


# DATE ####
## pub.date 
# Extract year, month, and day from pub.data
# There is one document with no date info
# There are two documents in 1970 in Todayonline which is not possible since TodyOnline was not published then

head(unique(news$pub.date), n = 40)
tail(unique(news$pub.date), n = 40)

# One document does not have a data value 
    # id1 = 10646
NROW(news)
news <-
    news %>% 
    filter(pub.date != "Correction Appended") %>% 
    filter(pub.date != "January 1, 1970 Thursday")

## month ====
months <- c("January", "February", "March", "April", 
            "May", "June", "July", "August", 
            "September", "October", "November", "December")

for (i in months) {
    news$pub_month <- str_extract(news$pub.date, 
                                     regex(pattern = paste(months, collapse = "|"), ignore_case = T))
}

head(news$pub_month, n = 10)
head(news$pub.date, n = 10)
table(news$pub_month)

news$pub_month <- factor(news$pub_month, levels = months)
janitor::tabyl(news$pub_month)

## year ====
head(news$pub.date, n = 50)
#str_view(news$pub.date, pattern = "\\d{4}")

news$pub_year <- str_extract(news$pub.date, pattern = "\\d{4}")
head(news$pub_year, n =10)
table(news$pub_year)


## day ====
head(news$pub.date, n = 50)
#str_view(news$pub.date, pattern = "\\d{1,2}")
#str_view(news$pub.date, pattern = "\\d+") # this also work

news$pub_day <- str_extract(news$pub.date, pattern = "\\d{1,2}")
news$pub_day <- as.numeric(news$pub_day)
table(news$pub_day)

#View_date <- select(news,starts_with("pub") )
#rm(View_date)

# Check on day variable [detect if errors for e.g. day > 31 days]
for (value in news$pub_day) {
    if (value > 31) {
        print(str_c(news$id1 ," has", " problem!"))
    } else {
        print("No problem, move on!")
    }
}

table(news$pub_day, exclude = F)
table(news$pub_year,exclude = F)
table(news$pub_month, exclude = F)

## Convert to DATA using lubricate ====
date.lub <- c("pub_year","pub_month", "pub_day")
news$pub_date <- lubridate::ymd(paste(news$pub_year, news$pub_month,news$pub_day))
class(news$pub_date)

news <- news %>% 
    relocate(starts_with("pub_"), .after = source) %>% 
    select(-(pub.date))

news %>% tabyl(pub_month)
news %>% tabyl(pub_year) # why 1999 and 2000 only 46 and 47 articles?
news %>% tabyl(pub_month)

# news %>% ggplot(aes(pub_date)) + geom_freqpoly(binwidth = 30) # R for data sci book pp.241

# SECTION ####

## Remove non-Home or non-Singapore or non-FORUM =====
#unique(news$section)
#tabyl(news$section)


#str_view_all(news$section, regex(pattern = "Malaysia", ignore_case = T), match = T)

# ## Remove:
# 
# NROW(news) #2442 
# news <-
#     news %>% 
#     filter(!str_detect(section, regex(pattern = "Malaysia|East Asia|Asia|Asean|Asean;|Business|CORPORATE NEWS|DIGITAL|EAST & SE ASIA|ECONOMY|Football|Formula One|World|Money|STYLE|Companies & Markets|News Focus; What it should have been; Pg. 2; Correction|SPORT|Tech & Science|sweat", ignore_case = T)) | is.na(section)) 
# 

# NROW(news) #2442 - 2012 = 430 removed; N = 2012
# tabyl(news$section)


## Create a forum variable ====
    # see if can distinguish forum section
    # In today news, it is called "voices"
    # Create a variable that indicates "forum letters"

# unique(news$section)
# 
# news$voice <- str_detect(news$section, regex(pattern = "voice|view|opinion", ignore_case =T))
# sum(news$voice, na.rm = T) # n = 36
# # view_voice <- filter(news, voice == T)
# # view_voice[2] # Only "voice" are forum letters
# # 
# # rm(view_voice)
# 
# ### detect for pattern in section with either word "forum" or "letter
# news$section_forum <- str_detect(news$section, regex(pattern = "forum|letter|voice", ignore_case =T))
# news$section_forum <-    as.numeric(news$section_forum) # change from logical to numeri
# 
# table(news$section_forum, exclude = F)
# 
# ###  detect for a new pattern in text since "i refer" and "i commend" in text => forum letters
# pattern.refer.commend = "I commend|We commend|I refer|We refer|with reference"
# section1 <- news[str_detect(news$text, regex(pattern = pattern.refer.commend, ignore_case =T)),]
# View_section1 <- section1 %>% select(text, section, section_forum)
# # we can replace the empty values with T or 1 if there is this pattern in the text
# # note that section = Voice can also be a forum letter
# # six articles are detected using this new pattern
# rm(section1, View_section1)
# 
# forum_pattern1 <- str_detect(news$text, regex(pattern = pattern.refer.commend, ignore_case =T))
# table(news$section_forum, exclude = F)
# 
# news <- news %>% 
#     mutate(section_forum = replace(section_forum, forum_pattern1 == T, 1))
# 
# table(news$section_forum, exclude = F) # 183 are forum/ letters
# 
# table(news$section_forum)
# news$section_forum <- factor(news$section_forum, labels = c("articles", "forum/letters"))
# table(news$section_forum)
# 

# SOURCE ####
    # Create a news source variable 
unique(news$source)
news <- news %>% mutate(source = replace(source, source == ("Today (Singapore) - Online") , "TODAY"))
news <- news %>% mutate(source = replace(source, source == ("TODAY (Singapore)") , "TODAY"))         

news$source <-  replace(news$source, news$source =="The Straits Times (Singapore)" , "ST")
news$source <-  replace(news$source, news$source =="The Straits Times" , "ST")

news$source <-  replace(news$source, news$source =="Channel NewsAsia" , "CNA")

#news$source <-  replace(news$source, news$source =="Business Times (Singapore)" , "Business Times")
#news$source <-  replace(news$source, news$source =="The Business Times Singapore" , "Business Times")

unique(news$source)
news$source <- factor(news$source)
table(news$source)
tabyl(news$source)


# TITLE #### 

# 
# 
# # # FILTER OUT OTHER IRRELEVANT ONES BASED ON EYEBALLING ####
# # # likely those irrelevant ones will have missing values for section var
# # irrelevant <-
# #     news_corporal %>% 
# #     select(foldernum, id, title, text, section) 
# # 
# # irrelevant <-
# #     irrelevant %>% 
# #     filter(title == "Maid jailed for sex with 14-year-old boy" | 
# #                title == "Inside" |
# #                title == "DISCOVER THE SIM UNIVERSITY ADVANTAGE" |
# #                title == "Re-skilling of workers for future a priority: ILO chief; Inequalities in labour skills already affecting job creation and economic growth, he says" |
# #                title == "20 S'poreans on first Asean Achievement Awards shortlist" |
# #                title == "5 THINGS YOU CAN DO TODAY" |
# #                title == "... and the week ahead" | 
# #                title == "Class list" |
# #                title == "Happenings - Arts" |  
# #                title == "Happenings - Gigs" |
# #                title == "Life! What's On" |
# #                title == "LIFE! What's On" |
# #                title == "Mixed vibes" |
# #                title == "Get involved" | 
# #                title == "GOINGS-ON IN THE VILLAGE") %>% 
# #     select(id) 
# 
# # ## Subset out the irrelevants ====
# # NROW(irrelevant)
# # 
# # news_corporal <-
# #     news_corporal %>% 
# #     anti_join(irrelevant)
# # rm(irrelevant)
# 
# # REPLACE SOCIAL_WORK TO SOCIAL WORK IN TITLE AND TEXT #####
# 
# # CHECK PHRASES ON SOCIAL WORK AND SOCIAL WORKERS IN TITLE  
# match <- str_extract_all(news_corporal$title, regex(pattern = "((social\\b) (work\\w*))",  ignore_case = T), simplify = TRUE)
# # this matches social work, social worker*
# 
# match_SOCIAL_WORK <- str_extract_all(news_corporal$title, regex(pattern = "((social\\b) (work\\b))",  ignore_case = T), simplify = TRUE)
# # this matches social work
# 
# match_SOCIAL_WORKER <- str_extract_all(news_corporal$title, regex(pattern = "((social\\b) (worker\\w*))",  ignore_case = T), simplify = TRUE)
# # this matches social worker, social workers
# unique(match)
# unique(match_SOCIAL_WORK)
# unique(match_SOCIAL_WORKER)
# 
# table(match)
# table(match_SOCIAL_WORK)
# 
# match2 <- str_extract_all(news_corporal$title , regex(pattern = "(\\w*(social-work)\\w*)|(\\w*(Social-work)\\w*)", 
#                                                 ignore_case = T), simplify = TRUE)
#     # this will match "social-work", "social -worker", "social-workprofessionals" 
# 
# unique(match2)
# 
# table(match2)
# 
# # REPLACE SOCIAL WORK WITH SOCIAL_WORK IN TITLES 
# ## SOCIAL WORK PROFESSION  
# news_corporal$title <- str_replace_all(news_corporal$title, 
#                                  regex(pattern = "((social\\b) (work\\b))",  ignore_case = T),
#                                  replacement = "social_work")
# 
# news_corporal$title <- str_replace_all(news_corporal$title, 
#                                  regex(pattern = "(\\w*(social-work)\\w*)|(\\w*(Social-work)\\w*)",  ignore_case = T),
#                                  replacement = "social_work")
# 
# 
# ## SOCIAL WORK PERSONS 
# news_corporal$title <- str_replace_all(news_corporal$title, 
#                                  regex(pattern = "((social\\b) (worker\\w*))",  ignore_case = T),
#                                  replacement = "social_worker")
# 
# 
# match_all <- str_extract_all(news_corporal$title, regex(pattern = "social_work\\w*",  ignore_case = T), simplify = TRUE)
# match <- str_extract_all(news_corporal$title, regex(pattern = "((social\\b) (work\\w*))",  ignore_case = T), simplify = TRUE)
# match_SOCIAL_WORK <- str_extract_all(news_corporal$title, regex(pattern = "((social\\b) (work\\b))",  ignore_case = T), simplify = TRUE)
# match_SOCIAL_WORKER <- str_extract_all(news_corporal$title, regex(pattern = "((social\\b) (worker\\w*))",  ignore_case = T), simplify = TRUE)
# match2 <- str_extract_all(news_corporal$title , regex(pattern = "(\\w*(social-work)\\w*)|(\\w*(Social-work)\\w*)", 
#                                                 ignore_case = T), simplify = TRUE)
# 
# table(match)
# table(match_SOCIAL_WORK)
# table(match_SOCIAL_WORKER)
# table(match2)
# 
# rm(match, match2, match_all, match_SOCIAL_WORK, match_SOCIAL_WORKER)
# 
# # CHECK PHRASES ON SOCIAL WORK AND SOCIAL WORKERS IN TEXT ####
# match <- str_extract_all(news_corporal$text, regex(pattern = "((social\\b) (work\\w*))",  ignore_case = T), simplify = TRUE)
# # this matches social work, social worker*
# 
# match_SOCIAL_WORK <- str_extract_all(news_corporal$text, regex(pattern = "((social\\b) (work\\b))",  ignore_case = T), simplify = TRUE)
# # this matches social work
# 
# match_SOCIAL_WORKER <- str_extract_all(news_corporal$text, regex(pattern = "((social\\b) (worker\\w*))",  ignore_case = T), simplify = TRUE)
# # this matches social worker, social workers
# 
# table(match)
# table(match_SOCIAL_WORK)
# table(match_SOCIAL_WORKER)
# 
# 
# match2 <- str_extract_all(news_corporal$text , regex(pattern = "(\\w*(social-work)\\w*)|(\\w*(Social-work)\\w*)", 
#                                                ignore_case = T), simplify = TRUE)
# # this will match "social-work", "social -worker", "social-workprofessionals" 
# 
# table(match2)
# 
# str_view(news_corporal$text , pattern = "Workersis" , match = T) # To change Workersis to workers is  
# str_view(news_corporal$text , pattern = "social working" , match = T) 
# # the first instane: replace social working with social worker
# # second instance: anti-social working hours = do not replace
# # third instance: social working groups where working groups is the noun and social is adjective 
# str_view(news_corporal$text , pattern = "Social Works" , match = T) 
# # NUS Department of Social Works = replace with social work



# 
# # REPLACE SOCIAL WORK WITH SOCIAL_WORK IN TEXT ####
# 
# 
# ## SOCIAL WORK PROFESSION  
# news_corporal$text <- str_replace_all(news_corporal$text, 
#                                 regex(pattern = "((social\\b) (work\\b))",  ignore_case = T),
#                                 replacement = "social_work")
# 
# news_corporal$text <- str_replace_all(news_corporal$text, 
#                                 regex(pattern = "(\\w*(social-work)\\w*)|(\\w*(Social-work)\\w*)",  ignore_case = T),
#                                 replacement = "social_work")
# 
# 
# ## SOCIAL WORK PERSONS 
# news_corporal$text <- str_replace_all(news_corporal$text, 
#                                 regex(pattern = "((social\\b) (worker\\w*))",  ignore_case = T),
#                                 replacement = "social_worker")
# 
# 
# match_all <- str_extract_all(news_corporal$text, regex(pattern = "social_work\\w*",  ignore_case = T), simplify = TRUE)
# match <- str_extract_all(news_corporal$text, regex(pattern = "((social\\b) (work\\w*))",  ignore_case = T), simplify = TRUE)
# match_SOCIAL_WORK <- str_extract_all(news_corporal$text, regex(pattern = "((social\\b) (work\\b))",  ignore_case = T), simplify = TRUE)
# match_SOCIAL_WORKER <- str_extract_all(news_corporal$text, regex(pattern = "((social\\b) (worker\\w*))",  ignore_case = T), simplify = TRUE)
# match2 <- str_extract_all(news_corporal$text , regex(pattern = "(\\w*(social-work)\\w*)|(\\w*(Social-work)\\w*)", 
#                                                ignore_case = T), simplify = TRUE)
# 
# unique(match)
# unique(match_SOCIAL_WORK)
# unique(match_SOCIAL_WORKER)
# unique(match2)
# 
# rm(match, match_all, match_SOCIAL_WORK, match_SOCIAL_WORKER, match2)
# 


# SELECT VARIABLES I WANT #####
names(news)


NROW(news) # N = 15682

# Remove duplicates

## in title
#dup <- news_covidsg %>% duplicated()
dup <- news[duplicated(news$title), ] 
NROW(dup) # 0 duplicates in title
rm(dup)
## in text
dup <- news[duplicated(news$text), ] 
NROW(dup) # 0 duplicates in text
rm(dup)

#news_corporal <- news_corporal[!duplicated(news_corporal$title), ] 
NROW(news) # N = 15705



# CREATE ANOTHER ID VAR ####
# id1 is originally created from the extracted data in the extract do file
# this id when the data has not been cleaned and irrelevant doc removed in this current do file 
# so i create a new id2 for this analytical datae

# news <- news %>%  mutate(id2 = row_number(), .after = id1)
# max(news$id2)
# max(news$id1)
# range(news$id2)



# Save for any future analyses
##################################
getwd()
news_gerard <- news
save(news_gerard, file = here("cr_data", "news", "news_gerard.RData"))

openxlsx::write.xlsx(news_gerard, file = here("cr_data", "news", "news_gerard.xlsx"))


n_phrase <- 
    news_gerard %>% 
    mutate(n = 
    str_detect(text, regex(pattern = 
                            "Fathers|dad|dads|father*|patern|paternal|paternity|stepdad*|stepfather*|step-dad*|Step-father*|papa", 
                            ignore_case = TRUE)))

n_phrase %>% tabyl(n)
n_phrase %>% tabyl(pub_year)


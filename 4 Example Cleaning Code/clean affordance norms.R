##This code is adapted from Buchanan et al. 2019's primer on processing feature production norms
#https://link.springer.com/article/10.1007/s10339-019-00939-6

####Set up####
##load packages #get these from packages script
pckgs = c('dplyr', 'here', 'hunspell', 'tidytext', 'stringi', 'stringr',
          'koRpus', 'koRpus.lang.en', 'tokenizers', 'textstem', 'udpipe', 'stopwords', 'ez')

#check if package is installed. If yes, load. If not, install then load.
for (i in 1:length(pckgs)) {
  if (!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}

##read in data
master = read.csv("Data/Sample.csv", stringsAsFactors = F) #path currently loads from the 'Data' folder. Rename as needed.

##only keep the columns we need
dat = master[ , c(1, 5, 11, 13, 19, 33)] #Drop all columns except username, Trial number, Condition number, Cue, trial type, and response

#note that response may is listed as response.JOL. This is because I repurposed the JOL trial type to make the affordance norm trail type.
#let's rename that column!
colnames(dat)[6] = "affordance_response" 

#make blank cells NA
dat$affordance_response[dat$affordance_response == ""] = NA
dat$affordance_response[dat$affordance_response == "n/a"] = NA

##normalize all responses to lowercase
dat$affordance_response = tolower(dat$affordance_response)

source("Remove idk.R") #this reads in a script which removes "I don't know" or "not sure" or "unfamilir with word" type respones

#Check for NAs
table(is.na(dat$affordance_response))

#remove nas
dat = na.omit(dat)

####Spell check and Remove White Space####
##Spelling
#Extract a list of words
#tokens = unnest_tokens(tbl = dat, output = token, input = affordance_response)
parsed_afforances = unnest_tokens(tbl = dat, output = parsed,
                       input = affordance_response, token = "regex",
                       pattern = ", ")

parsed_afforances = unnest_tokens(tbl = parsed_afforances, output = parsed,
                                  input = parsed, token = "regex",
                                  pattern = ",") #doing this twice. Once for double space after comma, and once for single space after comma

#extract unique responses for spell checking
wordlist = unique(parsed_afforances$parsed)

#Run the spell check
spelling.errors = hunspell(wordlist)
spelling.errors = unique(unlist(spelling.errors))
spelling.sugg = hunspell_suggest(spelling.errors, dict = dictionary("en_US"))

#Pick the first spelling suggestion
spelling.sugg = unlist(lapply(spelling.sugg, function(x) x[1]))

#manually check errors
spell_check = cbind(spelling.sugg, spelling.errors)

#Write to file and manually confirm
write.csv(spell_check, file = "spell_check_raw.csv", row.names = F) ##open excel and manually check that spelling suggestions are correct. Be sure to save changes!

#read back in the checked output
spell_check = read.csv("spell_check_raw.csv", stringsAsFactors = F)
spelling.sugg = as.list(spell_check$spelling.sugg)

#get the correct number of observations and make a spelling dictionary
spelling.errors = as.data.frame(spelling.errors)

spelling.dict = (merge(spelling.errors, spell_check, by = 'spelling.errors'))

spelling.dict$spelling.sugg = tolower(spelling.dict$spelling.sugg)

spelling.dict$spelling.pattern = paste0("\\b", spelling.dict$spelling.errors, "\\b")

##Remove white spaces and replace misspellings
parsed_afforances = parsed_afforances[!parsed_afforances$parsed =="", ]

parsed_afforances = unnest_tokens(tbl = parsed_afforances, output = parsed,
                                  input = parsed, token = "regex",
                                  pattern = ",")

parsed_afforances$corrected = stri_replace_all_regex(str = parsed_afforances$parsed,
                                           pattern = spelling.dict$spelling.pattern,
                                           replacement = spelling.dict$spelling.sugg,
                                           vectorize_all = FALSE)

#Fix column names
colnames(parsed_afforances)[6:7] = c("affordance_parsed", "affordance_corrected")

##Write spelled checked data to .csv
write.csv(parsed_afforances, file = "spell_checked.csv", row.names = F)

####Lemmatization####
dat = read.csv("spell_checked.csv", stringsAsFactors = F)

#extract updated tokens
tokens = unnest_tokens(tbl = dat, output = word, input = affordance_corrected)
cuelist = unique(tokens$Stimuli.Cue)

##okay, I think this does what I want.
dat$affordance_lemma = lemmatize_strings(dat$affordance_corrected)

#remove spaces from the middle of words
dat$affordance_corrected = stringr::str_remove_all(dat$affordance_corrected, " ")

##Remove stop words
no_stop = tokens %>%
  filter(!grepl("[[:punct:]]", word)) %>% #Remove punctuation
  filter(!word %in% stopwords(language = "en", source = "snowball")) %>% #remove stopwords
  filter(!grepl("[[:digit:]]+", word)) %>% #remove numbers
  filter(!is.na(word))

####Lemmatize w/ R####
#Lemmatize! (This gives a second set of lemmas using a different algorithm. Also provides part of speech info)
lemmatized = udpipe(no_stop$word, "english")

#If lemmatized and no_stop don't match up perfectly, can use the code below to see where the differences are
#lemmatized$token[!lemmatized$token %in% no_stop$affordance_corrected] #Then just tweak the character removal process above as needed

##Combine datasets and add in second set of Lemmas, part of speech info
combined = cbind(no_stop, lemmatized[ , c(10:11, 13)])

#Give useful column names
colnames(combined)[9] = c("POS")

#Drop unused columns
combined = combined[ , -c(2, 10)]

#don't know why it keeps changing dry to spin-dry though. Going to manually fix that.
combined$lemma[combined$lemma == "spin-dry"] = "dry"
combined$lemma[combined$lemma == "smoothy"] = "smoothie"

#might be a good idea to open this up in excel and spot check. Some of the lemmas are weird. Control r in excel will be your friend (find and replace)

##Write to .csv for POS checking. You will need to manually confirm that udpipe tagged each word correctly (its a bit wonky sometimes)
#write.csv(combined, file = "Sample Cleaned.csv", row.names = F) #Uncomment at start of line to write to file!

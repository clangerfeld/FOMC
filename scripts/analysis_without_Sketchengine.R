################################################################################
# Alternative script: Do the calculations without using the Sketchengine API
################################################################################
require(quanteda)
require(dplyr)
require(rvest)
require(xml2)
require(readr)
require(stringr)

load("~/Dropbox/FOMC/new_analysis/data/data_interruptions_laughter.Rdata")

################################################################################
# (0) Define some functions
################################################################################

# (0.1) Define a function to extract the meeting date from the file name
get.meeting.date <-function(file){
  gsub(".+?([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}).txt", "\\1",file) %>%
    as.Date()
}

# (0.2) Define a function that finds the chair for a given meeting
get.chair <- function(date){
  if(date>="1987-08-18"&&date<="2006-01-31"){chair <- "Greenspan"}
  if(date>="2006-02-01"&&date<="2014-01-31"){chair <- "Bernanke"}
  if(date>="2014-02-03"&&date<="2018-02-03"){chair <- "Yellen"}
  return(chair)
}


files <- list.files("~/Dropbox/FOMC/FOMC_transcripts/sketchengine/update_oct_23",
                    full.names = T,
                    pattern = "txt")

################################################################################
# (1) Create a data frame with some information about each document
################################################################################
lapply(files, function(x){
  read_file(x) %>%   # Read using readr because of issues with iconv
    read_html() %>%
    html_elements("participant") %>%
    html_text() -> t
  
  grepl("\\[laughter.*?\\]",t, ignore.case = T) %>%
    sum() -> turns.with.laughter
  
  ntoken(t) %>%
    sum() -> n.token
  
  paste(t, collapse = "|") -> text
  
  meeting.date <- get.meeting.date(x)
    
  tibble(meeting.date = meeting.date,
         n.token = n.token,
         n.turns = length(t),
         turns.with.laughter = turns.with.laughter,
         text = text)
  }) %>%
  bind_rows() -> df.docs
df.docs$chair <- sapply(df.docs$meeting.date, get.chair)
df.docs$pct.turns.with.laughter <- df.docs$turns.with.laughter/(df.docs$n.turns*0.01)

################################################################################
# (2) Get occurrences of laughter and interruptions
################################################################################

files<-list.files("~/Dropbox/FOMC/FOMC_transcripts/sketchengine/update_oct_23/",
                  pattern = "txt",
                  full.names = T)

# (2.1) Split each meeting into turns
lapply(files, function(x){
  gsub(".+?/([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}).txt","\\1", x) -> meeting.date
  read_file(x) %>%
    read_html() %>%
    html_elements("participant") -> raw
  raw %>%
    html_text() %>%
    paste0(.,"|")-> turn
  raw %>%
    html_attr("name") -> name
  n.words<-tokens(turn) %>% sapply(.,function(y){length(y)})
  tibble(meeting.date = meeting.date,
         name=name,
         turn=turn,
         n.words=n.words) -> df
  
  df$start <- NA
  df$end <- NA
  df$start[1] <- 1
  df$end[1] <- df$n.words[1]
  
  for(i in 2:nrow(df)){
    df$start[i] <- df$end[i-1]+1
    df$end[i] <- df$start[i]+df$n.words[i]-1
  }
  
  df$chair <- sapply(df$meeting.date, get.chair)
  
  return(df)
}) -> turns



# (2.2) Interruptions: Who is interrupted and where in the meeting does the interruption occur?
lapply(turns, function(x){
  text <- x$turn %>%
    paste(collapse = " ")
  
  tokens(text) %>%
    #index(., pattern = phrase("(\\.\\.\\.\\.|\\.\\.\\.|-|—|--) \\|"), valuetype = "regex") %>%
    #index(.,pattern = phrase("(—|(--)) \\|"), valuetype = "regex") %>%
    index(.,pattern = phrase("(…(\\.)?|—|(--)) \\|"), valuetype = "regex") %>%
    pull(from)-> idx
  
  lapply(idx, function(y){
    x %>%
      filter(start<=y & end >=y) %>%
      mutate(idx=y,.after = end) %>%
      mutate(pct = idx/(sum(x$n.words)*0.01), .after = idx) %>%
      mutate(bin = cut(pct, breaks=seq(0,100,1)))
  }) %>%
    bind_rows()
}) -> interruptions

# (2.3) Laughter: Who causes the laughter and where in the meeting does it occur?
lapply(turns, function(x){
  text <- x$turn %>%
    paste(collapse = " ")
  
  tokens(text) %>%
    index(.,pattern = phrase("\\[ laughter"), valuetype = "regex") %>%
    pull(from)-> idx
  
  lapply(idx, function(y){
    x %>%
      filter(start<=y & end >=y) %>%
      mutate(idx=y,.after = end) %>%
      mutate(pct = idx/(sum(x$n.words)*0.01), .after = idx) %>%
      mutate(bin = cut(pct, breaks=seq(0,100,1)))
  }) %>%
    bind_rows()
}) -> laughter

save(turns, interruptions, laughter, file = "~/Dropbox/FOMC/new_analysis/data/data_interruptions_laughter.Rdata")


################################################################################
# Create plots
################################################################################
require(reshape2)
require(ggplot2)
require(ggsci)

# (A.1) Time series plot laughter
temp <- df.docs %>%
  select(meeting.date, pct.turns.with.laughter,chair) %>%
  mutate(chair=factor(chair, levels=c("Greenspan","Bernanke","Yellen")))

ggplot(data=temp) +
  geom_col(aes(x=meeting.date,
               y=pct.turns.with.laughter,
           fill = chair),
           width = 10) +
  # geom_vline(xintercept = as.Date("1987-07-18"), color="green") +
  # annotate("text", x=as.Date("1987-03-18"), y=10, label="Greenspan", angle=90, size=2) +
  # geom_vline(xintercept = as.Date("2006-03-01"), color="blue") +
  # annotate("text", x=as.Date("2005-11-01"), y=10, label="Bernanke", angle=90, size=2) +
  geom_vline(xintercept = as.Date("2007-07-20"), color="grey") +
  annotate("text", x=as.Date("2007-11-20"), y=18, label="Fin. Crisis", angle=90, size=2) +
  # geom_vline(xintercept = as.Date("2014-03-04"), color="red") +
  # annotate("text", x=as.Date("2013-11-01"), y=10, label="Yellen", angle=90, size=2) +
  xlab("Date") +
  ylab("Percent of turns with laughter") +
  theme_classic() +
  theme(legend.position = c(0.1, 0.8),
        legend.title = element_text(size=5),
        legend.text = element_text(size=5),
        #legend.position="none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8)) +
  scale_fill_nejm(name="Chair")
  
  

ggsave(file = "~/Dropbox/FOMC/new_analysis/plots/time_line_plot_laughter_v1.eps",
       width = 12.25,
       height = 7,
       units = "cm")


# (A.2) Facet plot
temp <- df.docs %>%
  select(meeting.date, pct.turns.with.laughter, pct.interrupted.turns) %>%
  melt(., id.var = "meeting.date") 

ggplot(data=temp) +
  geom_col(aes(x=meeting.date,
               y=value,
               fill = variable),
           #colour = variable),
           width = 10) +
  facet_grid(variable ~ ., scales = "free_y") + 
  labs(color='Variables') +
  geom_vline(xintercept = as.Date("1987-07-18"), color="green") +
  annotate("text", x=as.Date("1987-03-18"), y=10, label="Greenspan", angle=90, size=2) +
  geom_vline(xintercept = as.Date("1993-08-18"), color="magenta") +
  annotate("text", x=as.Date("1993-04-18"), y=10, label="Transcript publication", angle=90, size=2) +
  geom_vline(xintercept = as.Date("2001-10-01"), color="magenta") +
  annotate("text", x=as.Date("2001-06-01"), y=10, label="Enron", angle=90, size=2) +
  geom_vline(xintercept = as.Date("2005-08-31"), color="magenta") +
  annotate("text", x=as.Date("2005-04-30"), y=10, label="Hurricane Kathrina", angle=90, size=2) +
  geom_vline(xintercept = as.Date("2006-03-01"), color="blue") +
  annotate("text", x=as.Date("2005-11-01"), y=10, label="Bernanke", angle=90, size=2) +
  geom_vline(xintercept = as.Date("2007-07-20"), color="magenta") +
  annotate("text", x=as.Date("2007-03-20"), y=10, label="Fin. Crisis", angle=90, size=2) +
  geom_vline(xintercept = as.Date("2014-03-04"), color="red") +
  annotate("text", x=as.Date("2013-11-01"), y=10, label="Yellen", angle=90, size=2) +
  geom_vline(xintercept = as.Date("2016-11-09"), color="magenta") +
  annotate("text", x=as.Date("2016-07-09"), y=10, label="Trump elected", angle=90, size=2) +
  xlab("Date") +
  ylab("Percent of turns") + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8)) +
  scale_fill_discrete(name = "Variables",
                      labels=c('Laughter', 'Interruptions')) 
ggsave(file = "~/Dropbox/FOMC/new_analysis/plots/time_line_facet_plot_pct_update.eps",
       width = 12.24,
       height = 8,
       units = "cm")



# (B) Dispersion plot laughter
laughter %>%
  bind_rows %>%
  select(bin,chair,meeting.date) -> d

d$chair <- factor(d$chair, levels=c("Greenspan","Bernanke","Yellen"))

ggplot(data=d) +
  geom_bar(aes(x=bin
  ))+
  facet_grid(chair ~ ., scales = "free_y") +
  labs(color='Variables') +
  xlab("Stage of the meeting (1-100)") +
  ylab("Occurrences of laughter") + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(file = "~/Dropbox/FOMC/new_analysis/plots/barplot_laughter_update.eps",
       width = 12.25,
       height = 7,
       units = "cm") 

# (C) Dispersion plot interruptions
interruptions %>%
  bind_rows() %>%
  select(bin,chair,meeting.date) -> d

ggplot(data=d) +
  geom_bar(aes(x=bin
  ))+
  facet_grid(chair ~ ., scales = "free_y") + 
  labs(color='Variables') +
  xlab("Stage of the meeting (1-100)") +
  ylab("Number of interruptions") + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(file = "~/Dropbox/FOMC/new_analysis/plots/barplot_interruptions_update.eps",
       width = 12.25,
       height = 7,
       units = "cm") 


################################################################################
# Other statistics used in the text
################################################################################

# Number of meetings chaired by each person
df.docs %>%
  count(chair)

# Number of meetings that have an instance of laughter in the first stage
# laughter %>%
#   filter(chair=="Greenspan" & bin=="(0,1]") %>%
#   pull(meeting.date) %>%
#   unique() %>%
#   length()

# For Greenspan: Timeline of meeting initial laughter
# laughter %>%
#   filter(chair=="Greenspan" & bin=="(0,1]") %>%
#   ggplot() +
#   geom_bar(aes(x=meeting.date))


###################



################################################################################
# Meetings with laughter initiated by the chair in an early/late stage of the meeting
################################################################################
lapply(laughter, function(x){
  if(length(x)>0){
    x %>%
      filter(bin=="(99,100]") %>%
      #filter(bin=="(0,1]") %>%
      filter(name %in% c("CHAIRMAN GREENSPAN.","CHAIRMAN BERNANKE.","CHAIR YELLEN.")) %>%
      pull(name) %>%
      unique()-> names
      }
})%>%
  unlist() %>%
  table() %>%
  sort(decreasing = T)

################################################################################
# Occurrences of laughter as percent of the total number of words a participant 
# utters
################################################################################
sapply(laughter, function(x){
  if(length(x)>0){
    x %>%
      pull(name)
  }
}) %>%
  unlist() %>%
  table() %>%
  sort(decreasing = T) %>%
  as_tibble()->laughter.per.part
colnames(laughter.per.part) <- c("name","n.laughter")

turns %>%
  bind_rows() %>%
  group_by(name) %>%
  summarise(n.words=sum(n.words))-> words.per.part

left_join(laughter.per.part,words.per.part) %>%
  mutate(rel.freq.per.1000.words=n.laughter/(n.words*0.001)) %>%
  filter(n.words>=70000) %>%
  arrange(-rel.freq.per.1000.words)-> x

################################################################################
# Number of meetings where there is registered laughter in the first/last interval
################################################################################
laughter %>%
  bind_rows() %>%
  #filter(bin=="(99,100]") %>%
  filter(bin=="(0,1]") %>%
  group_by(chair) %>%
  summarise(meetings=unique(meeting.date) %>% 
            length())
  

# Sequences of two or more [laughter] within a range of 100 characters
lapply(turns, function(x){
  x$turn %>%
    paste(collapse = " ")
})-> texts

lapply(texts, function(x){
  str_extract_all(x, pattern = regex("\\[laugh.{1,100}\\|.{1,100}\\[laugh.+?\\]",ignore_case = T))
}) %>%
  unlist()->y

# Calculate DP (Gries 2020) for the three subcorpora
chairs <- c("Greenspan","Bernanke","Yellen")

sapply(chairs, function(x){
  v <- df.docs %>%
    filter(chair==x) %>%
    pull(turns.with.laughter)
  f <- sum(v)
  df.docs %>%
    filter(chair==x) -> subcorpus
  total.tokens <- subcorpus %>%
    pull(n.token) %>%
    sum()
  s <- subcorpus$n.token/total.tokens
  
  dp <- 0.5*sum(abs((v/f)-s))
  dp.norm <- dp/(1-min(s))
}) 

############
## Mann-Kendall test for time series
require(Kendall)
chairs <- c("Greenspan","Bernanke","Yellen")
sapply(chairs, function(x){
  d %>%
    filter(chair==x) %>%
    #filter(bin=="(0,1]") %>%
    count(meeting.date) %>%
    pull(n) %>%
    MannKendall()
})

################################################################################
# Statistics from the boxplots
################################################################################
chairs <- c("Greenspan","Bernanke","Yellen")
sapply(chairs, function(x){
  temp %>%
    filter(chair==x) %>%
    pull(pct.turns.with.laughter) %>%
    boxplot.stats(.,
                  coef = 1.5,
                  do.conf = TRUE,
                  do.out = TRUE) %>%
    .$stats
    
})



################################################################################
# removed from script
################################################################################


# (2) Tokenize the texts
# df.docs %>%
#   pull(text) -> text
# names(text) <- get.meeting.date(files)  
# tokens(text) -> tokenized
# 
# save(tokenized, file="~/Dropbox/FOMC/FOMC_transcripts/data/corpus_tokenized.Rdata")
# 
# # (3) Find the instances of laughter
# kwic(tokenized, pattern = phrase("\\[ laughter"), valuetype = "regex") %>%
#   as_tibble()-> laughter
# laughter %>%
#   rename(meeting.date = docname,
#          toknum.l = from) %>%
#   select(meeting.date, toknum.l) -> laughter
# laughter$meeting.date <- as.Date(laughter$meeting.date)
# left_join(laughter,
#           df.docs %>%
#             select(meeting.date,n.token,chair)) -> laughter
# laughter$token.in.doc.pct <- laughter$toknum.l/(laughter$n.token*0.01)
# mutate(laughter, bin = cut(token.in.doc.pct, breaks=seq(0,100,1))) -> laughter
# 
# # (4) Find interrupted turns
# kwic(tokenized, pattern = phrase("(\\.\\.\\.\\.|\\.\\.\\.|-|—|--) \\|"), valuetype = "regex") %>%
#   as_tibble()-> interruptions
# interruptions %>%
#   rename(meeting.date = docname,
#          toknum.i = from) %>%
#   select(meeting.date, toknum.i) -> interruptions
# interruptions$meeting.date <- as.Date(interruptions$meeting.date)
# left_join(interruptions,
#           df.docs %>%
#             select(meeting.date,n.token,chair)) -> interruptions
# interruptions$token.in.doc.pct <- interruptions$toknum.i/(interruptions$n.token*0.01)
# mutate(interruptions, bin = cut(token.in.doc.pct, breaks=seq(0,100,1))) -> interruptions
# 
# # (5) Merge the data
# laughter %>%
#   count(meeting.date, name="n.laughter") %>%
#   left_join(df.docs, .) -> df.docs
# interruptions %>%
#   count(meeting.date, name="n.interruptions") %>%
#   left_join(df.docs, .) -> df.docs
# df.docs %>%
#   select(-text) -> df.docs
# 
# df.docs %>%
#   mutate(pct.turns.with.laughter = turns.with.laughter/(n.turns*0.01),
#          pct.interrupted.turns = n.interruptions/(n.turns*0.01),) -> df.docs
# 
# save(laughter,interruptions,df.docs,file = "~/Dropbox/FOMC/FOMC_transcripts/data/data_laughter_interruptions.Rdata")

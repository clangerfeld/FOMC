################################################################################
# Script used for the paper "Humor and laughter as indicators of meeting 
# leadership style"
################################################################################
require(dplyr)
require(rvest)
require(readr)
require(quanteda)
require(stringr)

setwd("~/Dropbox/FOMC/Humor_and_laughter/")

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

files <- list.files("data/", full.names = T, pattern = "txt")

################################################################################
# (1) Create a data frame with some information about each document
# - meeting date
# - number of tokens
# - number of turns
# - number of turns that contain laughter
# - the transcript text
################################################################################

lapply(files, function(x){
  read_file(x) %>%   # Read using readr because of issues with iconv
    read_html() %>%
    html_elements("participant") %>%
    html_text() -> turns
  
  str_count(turns, regex("\\[laughter", ignore_case = T)) %>%
    sum() -> turns.with.laughter
  
  paste(turns, collapse = "") -> text
  
  ntoken(text) %>%
    sum() -> n.token
  
  meeting.date <- get.meeting.date(x)
  
  tibble(meeting.date = meeting.date,
         n.token = n.token,
         n.turns = length(turns),
         turns.with.laughter = turns.with.laughter,
         text = text)
}) %>%
  bind_rows() -> df.docs

# Add meeting chair
df.docs$chair <- sapply(df.docs$meeting.date, get.chair)

# Add percent of turns that contain at least one instance of laughter
df.docs$pct.turns.with.laughter <- df.docs$turns.with.laughter/(df.docs$n.turns*0.01)

################################################################################
# (2) Create plots used in the paper
################################################################################
require(ggplot2)
require(ggsci)

# (2.1) Time series plot laughter
data <- df.docs %>%
  select(meeting.date, pct.turns.with.laughter,chair) %>%
  mutate(chair=factor(chair, levels=c("Greenspan","Bernanke","Yellen")))

ggplot(data) +
  geom_col(aes(x=meeting.date,
               y=pct.turns.with.laughter,
               fill = chair),
           width = 10) +
  geom_vline(xintercept = as.Date("2007-07-20"), color="grey") +
  annotate("text", x=as.Date("2007-11-20"), y=18, label="Fin. Crisis", angle=90, size=2) +
  xlab("Date") +
  ylab("Percent of turns with laughter") +
  theme_classic() +
  theme(legend.position = c(0.1, 0.8),
        legend.title = element_text(size=5),
        legend.text = element_text(size=5),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8)) +
  scale_fill_nejm(name="Chair")

# (2.2) Box plot
data %>%
  ggplot( aes(x=chair, y=pct.turns.with.laughter, fill=chair)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position="none",
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8))+
  scale_fill_grey(start = 0.5, end =0.9) +
  xlab("") + 
  ylab("Percent of turns with laughter")


# (2.3) Dispersion plot that shows the number of laughter tokens in all stages
#       of the meetings

# Get the token number and percentile of each laughter token
lapply(c(1:nrow(df.docs)), function(x){
  tokens(df.docs$text[x]) %>%
    index(.,pattern = phrase("\\[ laughter"), valuetype = "regex") %>%
    pull(from)-> idx
  tibble(idx = idx) %>%
    mutate(n.token = df.docs$n.token[x]) %>%
    mutate(pct = idx/(n.token*0.01)) %>%
    mutate(bin = cut(pct, breaks=seq(0,100,1))) %>%
    mutate(chair = df.docs$chair[x])
}) %>%
  bind_rows %>%
  select(pct,bin,chair) -> disp

disp$chair <- factor(disp$chair, levels=c("Greenspan","Bernanke","Yellen"))

ggplot(data=disp) +
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


################################################################################
# (3) Tables
################################################################################

# (3.1) Construct two objects needed for the frequency information in the tables:
#       (a) A data frame that allows us to identify in which participants turn a
#           laughter token occurs
#       (b) A data frame that holds information about where in the documents the
#           laughter tokens occur

# (a) Construct a name index mapping. Given a token number and a meeting
#     date, we can look up who produced the token.
lapply(files, function(x){
  read_file(x) %>%
    read_html() %>%
    html_elements("participant") -> raw
  raw %>%
    html_text() -> turn
  raw %>%
    html_attr("name") -> name
  
  get.meeting.date(x) -> meeting.date
  
  tokens(turn) -> t
  
  tibble(meeting.date=meeting.date,
         name=name,
         n.token=ntoken(t)) %>%
    mutate(start = NA) %>%
    mutate(end = NA) -> df
  df$start[1] <- 1
  df$end[1] <- df$n.token[1]
  
  for(i in 2:nrow(df)){
    df$start[i] <- df$end[i-1]+1
    df$end[i] <- df$end[i-1] + df$n.token[i]
  }
  
  df %>%
    select(meeting.date, name, start, end)
}) %>%
  bind_rows() -> name.idx.mapping

# (b) Identify the laughter tokens and their percentile. We also identify who
#     initiated the laughter based on "name.idx.mapping"
lapply(files, function(x){
  meeting.date.a <- get.meeting.date(x)
  chair <- get.chair(meeting.date.a)
  
  read_file(x) %>%
    read_html() %>%
    html_elements("participant") %>%
    html_text() %>%
    paste(collapse = "") -> text
  tokens(text) %>%
    index(.,pattern = phrase("\\[ laughter"), valuetype = "regex") %>%
    pull(from)-> idx
  
  if(length(idx)>0){
    n.tokens <- ntoken(text) 
    
    sapply(idx, function(i){
      name.idx.mapping %>%
        filter(meeting.date==meeting.date.a) %>%
        filter(start<=i & end>=i) %>%
        pull(name)
    }) -> names
    
    tibble(meeting.date = meeting.date.a,
           chair = chair,
           name = names,
           idx = idx,
           pct = idx/(n.tokens*0.01),
           bin = cut(pct, breaks=seq(0,100,1)))
  }
}) %>%
  bind_rows() -> laughter


################################################################################
# (3.2) Table 1: Mann-Kendall test for time series
################################################################################
require(Kendall)
chairs <- c("Greenspan","Bernanke","Yellen")
sapply(chairs, function(x){
  laughter %>%
    filter(chair==x) %>%
    count(meeting.date) %>%
    pull(n) %>%
    MannKendall()
})

################################################################################
# (3.3) Table 2 and Table 3
################################################################################

# Meetings chaired:
df.docs %>%
  count(chair, name="meetins.chaired") -> chaired

# Total meetings with laughter in early/late stage

lapply(c("(0,1]","(99,100]"), function(x){
  laughter %>%
    filter(bin==x) %>%
    group_by(chair) %>%
    reframe(unique(meeting.date)) %>%
    count(chair, name=x) %>%
    left_join(chaired,.) %>%
    mutate(pct.of.total=.[[3]]/(.[[2]]*0.01))
})

# Meetings with laughter in first/last stage initiated by chair
lapply(c("(0,1]","(99,100]"), function(x){
  laughter %>%
    filter(bin==x) %>%
    filter(name %in% c("CHAIRMAN GREENSPAN.","CHAIRMAN BERNANKE.","CHAIR YELLEN.")) %>%
    group_by(chair) %>%
    reframe(unique(meeting.date)) %>%
    count(chair, name=x) %>%
    left_join(chaired,.) %>%
    mutate(pct.of.total=.[[3]]/(.[[2]]*0.01))
})

################################################################################
# (3.4) Table 4: Laughter initiated by the chair in all stages of the meetings
################################################################################

# (a) Words produced:
name.idx.mapping %>%
  filter(name %in% c("CHAIRMAN GREENSPAN.","CHAIRMAN BERNANKE.","CHAIR YELLEN.")) %>%
  reframe(words.produced=end-start+1,name=name) %>%
  group_by(name) %>%
  summarise(total.words=sum(words.produced)) -> words
  
# (b) Number of laughter tokens
laughter %>%
  filter(name %in% c("CHAIRMAN GREENSPAN.","CHAIRMAN BERNANKE.","CHAIR YELLEN.")) %>%
  count(name, name = "n.laughter") -> laughter.tokens

# (c) Join and add relative frequency per 100 words
left_join(words, laughter.tokens) %>%
  mutate(per.1000 = .[[3]]/(.[[2]]*0.001))

################################################################################
# (3.5) Other statistics used in the paper
################################################################################

# Number of meetings chaired by each person
df.docs %>%
  count(chair)

# Statistics from the boxplots
chairs <- c("Greenspan","Bernanke","Yellen")
sapply(chairs, function(x){
  data %>%
    filter(chair==x) %>%
    pull(pct.turns.with.laughter) %>%
    boxplot.stats(.,
                  coef = 1.5,
                  do.conf = TRUE,
                  do.out = TRUE) %>%
    .$stats
  
})



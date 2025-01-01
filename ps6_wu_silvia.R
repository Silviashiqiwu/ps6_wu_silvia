################################################################################
##
## [ PROJ ] < Problem set 6 >
## [ FILE ] < 260B problemset6 >
## [ AUTH ] < Silviashiqiwu >
## [ INIT ] < 12312024 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
## ---------------------------

## ---------------------------
## directory paths
## ---------------------------
getwd()
data_dir <- file.path('.', 'data')
dir.exists(data_dir) #TRUE
## ---------------------------

## -----------------------------------------------------------------------------
## Part I : Setting up repository
## -----------------------------------------------------------------------------
url <- 'https://nces.ed.gov/ipeds/datacenter/data/'
files <- c('HD2017', 'HD2018', 'HD2019')
suffixes <- c('', '_Dict', '_Stata')

## -----------------------------------------------------------------------------
## part II : Looping over elements
## -----------------------------------------------------------------------------
download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2017.zip",
              destfile = file.path('.', 'data','HD2017.zip'))
download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2017_Dict.zip",
              destfile = file.path('.', 'data','HD2017_Dict.zip'))
download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2017_Stata.zip",
              destfile = file.path('.', 'data','HD2017_Stata.zip'))

download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2018.zip",
              destfile = file.path('.', 'data','HD2018.zip'))
download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2018_Dict.zip",
              destfile = file.path('.', 'data','HD2018_Dict.zip'))
download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2018_Stata.zip",
              destfile = file.path('.', 'data','HD2018_Stata.zip'))

download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2019.zip",
              destfile = file.path('.', 'data','HD2019.zip'))
download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2019_Dict.zip",
              destfile = file.path('.', 'data','HD2019_Dict.zip'))
download.file(url = "https://nces.ed.gov/ipeds/datacenter/data/HD2019_Stata.zip",
              destfile = file.path('.', 'data','HD2019_Stata.zip'))

for(i in files){
  writeLines(str_c("i=",i))
  for(x in suffixes){
    writeLines(str_c("x=",x))
    file_name <- str_c(i, x, ".zip")
    writeLines(str_c("file_name =", file_name))
    file_url <- str_c(url, file_name)
    writeLines(str_c("file name =", file_url))
    download.file(url = file_url, destfile = file.path(data_dir, file_name))
  }
}

## -----------------------------------------------------------------------------
# Part III: Looping over indices
## -----------------------------------------------------------------------------
unzip(zipfile = file.path(data_dir, 'HD2017.zip'), exdir = data_dir)
unzip(zipfile = file.path(data_dir, 'HD2018.zip'), exdir = data_dir)
unzip(zipfile = file.path(data_dir, 'HD2019.zip'), exdir = data_dir)

print(files)
str(files)
class(files)

for(i in 1: length(files)){
  writeLines(str_c("value of object = ", i))
  str(files[[i]])
  unzip(zipfile = file.path(data_dir, str_c(files[[i]], ".zip")), exdir = data_dir)
  df <- read_csv(file = file.path(data_dir, str_c(files[[i]], '.csv')))
}

dfs <- vector(mode = 'list', length = length(files))
for(i in 1: length(files)){
  writeLines(str_c("value of object = ", i))
  str(files[[i]])
  unzip(zipfile = file.path(data_dir, str_c(files[[i]], ".zip")), exdir = data_dir)
  df <- read_csv(file = file.path(data_dir, str_c(files[[i]], '.csv')))
  dfs[[i]] <- df
}

## -----------------------------------------------------------------------------
# Part IV: Looping over names
## -----------------------------------------------------------------------------
hd2019 <- dfs[[3]]


hd2019_subset <- hd2019 %>% 
  mutate(HBCU = ifelse(HBCU == 1, 1, 0),
         TRIBAL = ifelse(TRIBAL == 1, 1, 0),
         HOSPITAL = ifelse(HOSPITAL == 1, 1, 0)) %>% 
  select(HBCU,TRIBAL,HOSPITAL)
  
for(i in names(hd2019_subset)){
  writeLines(str_c("The name of columns in df2019_subset = ", i))
  writeLines(str_c("Number of ", i, "=", sum(hd2019_subset[[i]])))
  }

# The name of columns in df2019_subset = HBCU
# Number of HBCU=102
# The name of columns in df2019_subset = TRIBAL
# Number of TRIBAL=35
# The name of columns in df2019_subset = HOSPITAL
# Number of HOSPITAL=96

nums <- vector(mode = "list", length = length(hd2019_subset))

for(p in 1:length(hd2019_subset)){
  nums[[p]] <- sum(hd2019_subset[[p]])
}

ggplot(data.frame(nums), aes(seq_along(nums), nums)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = seq_along(hd2019_subset), labels = names(hd2019_subset)) +
  xlab(NULL) + ylab(NULL)

## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------

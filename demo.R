# Choose mirrors for the speed
chooseCRANmirror()
# Install packages 
pkg <- c("lubridate","stringr")
install.packages(pkg)


# vectors have variables of _one_ type
c(1, 2, "three") 
typeof(c(1, 2, "three"))
# shorter arguments are recycled 
(1:3) * 2
(1:4) * c(1, 2)
# warning! (why?)
(1:4) * (1:3)

# It is recommanded to add column names
capColor = c(huey = "red", duey = "blue", louie = "green")

# Indexing is a key to data cleaning 
capColor["louie"]
names(capColor)[capColor == "blue"]
x <- c(4, 7, 6, 5, 2, 8)
I <- x < 6
J <- x > 7
x[I | J]
x[c(TRUE, FALSE)] # Index is recycled in case of short indexed vector
x[c(-1, -2)]

# Replacing values are done in a similar way
x <- 1:10
x[c(TRUE, FALSE)] <- 1
x

# List 
# Single bracket returns a sub-list 
# double bracket returns an object in the list 
L <- list(x = c(1:5), y = c("a", "b", "c"), z = capColor)
L
L[[2]]
L$y # the dollar operator $ can be used to retrieve 
L[c(1, 3)]
L[c("x", "y")]
L[["z"]]

# data.frame is a list of vectors, with every vector of the same length

d <- data.frame(x = 1:10, y = letters[1:10], z = LETTERS[1:10])
d
d[1] # typeof(d[1])
d[, 1] # typeof(d[,1]),  the result of a two-index selection is
# simplified.  selecting a single column using a two-index results
# in a vector.
d[, 1, drop = FALSE] # This behaviour may be switched off using drop=FALSE
d[c("x", "z")]
d[d$x > 3, "y", drop = FALSE]
d[2, ]

# Special values: NA, NULL, Inf, NaN

# NA: Not Available, a placeholder for a missing value. It has length of one
# is.na() can be used to detect NA's.

NA + 1
sum(c(NA, 1, 2))
median(c(NA, 1, 2, 3), na.rm = TRUE)
length(c(NA, 2, 3, 4))
3 == NA
NA == NA
TRUE | NA
TRUE & NA

# NULL: mathematically an empty set, no length
# is.null() can be used to detect NULL variables.

length(c(1, 2, NULL, 4))
sum(c(1, 2, NULL, 4))
x <- NULL
c(x, 2)

# Inf: infinity, only applies to vectors of class numeric; Inf = number/0
pi/0
2 * Inf
Inf - 1e+10
Inf + Inf
3 < -Inf
Inf == Inf

# NaN: Not a Number. The result is unknown but surely is not a number
# 0/0, Inf-Inf, Inf/Inf
# is.nan() can be used to detect NaN's.

NaN + 1
exp(NaN)

######################################################

#     Read text data into R

######################################################

# The standard extension to store a dataset is csv-like format
# If you have non-standard extensions, e.g., xls, dta, etc, use the corresponding 
# software to convert the data into a standard format. 

# When reading, the default is to treat the first row as header
person <- read.csv("~/MEGAsync/WHU/Teaching/Undergraduate_econometrics/Advanced_Econometrics/Data_Cleaning_Demo/unnamed.txt")
View(person)
# The correct way is 
path <- c("~/MEGAsync/WHU/Teaching/Undergraduate_econometrics/Advanced_Econometrics/Data_Cleaning_Demo/unnamed.txt")
person <- read.csv(
  file
  = path
  , header
  = FALSE
  , col.names = c("age","height") )

# When colClass is not specified, R will automatically determine
# This could be troublesome 
str(person)

# Better way, it will report errors when matching fails
person <- read.csv(
  file
  = path,
  header= FALSE,
  colClass = c('numeric','numeric'),
  col.names = c("age","height") )

# Alternatively, one could convert everything into character
person <- read.csv(
  file
  = path,
  header= FALSE,
  stringsAsFactors=FALSE,
  col.names = c("age","height") )
# And us as. function to convert into proper type
person$height <- as.numeric(person$height)
person



# Reading data with readlines 
# Sometimes data set could be irregular, we could read lines one by one 

# Step 1. Read data
path <- c("~/MEGAsync/WHU/Teaching/Undergraduate_econometrics/Advanced_Econometrics/Data_Cleaning_Demo/daltons.txt")
txt <- readLines(path)
txt

# Step 2. Selecting relevent lines 
I <- grepl('^%',txt) # Detect lines starting with %
dat <- txt[!I] # and throw them out

# Step 3. Split lines into different fields with strsplit 
fieldList <- strsplit(dat,split = ',')
fieldList

# Step 4. standarlize rows 
assignFields <- function(x){
  out <- character(3)
  # get names
  i <- grepl("[[:alpha:]]",x)
  out[1] <- x[i]
  # get birth date (if any)
  i <- which(as.numeric(x) < 1890)
  out[2] <- ifelse(length(i)>0, x[i], NA)
  # get death date (if any)
  i <- which(as.numeric(x) > 1890)
  out[3] <- ifelse(length(i)>0, x[i], NA)
  out
}

standardFields <- lapply(fieldList, assignFields)
standardFields

# Transform to data.frame
# First, we transform list to matrix 
M <- matrix(
  unlist(standardFields) # unlist breaks the list into elements
  , nrow=length(standardFields)
  , byrow=TRUE)

M

# Or using sapply
M <- sapply(fieldList, assignFields)
M
M <- t(M)


colnames(M) <- c("name","birth","death")
daltons <- as.data.frame(M, stringsAsFactors=FALSE)

# Step 6. Normalize and coerce to correct types.
daltons$birth <- as.numeric(daltons$birth)
daltons$death <- as.numeric(daltons$death)
daltons

# Factor 
# R use factor to store category data
f <- factor(c("a", "b", "a", "a", "c"))
levels(f)

# Usually, one can use integers with a translation table
gender <- c(2, 1, 1, 2, 0, 1, 1) # Here male =1, female=2, unknown = 0
recode <- c(male = 1, female = 2) # Reference
gender <- factor(gender, levels = recode, labels = names(recode))


# Levels do not have order, but it is common to assign a reference level
gender <- relevel(gender, ref = "female")

# Levels can be summarized by its mean
age <- c(27, 52, 65, 34, 89, 45, 68)
gender <- reorder(gender, age)

# The means can also be removed bu NULL
attr(gender, "scores") <- NULL
gender


# Dates 
library(lubridate)
# Convert text to POSIXct objects
#  a POSIXct object stores the number of seconds that have passed since January
# 1, 1970 00:00. 

current_time <- Sys.time()
class(current_time)
# Converting from a calender time to POSIXct and back is not entirely trivial
# but package lubridate is helpful

dates <- c("15/02/2013", "15 Feb 13", "It happened on 15 02 '13")
dmy(dates) # Assume the input is a form of day month year order


# Character Manipulation
library(stringr)

# String normalization techniques are aimed at transforming a variety of strings to a smaller set of
# string values which are more easily processed.

str_trim(" hello world ") # remove extra empty spaces at the beginning and end of a string 

str_trim(" hello world ", side = "left") # remove extra empty, left 
str_trim(" hello world ", side = "right")

# strings can be padded with spaces or other characters 
str_pad(112, width = 6, side = "left", pad = 0)

# Convert to UPPER and lower cases 
toupper("Hello world")
tolower("Hello World")

# Approximate string matching

# First string match: Pattern

# grep, globally search for regular expression and print it, returns a numerical index 
# grepl, returns a logical index 
gender <- c("M", "male ", "Female", "fem.")
grepl("m", gender)
grep("m", gender)
which(grepl("m", gender))

# The result is case sensitive, turn off this option 
grepl("m", gender, ignore.case = TRUE)

# search for strings that start with an m or M. 
grepl("^m", gender, ignore.case = TRUE)
# the ^ (caret) is an example of `meta-character`, i.e., it does not 
# indicates `^` itself, but something else

# if search for meta-character is needed, fixed = T
grepl("^", gender, fixed = TRUE)


# Second string match: string distance 
# It indicates how much two strings differ from each other 
# adist(), it counts how many basic operations are needed to turn one string into another

adist("abc", "bac")

# Using adist, we can compare fuzzy text strings to a list of known codes.

codes <- c("male", "female")
D <- adist(gender, codes)
colnames(D) <- codes
rownames(D) <- gender
D

# Find which code matches best with raw data, find the smallest distance

i <- apply(D, 1, which.min)
data.frame(rawtext = gender, coded = codes[i])




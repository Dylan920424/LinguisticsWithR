# Chapter 8
library(dplyr)
library(stringr)
#string length
word_string <- c("你好", "word", "string")
word_string %>%
  str_length

# str_c(): combine strings into a longer one
## specify str_c(collapse=" ") when combining elements in a vector


# str_view with regex
x <- c("apple is good", 
       "banana is better than apple", 
       "an apple a day keeps the doctor away")]
str_view(x, "^apple")
str_view(x, "apple$")

#some types of regex
str_view_all("I love YOU_2", "\\w") #alphabets and numbers and _
str_view_all("I love YOU_2", "\\W") #other than alphabets and numbers and _
                                    #It does NOT include the hyphen -
str_view_all("I love YOU_2", "[a-z]") #lowercase alphabets [A-Z]
str_view_all("I love YOU_2", "\\s")#white space
str_view_all("I love YOU_2", "\\S")#non-white space
str_view_all("I love YOU, 2", "[[:PUNCT:]]") #punctuations
str_view_all("I love YOU, 2, my luv", "(ov)")

#using () and {}
x <- "aaabbbababcdf"
str_view(x, "(ab){2}") # the scope of the quantifier is `ab`

#non greedy match
str_view(x, "ab*?") # find minial match
str_view("abc", "ab?")

#backreference
str_view_all(x, "(.)\\1")

#str_detect()
str_detect(fruit, "e$")

#extract those words that end with e and make into a list
str_subset(fruit, "e$")
str_extract(fruit, "e$")
str_extract(x, "e$")# find all the match in each string

#string_match ("()")
str_match(fruit, "(bl)([aeiou]+)")
# Extract the content of the matches of the strings in STRING 
# as well as each capture groups (character)

#str_replace()

#str_split()
x <- sentences %>% head(5)
str_split(x,pattern ="\\s")

#str_split()
fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ")
fields %>% str_split(": ", simplify = T) #simplify=T :make into  matrix

#only replace part of it 
win <- c("Windows2000", "Windows", "WindowsNT", "Windows7", "Windows10")
str_view(win, "Windows((95)|(98)|(NT)|(2000))")
str_view(win, "Windows(?=95|98|NT|2000)") #is
str_view(win, "Windows(?!95|98|NT|2000)") #not 
#(?=...): positive lookahead—extract a match only when … is on the right
#(?!...): negative lookahead—extract a match only when … is NOT on the right
win <- c("2000Windows", "Windows", "NTWindowsNT", "7Windows", "10Windows")
str_view(win, "(?<=95|98|NT|2000)Windows")
#(?<=pattern): The target match has to be preceded by the pattern defined in the condition

#Chinese 4-character idioms
library(tidyverse)
library(dplyr)
all_idioms <- readLines(con = "dict-ch-idiom.txt",encoding="UTF-8")
head(all_idioms)
length(all_idioms)
idiom <- tibble(string = all_idioms)

idiom %>%
  filter(str_detect(string, ".來.去"))

idiom_laiqu <-idiom %>%
  filter(str_detect(string, ".來.去")) %>%
  mutate(pattern = str_replace(string, "(.)來(.)去", "\\1_\\2")) %>%
  separate(pattern, into = c("w1", "w2"), sep = "_")
idiom_laiqu

idiom_laiqu %>%
  mutate(structure = ifelse(w1==w2, "XX","XY")) %>%
  count(structure)

idiom_laiqu %>%
  mutate(structure = ifelse(w1==w2, "XX","XY")) %>%
  count(structure) %>%
  ggplot(aes(structure, n, fill = structure)) + 
  geom_col() 


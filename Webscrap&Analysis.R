#===Chapter 3===

library(tidyverse)
library(rvest)
#=see this link for documentation: http://rvest.tidyverse.org/
#=open a browser and link to the page=
ptt.url <- "https://www.ptt.cc/bbs/Gossiping"
gossiping.session <- html_session(ptt.url)

#=extract the age verification from the current page=
gossiping.form <- gossiping.session %>%
  html_node("form") %>%
  html_form()
gossiping.form

#===solution 2===
#using cookie
#gossip <- GET(url,set_cookies(over18=1))%>%
#  read_html()
#===solution 2 End===

gossiping <- submit_form(
  session = gossiping.session,
  form = gossiping.form,
  submit = "yes"
)
gossiping

#=find out the number of the most recent index page=
page.latest <- gossiping %>%
  html_nodes("a") %>% # extract all <a> elements
  html_attr("href") %>%  # extract the attributes `href`
  str_subset("index[0-9]{2,}\\.html") %>% # find the `href` with the index number except for 1 (index[0-9]{2,})
  str_extract("[0-9]+") %>% # extract the number
  as.numeric()+1
page.latest

#===problem===
#the page here might not the lastest=
#===problem===


#retrive links
link <- str_c(ptt.url, "/index", page.latest, ".html")
links.article <- gossiping %>%
  jump_to(link) %>% # move session to the most recent page
  html_nodes("a") %>% # extract article <a>
  html_attr("href") %>% # extract article <a> `href` attributes
  str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html") %>% # extract links
  str_c("https://www.ptt.cc",.)
links.article

#========
#jump_to() and follow_link()

#html_session("http://hadley.nz/") %>% follow_link(css="#code .text-center a")
#nevigate to the link by clicking the github botton

#html_session("http://hadley.nz/") %>% jump_to("http://github.com/hadley/")
#jump to another session
#========

#=======
#solution2: Using css selector
#=======
gossiping %>%
  jump_to(link) %>%
  html_nodes(css=".title a")%>%
  html_attr("href") %>%
  str_c("https://www.ptt.cc",.)
#=======
#solution2_done: Using css selector
#=======

#=====
#html_node is like [[ it always extracts exactly one element. 
#When given a list of nodes, html_node will always return a list of the same length, 
#the length of html_nodes might be longer or shorter.
#=====

#Time to scrape page info~~
temp.html <- gossiping %>%
  jump_to(links.article[1])

#Let's scrape metadata~
#A. the author
#B. board
#C. Title
#D. Date
article.header <- temp.html %>%
  html_nodes(css=".article-meta-value")%>%
  html_text()
article.header[1] #author
article.header[2] #board
article.header[3] #title
article.header[4] #time-stamp
#parse the time: optional#
lubridate::parse_date_time(article.header[4],order="b d H:M:S y") #time-stamp

#Let's scrape the content
article.content <- temp.html %>%
  html_nodes(xpath="//div[@id='main-content']/text()")%>%
  html_text()%>%
  str_c(collapse="")#make into a string

#combine into tibble~
article.table <- tibble(datetime=lubridate::parse_date_time(article.header[4],order="b d H:M:S y"),
                        title = article.header[3],
                        author = article.header[1],
                        content = article.content, 
                        url = links.article[1])    
article.table 

#extract push comments
push_tags <- ".push-tag"
push_author <- ".push-userid"
push_content <- ".push-content"
push_date <- ".push-ipdatetime"

push_tags_table <- temp.html %>%
  html_nodes(css=push_tags) %>%
  html_text()
push_author_table <- temp.html %>%
  html_nodes(css=push_author) %>%
  html_text()
push_content_table <- temp.html %>%
  html_nodes(css=push_content) %>%
  html_text()
push_date_table <- temp.html %>%
  html_nodes(css=push_date ) %>%
  html_text()
(push.table <- tibble(tag=push_tags_table,
                     author=push_author_table,
                     content = push_content_table,
                     datetime = push_date_table,
                     url=links.article[1]))
#====
#map(a,function_b)
#map every item in a to function B
a <- c(12,4,6,8,5,3)
a
map(a,as.character()) 
#but it returns a nested list
map(a,as.character()) %>%
  unlist()
#====


#======================
# Exercise 1: write a function to Scrape more than one index page
#======================
ScrapArticle <- function(u){
  ptt.url <- "https://www.ptt.cc/bbs/Gossiping"
  gossiping.session <- html_session(ptt.url)
  gossiping.form <- gossiping.session %>%
    html_node("form") %>%
    html_form()
  gossiping.form
  gossiping <- submit_form(
    session = gossiping.session,
    form = gossiping.form,
    submit = "yes"
  )
  gossiping
  temp.html <- gossiping %>%
    jump_to(u)
  article.header <- temp.html %>%
    html_nodes(css=".article-meta-value")%>%
    html_text()
  article.header[1] #author
  article.header[2] #board
  article.header[3] #title
  article.header[4] #time-stamp
  #parse the time: optional#
  lubridate::parse_date_time(article.header[4],order="b d H:M:S y")
  article.content <- temp.html %>%
    html_nodes(xpath="//div[@id='main-content']/text()")%>%
    html_text()%>%
    str_c(collapse="")
  article.table <- tibble(datetime=lubridate::parse_date_time(article.header[4],order="b d H:M:S y"),
                          title = article.header[3],
                          author = article.header[1],
                          content = article.content, 
                          url = u)    
  article.table 
  push_tags <- ".push-tag"
  push_author <- ".push-userid"
  push_content <- ".push-content"
  push_date <- ".push-ipdatetime"
  push_tags_table <- temp.html %>%
    html_nodes(css=push_tags) %>%
    html_text()
  push_author_table <- temp.html %>%
    html_nodes(css=push_author) %>%
    html_text()
  push_content_table <- temp.html %>%
    html_nodes(css=push_content) %>%
    html_text()
  push_date_table <- temp.html %>%
    html_nodes(css=push_date ) %>%
    html_text()
  (push.table <- tibble(tag=push_tags_table,
                        author=push_author_table,
                        content = push_content_table,
                        datetime = push_date_table,
                        url=u))
  return(list(article.table = article.table, push.table = push.table))
}

ScrapePage <- function(pageNumber){
  link <- str_c(ptt.url, "/index", pageNumber, ".html")
  links.article <- gossiping %>%
    jump_to(link) %>% # move session to the most recent page
    html_nodes("a") %>% # extract article <a>
    html_attr("href") %>% # extract article <a> `href` attributes
    str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html") %>% # extract links
    str_c("https://www.ptt.cc",.)
  links.article %>%
    map(ScrapArticle) -> scraped
  article.table.all <- scraped%>%
    map(function(x) x$article.table)%>%
    bind_rows()
  push.table.all <- scraped%>%
    map(function(x) x$push.table)%>%
    bind_rows()
  return(list(article.table.page = article.table.all, push.table.page = push.table.all))
}

ScrapePtt <- function(pages){
  page.latest <- gossiping %>%
    html_nodes("a") %>% # extract all <a> elements
    html_attr("href") %>%  # extract the attributes `href`
    str_subset("index[0-9]{2,}\\.html") %>% # find the `href` with the index number except for 1 (index[0-9]{2,})
    str_extract("[0-9]+") %>% # extract the number
    as.numeric()+1
  page.latest
  start <- page.latest-pages+1
  test <- c(start:page.latest)
  test%>%
    map(ScrapePage) -> scraped
  article.table.all <- scraped%>%
    map(function(x) x$article.table)%>%
    bind_rows()
  push.table.all <- scraped%>%
    map(function(x) x$push.table)%>%
    bind_rows()
  return(list(article.table = article.table.all, push.table = push.table.all))
}

#======================
# END
#======================

#======================
# Exercise 2: Collect data from 3 index pages
#======================
test <- ScrapePtt(3)

#======================
# END
#======================

#======================
# Exercise 3: Collect data about COVID19
#======================


#======================
# END
#======================

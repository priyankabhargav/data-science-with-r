install.packages("reticulate")
library(reticulate)

path_to_python <- "C:\\Users\\calid\\AppData\\Local\\R-MINI~1\\envs\\R-RETI~1\\python.exe"
use_python(path_to_python)
py_install("newspaper3k")

ns <- import("newspaper")
df_news_url  <- read.csv("C:\\Users\\calid\\Desktop\\Shortages.csv", header = TRUE)

#Dataframe for results
df_results<- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_results) <- c("url", "title", "content","date","keywords")

count <- nrow(head(df_results))
 for (i in 1:count){
    print(i)
    url <- df_news_url$URL[i]
    article <- ns$Article(url)
    article$download()
    article$html
    try({
      article$parse()
      content = article$text
      print(content)
      title = article$title
      print(title)
      date = article$publish_date
      print(date)
      article$nlp()
      keyword = article$keywords
      row1 <- data.frame(url = toString(url),title = toString(title),content = toString(content), date = toString(date),keywords = toString(keywords))
      df_results<- rbind(df_results,row1)
    })
    }
  


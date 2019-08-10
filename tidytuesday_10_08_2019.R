library("data.table")
library("magrittr")
library("ggplot2")
library("viridis")
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")
bob_ross <- data.table(bob_ross) %>% 
  setnames(names(.), tolower(names(.)))

  
bob_ross[,season:=as.integer(gsub(".*S(.+)E.*", "\\1", episode))]
bob_ross %>% 
  melt(id.vars = c("episode","title","season")) %>%
  .[,sum(value),by=.(season,variable)] %>% 
  ggplot(aes(x=season, y=V1, color = variable))+geom_point()+ geom_line()


# frames------------------------------------------------------------------


bob_ross_frames <- bob_ross[, .SD, .SDcols = names(bob_ross) %like% "frame"] %>% 
  cbind(bob_ross[,.(season,title,episode)])

bob_ross_frames %>% 
  .[framed == 0,not_framed := 1] %>% 
  .[framed == 1, not_framed := 0] %>% 
  melt(id.vars=c("season","title","episode")) %>% 
  .[,sum(value),by=variable]

# content plot (no frames) (http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#5.%20Composition)------------------------------------------------------------

bob_ross_content<-bob_ross[, .SD, .SDcols = !(names(bob_ross) %like% "frame")] %>% melt(c("episode","title","season"))

bobRossCateg <- data.table(variable=unique(bob_ross_content$variable))
bobRossCateg[,category := c("sky","countryside", "beach","beach","city","city","desert","countryside",
                            "desert","sky","beach","sky","forest","sky","forest","people","city","countryside",
                            "countryside","other","countryside","countryside","countryside","people","countryside",
                            "countryside","countryside","beach","countryside","sky","mountain","mountain","other",
                            "beach","beach","countryside","people","people","countryside","mountain","other",
                            "mountain","people","city","sky","forest","forest","mountain","beach","countryside","other"
                            )]

bob_ross_content %>% merge(bobRossCateg, by="variable") %>% 
  .[,sum(value),by=.(category,variable)] %>% 
  ggplot(aes(label = variable, size = V1, color=category))+
  geom_text_wordcloud()+
  scale_size_area(max_size=20)+
  theme_minimal()+
  scale_color_viridis_d()




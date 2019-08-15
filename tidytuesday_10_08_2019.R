library("data.table")
library("magrittr")
library("ggplot2")
library("viridis")
library("ggwordcloud")

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

frame <- png::readPNG("images/bobRossFrameLandscape4.png")



p1 <- bob_ross_content %>% merge(bobRossCateg, by="variable") %>% 
  .[variable%in%c("tree","trees"), variable:="tree(s)"] %>% 
  .[variable%in%c("mountain","mountains"),variable:="mountain(s)"] %>%  
  .[,sum(value),by=.(category,variable)] %>% 
  .[,variable:=gsub("_"," ",variable)] %>% 
  .[variable =="steve ross", variable:="Steve Ross"] %>% 
  .[variable == "diane_andre", variable := "Diane Andre"] %>% 
  .[,angle:= 90 * sample(c(0, 1), nrow(.), replace = TRUE, prob = c(60, 40))] %>% 
  ggplot(aes(label = variable, size = V1, color=category, angle = angle))+
  geom_text_wordcloud()+
  scale_size_area(max_size=12)+
  theme_minimal()+
  scale_color_viridis_d(option="D", direction = -1)+
  annotation_custom(grid::rasterGrob(frame,
                    width = unit (.9, "npc"),
                    height = unit(.9, "npc")),
        -Inf, Inf, -Inf, Inf)+
  theme(legend.position = "none")#,
        #panel.background = element_rect(fill = 'grey23'))

#legend

bob_ross_content %>% merge(bobRossCateg, by="variable") %>% 
  .[,sum(value),by=.(category)] %>% 
  .[order(category)] %>% 
  ggplot(aes(x=1,label=category,y=9:1, size=V1/10, color = category))+
  geom_text()+
  scale_color_viridis_d(option="viridis", direction = -1)+
  theme_void()+
  theme(legend.position = "none",
        #panel.background = element_rect(fill = 'grey23')
        )

#pie/donut chart content

p2 <- bob_ross_content %>% merge(bobRossCateg, by="variable") %>% 
  .[,sum(value),by=.(category)] %>% 
  .[order(category)] %>% 
  .[,prop := round((V1/(.[,sum(V1)]))*100,0)] %>% 
  ggplot(aes(x=2, y = prop, fill = category))+
  geom_bar(stat = "identity", color="white")+
  coord_polar("y",start=0)+
  #geom_text(aes(y= prop, label = prop), color = "white")+
  scale_fill_viridis_d(option="viridis", direction = -1)+
  theme_void()+
  theme(legend.position = "left")+#,
        #plot.background = element_rect(fill = 'grey23'),
        #legend.text = element_text(color="white"))
  xlim(0.5,2.5)
  # theme(legend.position = "none",
  #       panel.background = element_rect(fill = 'grey23')
  # )

#pie/donut chart frames

bob_ross_frames[,.(not_framed)] %>% 
  .[,.N, by=not_framed] %>% 
  .[,prop := round((N/.[,sum(N)])*100,0)] %>% 
  .[not_framed == 1,c("type","labelpos") :=.("no frame", prop/2)] %>% 
  .[not_framed == 0,c("type","labelpos") :=.("frame", prop/2 + 87)] %>% 
  ggplot(aes(x="", y=prop, fill = type))+
  geom_bar(stat="identity", color = "white")+
  geom_text(aes(x="",label=paste0(prop,"%"), y=labelpos), color ="white")+
  theme_void()

bob_ross_frames


p3 <- ggpubr::ggarrange(p1,p2, widths = c(1.2, 0.8))
ggsave("images/bobRossResult.pdf", p3, width = 20, height = 10, unit = "cm")

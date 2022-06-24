#Load libraries
library(tidyverse)
library(readxl)
library(ggtext)


raw_data <- list.files("/Users/danielgray/Desktop/projects/childrens_books",recursive = F,pattern=".csv")
salaries <- read.csv("https://www.ons.gov.uk/visualisations/dvc1031/fig6/datadownload.csv") %>% 
        mutate(X.2 = gsub(",","",X.2),
               X.2 = as.numeric(X.2)) %>%
               select(3:4) %>%
               rename(code = 1,
                      salary = 2)

job_numbers <- read_excel("/Users/danielgray/Desktop/projects/childrens_books/ons/emp04sep2018.xls", sheet = "People") %>%
               select(1:2) %>%
               rename(job = 1, number = 2) %>%
               separate(job, into = c("code","title"), sep = "^\\S*\\K\\s+") %>%
               na.omit()

salaries <- left_join(salaries, job_numbers, by = c("code")) %>%
            mutate(number = as.numeric(number)) %>% na.omit()

setwd("/Users/danielgray/Desktop/projects/childrens_books")
final <- data.frame()

for (i in raw_data) {
  temp <- read.csv(i) %>% distinct(stem,file_name) %>% select(stem)
  final <- bind_rows(final,temp)
}

final <- final %>% group_by(stem) %>% tally()

terms <- data.frame(terms = c(
  "taxi",
  "officer",
  "police",
  "nurse",
  "librarian",
  "scientist",
  "engineer",
  "vet",
  "electrician",
  "pilot",
  "architect",
  "dentist",
  "chef",
  "waiter",
  "butcher",
  "judge",
  "doctor",
  "conductor",
  "detective",
  "DJ",
  "fisherman",
 "magician",
 "magistrate",
"mechanic",
"porter",
"servant",
"captain",
"weatherman",
"priest",
"assistant",
"sailor",
"jeweler",
"astronaut",
"detective",
"housekeeper",
"headmaster",
"brigade"
))

terms$job_name <- terms$terms
  
terms <- terms %>% mutate(
  job_name = ifelse(terms == "dentist", "dental ", job_name),
  job_name = ifelse(terms == "officer", "police", job_name),
  job_name = ifelse(terms == "detective", "police", job_name),
  job_name = ifelse(terms == "doctor", "Medical practitioners", job_name),
  job_name = ifelse(terms == "conductor", "Transport and distribution clerks and assistants", job_name),
  job_name = ifelse(terms == "DJ", "artist", job_name),
  job_name = ifelse(terms == "magician", "artist", job_name),
  job_name = ifelse(terms == "fisherman", "fishing", job_name),
  job_name = ifelse(terms == "magistrate", "judge", job_name),
  job_name = ifelse(terms == "mechanic", "mechanics", job_name),  
  job_name = ifelse(terms == "porter", " porter", job_name),
  job_name = ifelse(terms == "servant", "Cleaners and domestics", job_name),
  job_name = ifelse(terms == "captain", "Ship and hovercraft officers", job_name),
  job_name = ifelse(terms == "weatherman", "Journalist", job_name),
  job_name = ifelse(terms == "priest", "Clargy", job_name),
  job_name = ifelse(terms == "weatherman", "Journalist", job_name),
  job_name = ifelse(terms == "sailor", "Marine and waterways transport operatives", job_name),
  job_name = ifelse(terms == "jeweler", "Estimators, valuers and assessors", job_name),
  job_name = ifelse(terms == "astronaut", "pilot", job_name),
  job_name = ifelse(terms == "detective", "police", job_name),
  job_name = ifelse(terms == "headmaster", "Secondary education teaching professionals", job_name),
  job_name = ifelse(terms == "brigade", "Fire service officers", job_name)
  
  
  
  )

input <- data.frame()

for (i in terms$terms) {
  
  job_name_temp <- terms %>% filter(terms == i) %>% pull(job_name)
  
  jobs_temp <- salaries %>% filter(grepl(job_name_temp,title,ignore.case = T)) %>% summarise(salary = mean(salary,na.rm = T)) %>% pull(salary)
  mentions_temp <- final %>% filter(stem == i) %>% pull(n)
  
  temp <- data.frame(salary = c(jobs_temp),
                     number = c(mentions_temp),
                     job = c(i))
  
  input <- bind_rows(input,temp)
}

salaries$type <- "economy"

chart <- input %>% select(-c(job)) %>% mutate(type = "childrens") %>%
          bind_rows(.,salaries) %>%
          group_by(type) %>% mutate(weights = number / sum(number))

#Add in font for chart.
#font_add(famil = "mulish-2",regular = "/Users/danielgray/Desktop/projects/fpn_git/Mulish-2/static/Mulish-Regular.ttf")
#showtext::showtext_auto()


c <- ggplot(chart,aes(salary)) + 
      geom_density(aes(weight = weights ,group=type, colour = type, fill  = type),adjust = 1.3, alpha = 0.1) +
  theme(
   # text = element_text(family = "mulish-2"),
    axis.title.x = element_text(angle = 0),
    axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")),
    axis.text.y=element_blank(), #remove x axis labels
    axis.ticks.y=element_blank(),
    axis.text = element_text(size=12),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="black" ),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    legend.position = "none",
    plot.subtitle = element_markdown()) + 
  labs(title = "Characters in children's books typically work in higher-paid professions.",
       subtitle = "Salary distribution for <span style='color:#F8766D;'>characters in children's books</span> 
    and<span style='color:#00BFC4;'> people in the actual UK economy.</span>",
       y = "Rare                             Common",
       x = "Typical annual salary",
       caption = "\n Produced by @DataLobo. \n Data on the words included in children's books from Dawson et al. 2020 study 'Features of lexical richness in children’s books: \n Comparisons with child-directed speech'. We then manually identified 37 different jobs included in these 160 texts. \n Only one reference to a job is counted per book (e.g. 'doctor' is the most referenced profession, appearing in 14 of 160 books. \n Data on salary distribution of UK economy from ONS. This distribution includes 332 different jobs.") + 
  geom_curve(aes(x = 40000, y = 0.000032, xend = 26000, yend = 0.000025),
         arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_curve(aes(x = 75000, y = 0.00001, xend = 65000, yend = 0.000005),
             arrow = arrow(length = unit(0.03, "npc")),curvature = -0.5) +
  annotate("text", x=50000, y=0.0000335, label= "Care-workers,cleaners and \n check-out assistants are \n under-represented in \n children's books... ") + 
  annotate("text", x = 75000, y=0.000015, label = "... meanwhile doctors, pilots \n and police-officers are \n over-represented.")

library(ggtext)

plot_row <- plot_grid(p1,p2,p3,p4,nrow = 1, rel_widths = c(0.28, 0.205, 0.205,0.205))
footnote_row <- plot_grid(footnote,logo, ncol = 2,rel_widths= c(0.8,0.25),hjust = -1)
vis_2 <- plot_grid(title,plot_row,footnote_row,buffer, ncol = 1,rel_heights = c(0.1, 0.8,0.15,0.05))

ggplot(input, aes(salary)) + geom_density(aes(weights=mentions/sum(mentions)))
ggplot(data = input, aes(x = mentions, y = salary)) + geom_point()
 
# Books help shape children's aspirations, and tend to feature well-paid, prestigious professions such as doctors and pilots. But they under-represent less well-paid professions such as carers and cleaners. 
# That perhaps contributes to the under-appreciation of these crucial and female-dominated jobs?



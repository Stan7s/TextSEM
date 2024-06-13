## code to prepare `prof1000` dataset goes here

prof1000 <- read.csv("data/prof1000.original.csv", stringsAsFactors = FALSE)
str(prof1000)

prof.tm <- unnest_tokens(prof1000, word, comments)

male.words <- tibble(word=c("he", "him", "his", "he's", "he'd", "he'll", "mr"))
female.words <- tibble(word=c("she", "her", "hers", "she's", "she'd", "she'll", "ms", "mrs", "miss"))

male.info <- inner_join(prof.tm, male.words)
female.info <- inner_join(prof.tm, female.words)

male.score <- male.info %>% group_by(profid) %>% summarize(n=n())
female.score <- female.info %>% group_by(profid) %>% summarize(n=n())

gender.info <- full_join(male.score, female.score, by = c("profid")) %>%
  replace(., is.na(.), 0) %>% rename(total.m = n.x, total.f = n.y)


prof.gender <- gender.info %>% filter(total.m != 0 & total.f !=0)

gender.info <- gender.info %>%
  mutate(gender = ifelse(total.m > total.f, "M", "F"))

prof1000 <- left_join(prof1000, gender.info)

prof1000$total.f <- NULL
prof1000$total.m <- NULL

prof1000$gender <- ifelse(prof1000$gender=='M', 1, 0)

str(prof1000)
usethis::use_data(prof1000, overwrite = TRUE)

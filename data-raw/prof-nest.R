## code to prepare `prof.nest` dataset goes here
data(prof1000)
prof.nest <- prof1000 %>% group_by(profid) %>%
summarise(comments = paste(comments, collapse = " "),
  rating = mean(rating, na.rm = TRUE), difficulty=mean(difficulty, na.rm = TRUE),
  book = mean(book, na.rm = TRUE), grade=mean(grade, na.rm = TRUE))

usethis::use_data(prof.nest, overwrite = TRUE)

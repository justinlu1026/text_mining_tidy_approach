library(dplyr)
library(tidytext)

# 1.2 The unnext_tokens function ----

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

text_df <- data_frame(line = seq_along(text), text = text)

# Two basic arguments to unnest_tokens:
# output column name: word
# input column:
text_df %>% unnest_tokens(word, text)

# 1.3 Tidying the works of Jane Austen ----

library(janeaustenr)
library(dplyr)
library(stringr)

# \\divxlc are Roman numerals in the chapter title
# e.g. "Chapter XXIII"
(original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup())

data("stop_words")

(tidy_books <- original_books %>% 
  unnest_tokens(word, text))

tidy_books <- tidy_books %>% anti_join(stop_words)

tidy_books %>% count(word, sort = TRUE)

library(ggplot2)
tidy_books %>% 
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() + 
  xlab(NULL) + coord_flip()


# 1.4 The gutenbergr package ----
# The package provides access to the publci domain works from the Project Gutenberg collection
# inlude toosl both for downloading books (stripingg out the unhelpful header/footer information)
# an a complete dataset of Project Gutenberg metadata to find works of interest

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)


bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)


library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

library(scales)

ggplot(frequency %>% head(5000), aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 0) +
  scale_x_log10(label = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency %>% filter(author == "Brontë Sisters"), ~ proportion + `Jane Austen`)
cor.test(data = frequency %>% filter(author == "H.G. Wells"), ~ proportion + `Jane Austen`)

# Bulk rename filenames in the example scripts

library("stringr")
all_files <- list.files("examples", recursive = TRUE, full.names = TRUE)

numbered_examples <- all_files %>%
  str_subset("Ch\\d+/\\d+[.]\\d+")

new_names <- numbered_examples %>%
  # "Ch02/2.3" => "Ch02/02.3" so file's chapter num matches folder's num
  str_replace("(/Ch)(\\d+)(/)(\\d+)([.])", "\\1\\2\\3\\2\\5") %>%
  # "/16.6_" => "/16.06_" so filename-order matches presentation-order
  str_replace("(/\\d+[.])(\\d_)", "\\10\\2") %>%
  # "/16.06_" => "/16-06_" bc we don't want dots in filenames
  str_replace("(/\\d+)[.](\\d+_)", "\\1-\\2") %>%
  # Remove other symbols
  str_replace_all("[&]", "And") %>%
  str_replace_all("\\^2", "Squared")

file.rename(numbered_examples, to = new_names)

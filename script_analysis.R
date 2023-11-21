# install and open the pdftools package
library("pdftools")
library("stringr")

# store link to Parasite script and download it to local directory
pdf_file <- "https://8flix.com/assets/screenplays/p/tt6751668/Parasite-2019-screenplay-by-Bong-Joon-Ho-and-Jin-Won-Han.pdf"
download.file(pdf_file, destfile = "parasite_script.pdf", mode = "wb")

# extract each page of the script
parasite_pages <- pdftools::pdf_text("parasite_script.pdf")

# extract the text from their corresponding pages
parasite_text <- unlist(parasite_pages)

# vector to store each character's word count
num_words <- c()
kim_family <- c("KI-TEK", "CHUNG-SOOK", "KI-WOO", "KI-JUNG")

# get the word count for each character
for (member in kim_family) {
  word_count <- 0
  page <- 1
  # expression used to search for the character names
  name_search <- paste0("(?s)", member, "(\\s*)(.*?)([:punct:]\\n\\n)")
  # obtain all the lines spoken by the character
  all_lines <- str_match_all(parasite_text, name_search)
  while (page <= length(all_lines)) {
    cur_row <- 1
    while (cur_row <= length(all_lines[[page]][, 3])) {
      # check the first match of the page
      cur_line <- all_lines[[page]][cur_row, 1]
      # if there is any punctuation immediately after the character's name, then
      # the match is a scene change, not a line
      if (str_detect(cur_line, paste0(member, "[:punct:]"))) {
        cur_row <- cur_row + 1
        next
      }
      # change to the actual line of dialogue
      cur_line <- all_lines[[page]][cur_row, 3]
      # first removes any acting directions in parentheses
      new_line <- str_remove_all(cur_line, "\\((.*)\\)")
      # then removes all punctuation to get proper word segments
      new_line <- str_remove_all(new_line, "[:punct:]")
      word_count <- word_count + str_count(new_line, "\\w+")
      cur_row <- cur_row + 1
    }
    page <- page + 1
  }
  num_words <- append(num_words, word_count)
}

# vectors to store number of pages and lines for each character
num_pages <- c()
num_lines <- c()

for (member in kim_family) {
  # vector of integers that corresponds to the number of occurrences of each
  # string, or name in this case
  # index of the vector represents the page number
  member_pages <- str_count(parasite_text, member)
  # finds number of pages where character appears
  num_pages <- append(num_pages, sum(member_pages > 0))
  # finds the number of lines of each character
  # need to subtract 1 since first occurrence of their name is in uppercase
  # but is only a description
  num_lines <- append(num_lines, (sum(member_pages) - 1))
}

# create data frame with totals for each character
kim_family <- c("Ki-Taek", "Chung-Sook", "Ki-Woo", "Ki-Jung")
parasite_df <- data.frame(kim_family, num_pages, num_lines, num_words)

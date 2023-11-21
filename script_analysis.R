# install and open the pdftools package
library("pdftools")
library("stringr")

# store link to Parasite script and download it to local directory
pdf.file <- "https://8flix.com/assets/screenplays/p/tt6751668/Parasite-2019-screenplay-by-Bong-Joon-Ho-and-Jin-Won-Han.pdf"
download.file(pdf.file, destfile = "parasite_script.pdf", mode = "wb")

# extract each page of the script
parasite.pages <- pdftools::pdf_text("parasite_script.pdf")

# extract the text from their corresponding pages
parasite.text <- unlist(parasite.pages)

# vector to store each character's word count
num.words <- c()
kim.family <- c("KI-TEK", "CHUNG-SOOK", "KI-WOO", "KI-JUNG")

# get the word count for each character
for (member in kim.family) {
  word.count <- 0
  page <- 1
  # expression used to search for the character names
  name.search <- paste0("(?s)", member, "(\\s*)(.*?)([:punct:]\\n\\n)")
  # obtain all the lines spoken by the character
  all.lines <- str_match_all(parasite.text, name.search)
  while (page <= length(all.lines)) {
    cur.row <- 1
    while (cur.row <= length(all.lines[[page]][,3])) {
      # check the first match of the page
      cur.line <- all.lines[[page]][cur.row,1]
      # if there is any punctuation immediately after the character's name, then
      # the match is a scene change, not a line
      if (str_detect(cur.line, paste0(member, "[:punct:]"))) {
        cur.row <- cur.row + 1
        next
      }
      # change to the actual line of dialogue
      cur.line <- all.lines[[page]][cur.row,3]
      # first removes any acting directions in parentheses
      new.line <- str_remove_all(cur.line, "\\((.*)\\)")
      # then removes all punctuation to get proper word segments
      new.line <- str_remove_all(new.line, "[:punct:]")
      word.count <- word.count + str_count(new.line, "\\w+")
      cur.row <- cur.row + 1
    }
    page <- page + 1
  }
  num.words <- append(num.words, word.count)
}

# vectors to store number of pages and lines for each character 
num.pages <- c()
num.lines <- c()

for (member in kim.family) {
  # vector of integers that corresponds to the number of occurrences of each
  # string, or name in this case
  # index of the vector represents the page number
  member.pages <- str_count(parasite.text, member)
  # finds number of pages where character appears
  num.pages <- append(num.pages, sum(member.pages > 0))
  # finds the number of lines of each character
  # need to subtract 1 since first occurrence of their name is in uppercase 
  # but is only a description
  num.lines <- append(num.lines, (sum(member.pages) - 1))
}

# create data frame with totals for each character
kim.family <- c("Ki-Taek", "Chung-Sook", "Ki-Woo", "Ki-Jung")
parasite.df <- data.frame(kim.family, num.pages, num.lines, num.words)
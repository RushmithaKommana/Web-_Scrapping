# Web Scraping Using R : Extracting and Analyzing Journal Article Data
# Journal : Radiation Oncology 


# Task 1: Set Up Your R Environment


# Install & load packages (run only once)

install.packages("rvest")
install.packages("httr")
install.packages("xml2")
install.packages("dplyr")
install.packages("wordcloud2")

library(rvest)
library(httr)
library(xml2)
library(dplyr)
library(ggplot2)
library(wordcloud2)


# Task 2: Scraping Article Data : Title, Authors, Correspondence Author, Correspondence Author's Email, Publish Date, Abstract, Keywords.



# Step 1: Extract volume info (volume number and year)
# volumes with corresponsing years

url <- "https://ro-journal.biomedcentral.com"
query_url <- "https://ro-journal.biomedcentral.com/articles?query=&volume=&searchType=&tab=keyword"
page <- read_html(query_url)

volume_data <- page %>% html_nodes("#volume-from > option") %>% html_text()
volume_numbers <- gsub("Volume (\\d+) \\(\\d+\\)", "\\1", volume_data) %>% as.numeric()
volume_years <- gsub("Volume \\d+ \\((\\d+)\\)", "\\1", volume_data) %>% as.numeric()

volume_df <- data.frame(
  volume_number = volume_numbers,
  volume_year = volume_years
) %>% na.omit() %>% arrange(desc(volume_year))

print(volume_df)


# Step 2: Ask for input year from user


year_input <- as.numeric(readline(prompt = "Enter the year to scrape articles from: "))

# checking if the input year is present in the volumes or not
if (!(year_input %in% volume_df$volume_year)) {
  stop(paste("Year", year_input, "not found in available volumes."))
}


# Step 3: Define scraping functions


safe_text <- function(node) {
  if (length(node) > 0) html_text(node, trim = TRUE) else NA
}


page_scraping <- function(link) {
  page <- read_html(link)
  title <- safe_text(html_node(page, "h1.c-article-title")) 
  if (is.na(title)) title <- safe_text(html_node(page, "h1.Title")) 
  
  authors <- page %>%
    html_nodes("li.c-article-author-list__item") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = ", ")
  
  correspondence_info <- page %>%
    html_node("#corresponding-author-list a") %>%
    html_text()
  
  correspondence_email <- page %>%
    html_nodes("a[href^='mailto:']") %>%
    html_attr("href") %>%
    gsub("mailto:", "", .) %>%
    unique() %>%
    paste(collapse = ", ")
  
  publish_date <- page %>%
    html_node("time") %>%
    html_text(trim = TRUE)
  
  abstract <- safe_text(html_node(page, ".Abstract"))
  if (is.na(abstract)) {
    abstract <- safe_text(html_node(page, ".c-article-section__content"))
  }
  if (is.na(abstract)) {
    abstract <- page %>%
      html_nodes("section") %>%
      html_text(trim = TRUE) %>%
      .[grepl("Abstract", ., ignore.case = TRUE)][1]
  }
  
  keywords <- page %>%
    html_nodes(".c-article-subject-list__subject, .Keyword, .c-article-subject-list span") %>%
    html_text(trim = TRUE) %>%
    unique() %>%
    paste(collapse = ", ")
  
  return(data.frame(
    title = title,
    authors = authors,
    correspondence_author = paste(correspondence_info, collapse = ", "),
    correspondence_email = correspondence_email,
    publish_date = publish_date,
    abstract = abstract,
    keywords = keywords,
    stringsAsFactors = FALSE
  ))
}



#test_url <- "https://ro-journal.biomedcentral.com/articles/10.1186/1748-717X-2-45"
#result <- page_scraping(test_url)
#print(result)



# Step 4: Scrape articles for selected year

extract_article_links <- function(full_page_url) {
  page <- read_html(full_page_url)
  links <- page %>%
    html_nodes(".c-listing__title a") %>%
    html_attr("href") %>%
    paste0(url, .)
  return(links)
}


scrape_articles_by_year <- function(year) {
  volumes_to_scrape <- volume_df %>% filter(volume_year == year)
  
  if (nrow(volumes_to_scrape) == 0) {
    cat("No volumes found for year", year, "\n")
    return(data.frame())
  }
  
  all_articles <- data.frame()
  
  for (volume in volumes_to_scrape$volume_number) {
    cat("Scraping Volume:", volume, "\n")
    page_num <- 1
    
    repeat {
      page_url <- paste0(url, "/articles?searchType=journalSearch&volume=", volume, "&page=", page_num)
      cat(" → Page:", page_num, "\n")
      
      article_links <- extract_article_links(page_url)
      if (length(article_links) == 0) break
      
      for (link in article_links) {
        cat("   ↳ Article:", link, "\n")
        Sys.sleep(1)
        article_data <- tryCatch({
          page_scraping(link)
        }, error = function(e) {
          message("Failed to scrape:", link)
          return(NULL)
        })
        if (!is.null(article_data)) {
          all_articles <- rbind(all_articles, article_data)
        }
      }
      
      # Check for next page
      next_check <- read_html(page_url) %>% html_node("li.c-pagination__item a[data-test='next-page']")
      if (is.na(next_check)) break
      page_num <- page_num + 1
    }
  }
  return(all_articles)
}


# Step 5: Run scraping and output the article links which are scraped with given year

result_articles <- scrape_articles_by_year(year_input)
print(result_articles)




# Task 3: Data Cleaning and Preprocessing


# Remove rows where title is missing (not useful articles)
cleaned_data <- result_articles %>% filter(!is.na(title) & title != "")

# Replace NA values with "Not Available" (or leave as NA if needed)
cleaned_data[is.na(cleaned_data)] <- "Not Available"

# Preview cleaned data
print(head(cleaned_data))




# Task 4: Data Analysis and Visualization


# Visualization 1: Creating a bar plot for the top 10 keyword frequencies


# Manually split the keywords and clean up any whitespace and creating data frame with keywords and frequencies
extracted_keywords <- unlist(strsplit(cleaned_data$keywords, ",\\s*"))
keyword_freq_table <- table(extracted_keywords)
keyword_freq_df <- data.frame(keyword = names(keyword_freq_table),
                              frequency = as.numeric(keyword_freq_table))


# Sort the data frame by frequency in descending order (optional)
keyword_freq_df <- keyword_freq_df[order(-keyword_freq_df$frequency), ]
top_n_keywords <- keyword_freq_df[1:10, ]

# Plotting the top 10 keywords using bar plot
ggplot(top_n_keywords, aes(x =frequency , y = reorder(keyword, -frequency))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Keyword Frequency", x = "Keyword", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  theme_minimal()



# Visualization 2: Displaying top 10 authors by no of articles 


# Extract and clean author names
clean_author_names <- function(author_string) {
  # Remove numeric superscripts (e.g., 1,2,3)
  cleaned <- gsub("[0-9]+(,[0-9]+)*", "", author_string)
  
  # Remove stray ampersands and trailing commas
  cleaned <- gsub("&", "", cleaned)
  cleaned <- gsub(",+", ",", cleaned)  # Collapse multiple commas into one
  cleaned <- gsub("^\\s*,|,\\s*$", "", cleaned)  # Remove leading/trailing commas
  cleaned <- gsub("\\s{2,}", " ", cleaned)  # Normalize extra spaces
  
  # Split and trim names
  authors <- unlist(strsplit(cleaned, ","))
  authors <- trimws(authors)
  
  # Remove any empty entries
  authors <- authors[authors != ""]
  
  return(authors)
}

author_list <- unlist(lapply(cleaned_data$authors, clean_author_names))

# Count occurrences of each author
author_counts <- table(author_list)
author_counts_df <- as.data.frame(author_counts, stringsAsFactors = FALSE)
colnames(author_counts_df) <- c("Author", "Article_Count")
author_counts_df$Author <- gsub("\\)\\)|list\\(c\\(|\\)|c\\(|\\)|list\\(", "", author_counts_df$Author)
author_counts_df$Article_Count <- as.numeric(author_counts_df$Article_Count)

# Group by Author (if needed — usually not for table output)
author_counts_final <- author_counts_df %>%
  group_by(Author) %>%
  summarize(Total_Articles_Count = sum(Article_Count), .groups = "drop")

# Sorting authors by the number of articles
author_counts_final <- author_counts_final[order(-author_counts_final$Total_Articles_Count), ]

# Plotting the top 10 authors
bar_chart_authors <- ggplot(head(author_counts_final, 10), aes(x = reorder(Author, Total_Articles_Count), y = Total_Articles_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") + coord_flip() + labs(x = "Author", y = "Number of Articles", title = "Top 10 Authors by Number of Articles") +
  theme_minimal()

print(bar_chart_authors)


  
# Visualization 3 : Displaying no of articles published per month for the given year
  

# Convert to Date format
publish_dates <- unlist(cleaned_data$publish_date)
publish_dates <- as.Date(publish_dates, format = "%d %B %Y")
cleaned_data$publish_date <- publish_dates

# Histogram of publish dates
histogram_publish_date <- ggplot(cleaned_data, aes(x = publish_date)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(x = "Publish Date", y = "Frequency", title = "Histogram of Publish Dates")

print(histogram_publish_date)



# Visualization : Displaying no of articles published for that year 

# Extract the year from the publish_date
#cleaned_data$year <- format(cleaned_data$publish_date, "%Y")

# Count the number of articles per year
#article_count_by_year <- cleaned_data %>%
#  group_by(year) %>%
#  summarize(Article_Count = n())

# Bar plot of articles per year
#ggplot(article_count_by_year, aes(x = year, y = Article_Count)) +
#  geom_bar(stat = "identity", fill = "lightblue") +
#  labs(x = "Year", y = "Number of Articles", title = "Number of Articles Published by Year") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Visualization 4 : Creating word cloud of top keywords 


# Create a word cloud of top keywords
wordcloud_keywords <- wordcloud2(keyword_freq_df[1:100, ], 
                                 size = 1.5, 
                                 color = "random-light", 
                                 backgroundColor = "black")

# Display the word cloud
wordcloud_keywords


#install.packages("writexl")
#library(writexl)
#write_xlsx(cleaned_data, "cleaned_data.xlsx")

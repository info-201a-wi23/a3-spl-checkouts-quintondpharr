# Calculate the average number of checkouts for each item
avg_checkouts_by_item <- spl_df %>% 
     group_by(Title) %>% 
     summarise(avg_checkouts = mean(Checkouts, na.rm = TRUE)) %>% 
     summarise(avg = mean(avg_checkouts)) %>% 
     pull(avg)
#print(paste("Average number of checkouts for each item: ", mean(spl_df$Checkouts, na.rm = TRUE)))

# Find the rows for the book "Scythe: Arc of a Scythe Series, Book 1"
scythe_checkouts <- spl_df %>% 
     filter(grepl("Scythe.*Arc of a Scythe Series, Book 1", Title, ignore.case = TRUE))
#print(paste("scythe_checkouts", scythe_checkouts$Checkouts))


# Identify the month with the most checkouts for "Scythe: Arc of a Scythe Series, Book 1"
most_checked_out_month_scythe <- scythe_checkouts %>% 
     group_by(CheckoutMonth) %>% 
     summarise(total_checkouts = sum(Checkouts)) %>% 
     arrange(desc(total_checkouts)) %>% 
     slice(1) %>% 
     pull(CheckoutMonth)
#print(paste("most_checked_out_month_scythe", most_checked_out_month_scythe))

# Identify the month with the least checkouts for "Scythe: Arc of a Scythe Series, Book 1"
least_checked_out_month_scythe <- scythe_checkouts %>% 
     group_by(CheckoutMonth) %>% 
     summarise(total_checkouts = sum(Checkouts)) %>% 
     arrange(total_checkouts) %>% 
     slice(1)%>% 
     pull(CheckoutMonth)
#print(paste("least_checked_out_month_scythe", least_checked_out_month_scythe))

# Identify the month with the most checkouts for ebooks
most_checked_out_month_ebooks <- spl_df %>% 
     filter(MaterialType == "EBOOK") %>% 
     group_by(CheckoutMonth) %>% 
     summarise(total_checkouts = sum(Checkouts)) %>% 
     arrange(desc(total_checkouts)) %>% 
     slice(1)%>% 
     pull(CheckoutMonth)
#print(paste("most_checked_out_month_ebooks", most_checked_out_month_ebooks))

most_checked_out_month_ebooks_num <- spl_df %>%
     filter(MaterialType == "EBOOK") %>%
     group_by(CheckoutMonth) %>%
     summarise(total_checkouts = sum(Checkouts)) %>%
     arrange(desc(total_checkouts)) %>%
     slice(1)%>%
     pull(total_checkouts)

# Identify the month with the least checkouts for ebooks
least_checked_out_month_ebooks <- spl_df %>% 
     filter(MaterialType == "EBOOK") %>% 
     group_by(CheckoutMonth) %>% 
     summarise(total_checkouts = sum(Checkouts)) %>% 
     arrange(total_checkouts) %>% 
     slice(1)%>% 
     pull(CheckoutMonth)
# print(paste("least_checked_out_month_ebooks", least_checked_out_month_ebooks))

least_checked_out_month_ebooks_num <- spl_df %>% 
     filter(MaterialType == "EBOOK") %>% 
     group_by(CheckoutMonth) %>% 
     summarise(total_checkouts = sum(Checkouts)) %>% 
     arrange(total_checkouts) %>% 
     slice(1)%>% 
     pull(total_checkouts)

# Calculate the number of print book checkouts by month
print_book_checkouts_by_month_high <- spl_df %>% 
     filter(MaterialType == "BOOK", UsageClass == "Physical") %>% 
     group_by(CheckoutMonth) %>% 
     summarise(total_checkouts = sum(Checkouts))%>% 
     filter(total_checkouts == max(total_checkouts)) %>% 
     pull(total_checkouts)

print_book_checkouts_by_month_low <- spl_df %>% 
     filter(MaterialType == "BOOK", UsageClass == "Physical") %>% 
     group_by(CheckoutMonth) %>% 
     summarise(total_checkouts = sum(Checkouts))%>% 
     filter(total_checkouts == min(total_checkouts)) %>% 
     pull(total_checkouts)
#print(paste("print_book_checkouts_by_month", print_book_checkouts_by_month))

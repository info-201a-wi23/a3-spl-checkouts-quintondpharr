# Calculate the total number of checkouts for print books and ebooks by month
checkout_totals_by_month <- spl_df %>% 
     group_by(CheckoutMonth, MaterialType, UsageClass) %>% 
     summarise(total_checkouts = sum(Checkouts))

# Filter for only print books and ebooks
checkout_totals_by_month_books_ebooks <- checkout_totals_by_month %>%
     filter(MaterialType %in% c("BOOK", "EBOOK", "SOUNDDISC", "AUDIOBOOK"))

# Create the plot
ggplot(checkout_totals_by_month_books_ebooks, aes(x = CheckoutMonth, y = total_checkouts, color = MaterialType)) + 
     geom_line(size = 1) +
     labs(x = "Month", y = "Total Checkouts Per Month", color = "Material Type") +
     ggtitle("Total Checkouts by Month for each type of book for 2022/23") +
     scale_x_continuous(breaks = seq(1, 12, 1)) +
     scale_y_continuous(labels = label_number_si())

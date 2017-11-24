#-----------------------------------------------------------------------------#
# File description:
# Re-name the city coefficients for IN and LA. Ideally, this would have been
# done earlier, in the correct file, but for now, I am doing this here.
# Input: 
# Output:
#-----------------------------------------------------------------------------#

# Indiana
load("./data/URLs_IN.rdata")
URLs$foldername[URLs$foldername=="www.batesvilleindiana.us"] <- "batesvilleindiana.us"
URLs$foldername[URLs$foldername=="www.bloomington.in.gov"] <- "bloomington.in.gov"
URLs$foldername[URLs$foldername=="www.brazil.in.gov"] <- "brazil.in.gov"
URLs$foldername[URLs$foldername=="www.elwoodcity-in.org"] <- "elwoodcity-in.org"
URLs <- subset(URLs, select = c("Name", "Year", "winner", "foldername"))
names(URLs) <- c("City", "Year", "Party", "Website")
indiana.coefficients <- URLs
save(indiana.coefficients, file = "rfiles/city_coefficients_indiana.rdata")
saveRDS(indiana.coefficients, file = "rfiles/city_coefficients_indiana.rds")

# Louisiana
load("./data/louisiana.rdata")
louisiana$Website <- str_extract(louisiana$Website, "//(.*)") %>%
  str_replace_all("/", "")
names(louisiana) <- c("City", "Year", "Party", "Website")
louisiana.coefficients <- louisiana
save(louisiana.coefficients, file = "rfiles/city_coefficients_louisiana.rdata")
saveRDS(louisiana.coefficients, file = "rfiles/city_coefficients_louisiana.rds")

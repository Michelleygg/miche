#Question 1
theta <- seq(0,6*pi,by=0.1)
x <- exp(0.2*theta)*cos(theta)
y <- exp(0.2*theta)*sin(theta)
library(ggplot2)
ggplot(data.frame(x,y), aes(x=x,y=y))+geom_path()
plot(x,y,type="l", asp=1)

#Question 2
hawker_dist <- readRDS("C:/Users/admin/Downloads/hawk_ctr_distances.rds")
two_step_v1 <- function(h_name, h_dists) {
  #change '==' to '!', to prevent incorrect referencing
  if (!h_name %in% colnames(h_dists)) {
    #use return instead of break
    break
  }
  else {
    #incorrect data name, h_dist instead of h_dists
    #Default of sort function is false, no need to indicate
    step_1 <- names(sort(h_dist[h_name,], FALSE)[2])
    #use assignment operator
    h_dist[step_1, h_name] = 0
    step_2 <- names(sort(h_dist[step_1,], FALSE)[3])
    return(step_2)
  }
}
two_step_v2 <- function(h_name, h_dists) {
  #changes class of matrix to characters. 
  #Use NA instead to keep the matrix as numeric.
  h_dists[which(h_dists == 0)] <- NA_character_
  if (h_name %in% rownames(h_dists)) {
    rowstep1 <- match(h_name, rownames(h_dists))
    colstep1 <- which.min(h_dists[rowstep1, ])
    step1 <- rownames(h_dists)[colstep1]
    rowstep2 <- match(step1, rownames(h_dists))
    colstep2 <- match(sort(h_dists[rowstep2, ])[2], h_dists[rowstep2, ])
    step2 <- rownames(h_dists)[colstep2]
    #return
    print(step2)
  } else {
    stop("Hawker Name incorrect!!")
  }
}
# Example call:
two_step_v2("Blk 93 Toa Payoh Lorong 4", hawker_dist)

isSymmetric(hawker_dist)
#return array index of of regular r indexing
which(hawker_dist == 0, arr.ind = TRUE)
#get value close to 0 but not equal 0 in decimal places
which(hawker_dist <= 1*exp(-6), arr.ind = TRUE)

wine_data <- readRDS("C:/Users/admin/Downloads/wine_titles.rds")
#remove misaligned data
wine_data <- wine_data[!(str_detect(wine_data$taster_twitter_handle, "^@",TRUE)),]
#remove data rows with NA
wine_data <- na.omit(wine_data)
#adding new regions column
regions <- str_extract_all(wine_data$title, "(?<=\\().+?(?=\\))")
wine_data$regions = regions

#extracting years(4 digit numbers) from title
wine_years <- wine_data[!(str_detect(wine_data$title, "[:digit:]{4}",TRUE)),]
years <- as.data.frame(str_extract_all(wine_years, "\\d{4}",simplify = FALSE))
#some titles have more than 1 year, causing error: arguments imply differing number of rows
library(drake)

loadd(data)
names(data)

purrr::map(data[,-1], ~sum(!is.na(.)))

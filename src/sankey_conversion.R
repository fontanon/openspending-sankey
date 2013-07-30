# Functions defined for transforming data
# =======================================

# tJSONarray converts dataframe into D3.js JSON objects
# Found at: http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis/
toJSONarray = function(dtf){
  clnms <- colnames(dtf)
  
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric'){
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, collapse=', ')})
  objs <- paste('{', objs, '}')
  
  res <- paste('[', paste(objs, collapse=', '), ']')
  
  return(res)
}

# Trim whitespaces
trim <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)

# Load data and aggregate data
# ============================
library("rjson")

json_cofog1_url <- "http://openspending.org/api/2/aggregate?dataset=ukgov-finances-cra&cut=time.year:2010&drilldown=cofog1"
json_cofog1_data <- fromJSON(file=json_cofog1_url, method="C")
cofog1 <- data.frame(cbind(
  "UKGov", 
  matrix(sapply(sapply(json_cofog1_data$drilldown, "[", c(3)), "[", c(5))), 
  matrix(sapply(json_cofog1_data$drilldown, "[", c(1)))
))
names(cofog1) <- c("source", "target", "value")

json_cofog2_url <- "http://openspending.org/api/2/aggregate?dataset=ukgov-finances-cra&cut=time.year:2010&drilldown=cofog1|cofog2"
json_cofog2_data <- fromJSON(file=json_cofog2_url, method="C")
cofog2 <- data.frame(cbind(
  matrix(sapply(sapply(json_cofog2_data$drilldown, "[", c(4)), "[", c(5))), 
  matrix(sapply(sapply(json_cofog2_data$drilldown, "[", c(3)), "[", c(5))), 
  matrix(sapply(json_cofog2_data$drilldown, "[", c(1)))
))
names(cofog2) <- c("source", "target", "value")

json_cofog3_url <- "http://openspending.org/api/2/aggregate?dataset=ukgov-finances-cra&cut=time.year:2010&drilldown=cofog2|cofog3"
json_cofog3_data <- fromJSON(file=json_cofog3_url, method="C")
cofog3 <- data.frame(cbind(
  matrix(sapply(sapply(json_cofog3_data$drilldown, "[", c(3)), "[", c(5))), 
  matrix(sapply(sapply(json_cofog3_data$drilldown, "[", c(4)), "[", c(5))), 
  matrix(sapply(json_cofog3_data$drilldown, "[", c(1)))
))
names(cofog3) <- c("source", "target", "value")

# Producing nodes and links D3.js JSON data
# ===========================================
  
# Initialization of D3.js nodes dataframe
nodes <- union("UKGOV", union(cofog1$target,union(cofog2$target, cofog3$target)))
nodes <- data.frame(cbind(seq(1:length(nodes)), nodes))
names(nodes) <- c("cod", "name")
nodes$name <- trim(nodes$name)

# Initialization of D3.js links dataframe
links <- rbind(cofog1, cofog2, cofog3)
links$source <- trim(links$source)
links$target <- trim(links$target)

# Here comes the magic: merging datasets for replacing names with codes
links <- merge(links, nodes, by.x="source", by.y="name")
links <- merge(links, nodes, by.x="target", by.y="name")
links <- links[,c("cod.x", "cod.y", "value")]
names(links) <- c("source","target","value")
links$source <- as.numeric(links$source)-1
links$target <- as.numeric(links$target)-1
links$value <- as.numeric(links$value)

nodes <- data.frame(nodes[,c("name")])
names(nodes) <- c("name")

# Output to JSON D3.js compatible format
output <- paste('{"nodes":', toJSONarray(nodes), ',')
output <- paste(output, '"links":', toJSONarray(links), '}')
write(output, "openspending.json")
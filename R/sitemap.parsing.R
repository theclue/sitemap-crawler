suppressMessages(library("rprojroot"))

if(!interactive()) setwd(find_root_file(criterion = is_git_root))

source(find_root_file("R", "setup.R", criterion = is_git_root))

##############
# PARAMETERS 
##

sitemap.url <- "https://www.opel.it/sitemap.xml"

sitemap <- read_xml(sitemap.url)

urls <- sapply(xml_children(sitemap), function(x){
  x %>% xml_contents() %>% xml_text()
})

urls <- urls[!grepl("baselball", urls)]

##################
# CRAWLING
##

pnames <- lapply(urls, function(url){
  
  flog.info(sprintf("Parsing page: %s", url))
  
  url.proxy <- sprintf("https://api.proxycrawl.com/?token=%s&url=%s", proxy.crawl, URLencode(url))

  r <- GET(url.proxy)
  status <- status_code(r)

  pagename <- ""
  
  if(status != 500){
    
    page <- read_html(r)

    data.layer <- page %>% html_nodes(xpath = "//script[contains(., \"window.digitalData\")]") %>% html_text()
    
    if(length(data.layer) > 0){
      data.layer.split <- strsplit(data.layer, "\n")[[1]]
      
      data.layer.clean <- gsub("[\r\n\t;]+", "", data.layer.split[grepl("digitalData\\..*\\s=\\s", data.layer.split)])
      data.layer.clean <- data.layer.clean[!grepl("viewport|internalSearchNoOfResults|articleName|zipCode|url|siteSectionsLevel5", data.layer.clean)]
      data.layer.clean <- data.layer.clean[grepl("siteSections", data.layer.clean)]
      
      eval(parse(text=data.layer.clean))
      
      pagename <- c("opel:IT:IT:it:t1",
                    ifelse(grepl("undefined", digitalData.pageInfo.siteSectionsLevel1), NA, digitalData.pageInfo.siteSectionsLevel1),
                    ifelse(grepl("undefined", digitalData.pageInfo.siteSectionsLevel2), NA, digitalData.pageInfo.siteSectionsLevel2),
                    ifelse(grepl("undefined", digitalData.pageInfo.siteSectionsLevel3), NA, digitalData.pageInfo.siteSectionsLevel3),
                    ifelse(grepl("undefined", digitalData.pageInfo.siteSectionsLevel4), NA, digitalData.pageInfo.siteSectionsLevel4))
      
      pagename <- paste(pagename[!is.na(pagename)], collapse = ":")
    } else{ pagename <- NA}
    
  }
  
  else{ pagename <- NA}
  
  return(data.frame(url, pagename, stringsAsFactors = FALSE))
})

ff <- do.call(plyr::rbind.fill,pnames)

sitemap.parsed <- data.frame(url = urls, stringsAsFactors = F)

##################
# WRITING REPORT
##

# highligh color
hi.color <- "#ceffcf"

# create a style to the column headers
headerStyle <- createStyle(fontColour = "#000000",
                           textDecoration = "bold",
                           halign = "center",
                           fgFill = hi.color)


wb <- createWorkbook(title = sprintf("Sitemap OPIT"))

addWorksheet(wb, "OPIT Pagenames")

writeData(wb,
          sheet = "OPIT Pagenames",
          x = ff,
          colNames = TRUE,
          rowNames = FALSE,
          headerStyle = headerStyle
)

setColWidths(wb,
             sheet = "OPIT Pagenames",
             cols = 1:length(colnames(ff)),
             width = "auto")

###############
# SAVE REPORT
##

file.name <- sprintf("OPIT.pagenames.%s.xlsx",
                     Sys.Date()
)

saveWorkbook(wb,
             file.path(".", "vignettes", file.name),
             overwrite = TRUE)


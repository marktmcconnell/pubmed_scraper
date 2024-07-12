



# make a version to only print the printable ones....

# this will need a bit more... finesse...

# we will save it to another dir, and have instructions




scrape_pubmed <- function(query,out_dir){
  library(RISmed)
  library(tidyverse)
  
  uti_out <- EUtilsSummary(query = query)
  
  uti_out@db
  
  medline <- EUtilsGet(uti_out)
  
  # then get the year, month, day? 
  
  # how about for article date.
  
  # add that to obj
  
  # along with an author defined by user...
  
  # do i want the last author or just the used defined
  
  #i prob should get the last author
  
  
  pmids <- uti_out@PMID
  # this only gets the first 20
  #my_entrez_id <- get_pubmed_ids(my_query)
  # make a season_scraper
  
  #driver <- rsDriver(remoteServerAddr = "localhost", port=4445L, browser=c("firefox"), version = "latest")
  
  
  dir_path <- out_dir
  
  #pmid <- "29980527"
  
  #pmids <- unlist(my_entrez_id$IdList)
  
  
  already_files <- list.files(out_dir)
  
  #arrange(modificationTime)
  
  if(length(already_files) == 0){
    already_files <- ""
  }
  
  is_skipped <- max(grepl("skipped_ids.csv",already_files,fixed = T))
  
  # if its there load it, else make it
  
  skip_file <- paste(out_dir,"skipped_ids.csv",sep = "/")
  if(is_skipped){
    print("loading skip vec")
    skip_vec <- read.csv(skip_file,row.names = 1)$x
  } else{
    print("new skip vec")
    skip_vec <- NULL
  }
  
  
  for(each_i in 1:length(pmids)){
    print(each_i)
    cur_pmid <- pmids[each_i]
    
    cur_last_author <- medline@Author[[cur_pmid]]$LastName[length(medline@Author[[cur_pmid]]$LastName)]
    # check if its a preprint, if so lets grab it
    cur_y_m_d <- paste(medline@YearArticleDate[each_i],medline@MonthArticleDate[each_i],medline@DayArticleDate[each_i],sep = "-")
    # check if its in the directory
    
    
    pastable_file_name <- paste(cur_pmid,cur_last_author,cur_y_m_d,sep = "_")
    # if so, skip
    
    # else scrape
    
    foundany <- max(grepl(cur_pmid,already_files))
    #str(cur_pmid)
    #str(already_files)
    if(foundany > 0 ){
      message(paste(cur_pmid,"found in folder, skipping"))
    } else if(cur_pmid %in% skip_vec){
      print(paste(cur_pmid,"in skip vec"))
      }else{
        cur_url <- paste0("https://pubmed.ncbi.nlm.nih.gov/",cur_pmid,"/")
      print(cur_url)
      
      print("hio")
      print(length(cur_url))
      remDr$navigate(url = cur_url)
      Sys.sleep(max(rnorm(n = 1,mean = 2,sd = 1),0)+5)
      
      
      which_to_click <- tryCatch({
        remDr$findElement(using = "css selector",value = "#full-view-identifiers > li:nth-child(2) > span:nth-child(1) > a:nth-child(2)")
        
        #remDr$findElement(using = "css selector",value = "div.full-text-links-list:nth-child(2) > a:nth-child(2)")
        
      },
      error = function(cond){
        return(NULL)
      })
      
      
      # need to find if its preprint..
      
      images <- remDr$findElements(using = "tag", value = "img")
      
      src_attributes <- unlist(lapply(images, function(img) {
        return(img$getElementAttribute("src"))
      }))
      
      # here was my logic for biorxiv and medrxiv
      
      
      
      # TODO check for JASN and PMID, if so, then need to go to JASN because... PMID will be some printable form
      
      
      
      # yes...
      if("https://cdn.ncbi.nlm.nih.gov/corehtml/query/egifs/https:--biorxiv.s3.amazonaws.com-bioRxiv_link_icon.gif" %in% src_attributes | "https://cdn.ncbi.nlm.nih.gov/corehtml/query/egifs/https:--medrxiv.s3.amazonaws.com-medRxiv_link_icon.gif" %in% src_attributes){
        # go there..
        # get the DOI
        print("found biorxiv | medrxiv")
        #doi_flag <-   which_to_click <- remDr$findElement(using = "css selector",value = "#full-view-identifiers > li:nth-child(2) > span:nth-child(1) > a:nth-child(2)")
        
        
        doi_flag <-   which_to_click <- remDr$findElement(using = "class",value = "citation-doi")

        doi_text <- gsub("doi: ","",doi_flag$getElementText()[[1]])
        doi_text <- gsub("\\.$","",doi_text)
        
        #if its medrxiv its as follows
        # need to get doivec... or does that just go to... 
        
        cur_url <- paste0("https://doi.org/",doi_text)
        remDr$navigate(cur_url)
        # click download pdf
        
        # if medrxiv / biorxiv its the same
        to_click <- remDr$findElement(using = "css selector",value = ".article-dl-pdf-link > span:nth-child(2)")
        
        # if biorxiv
        
        # for biorxiv
        
        #".article-dl-pdf-link > span:nth-child(2)"
        to_click$clickElement()
        Sys.sleep(max(rnorm(n = 1,mean = 2,sd = 1),0)+10)
        
        
        # append name with biorxiv
        file_list <- list.files(dir_path, full.names = TRUE)
        
        # Get file information and sort by modification time
        sorted_files <- file_list %>%
          file.info() %>%
          arrange(desc(mtime))
        
        
        file.rename(rownames(sorted_files)[1],paste0(dir_path,pastable_file_name,"_biorxiv.pdf"))
        #TODO elsevier open access. 37543256
      } else if(!is.null(which_to_click)){
          cur_pmc <- which_to_click$getElementText()[[1]]
          
          is_pmc <- grepl("PMC",cur_pmc)
        
          if(is_pmc){
            
            
            
            new_url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/",cur_pmc,"/")
            
            remDr$navigate(new_url)
            
            #elemento <- remDr$findElement(using = "css selector", value = "li.pdf-link.other_item")
            
            #cur_pmc2 <- element$getElementAttribute("href")
            
            #pdf_div <- remDr$findElement(using = "css selector",value = ".pdf-link.other_item")
            #element <- remDr$findElement(using = "css selector", value = "li.pdf-link.other_item a.int-view")
            
            elementx <- remDr$findElement(using = "xpath", value= "/html/body/main/aside/div/section[1]/ul/li/a")
            
            
            element_text <- elementx$getElementText()[[1]]
            
            
            # TODO check if it says printer friendly
            
            #if so ?????
           # remDr$sendKeysToActiveElement
            #remDr$executeScript("window.print();")            # if its that printable type.... add it to the qyeye
            # may need to change to control p on windows 
            #remDr$findElements('css selector', 'html')[[1]]$sendKeysToElement(list(key='command', 'p'))
            #remDr$sendKeysToActiveElement(sendKeys = list(key='command', key= 'p'))
            #remDr$findElements('css selector', 'html')[[1]]$sendKeysToElement(list(key='control', 'p'))
            # try this next
            #element <- remDr$findElement(using = "xpath", value = "//a[contains(@href, '.pdf')]") # Assuming this 
            if(element_text == "Printer Friendly"){
              print("cant deal with this right now, need to print page")
              skip_vec <- c(skip_vec,cur_pmid)
              
            } else{
              elementx$clickElement()
              Sys.sleep(max(rnorm(n = 1,mean = 2,sd = 1),0)+10)
              
              # need to rename paper.
              # List all files in the directory
              file_list <- list.files(dir_path, full.names = TRUE)
              
              # Get file information and sort by modification time
              sorted_files <- file_list %>%
                file.info() %>%
                arrange(desc(mtime))
              
              
              file.rename(rownames(sorted_files)[1],paste0(dir_path,pastable_file_name,"_pmc.pdf"))
            }
           
            
          }else{
            print("not found pmc")
            skip_vec <- c(skip_vec,cur_pmid)
            
          }
          
        # element$clickElement()
        # elemento$clickElement()
        
        # download.file(url = cur_pmc2[[1]],paste0("~/pdf_downloads/",pmid,".pdf"))
        
        # remDr$executeScript("document.getElementById('downloadButton').click();")  # Replace with the actual JavaScript code
        
        
        # remDr$findElements('css selector', 'html')[[1]]$sendKeysToElement(list(key='control', 'p'))
        
        
        
      }else{
        # try scihub if 2020 or older
        
        # get the year
        
        # if year 2020 or before try scihub.
        
        
        
        print("no pmc found too complex")
        
        skip_vec <- c(skip_vec,cur_pmid)
      }
    }
    
    
    # if we need to stop
    
    write.csv(skip_vec,skip_file)
  }
  
  
  
  
  
  
  # else if("https://cdn.ncbi.nlm.nih.gov/corehtml/query/egifs/https:--linkinghub.elsevier.com-ihub-images-elsevieroa.png"%in% src_attributes){
  #   print("elsevier")
  #   
  #   # navigate
  #   #onetrust-accept-btn-handler
  #   print(paste("elsevier, skipping, these get angry at scraping, adding to manual list"))
  #   # to_click <- remDr$findElement(using = "css selector",value =   "div.full-text-links-list:nth-child(2) > a:nth-child(1) > img:nth-child(1)")
  #   #                               to_click$clickElement()
  #   #                             
  #   # cur_css <- remDr$findElement(using = "css selector",value = "div.full-text-links-list:nth-child(2) > a:nth-child(1)")
  #   # 
  #   # the_url <- cur_css$getElementAttribute("href")[[1]]
  #   # # then when here click thsi
  #   # remDr$navigate(the_url)
  #   # 
  #   # to_click <- remDr$findElement(using = "css selector",value =           ".article-tools__item__pdf")
  #   #                               
  #   #                               to_click$clickElement()
  #   skip_vec <- c(skip_vec,cur_pmid)
  #   
  # }
  # 
  
  
}
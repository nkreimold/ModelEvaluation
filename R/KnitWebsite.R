#' @rdname RenderWebsite.data.frame
#' @export
RenderWebsite <- function(.data, ...) {
  UseMethod("RenderWebsite")
}


#' Data Quality and Model Performance Report Generation
#'
#' @description The RenderWebsite() produces reports for evaluating
#' performance of a two stage modeling process. 
#'
#' @details Knit together a bunch of model performance and data quality reports into a website. 
#' All report objects must have been previously generated
#' 
#' @section Reported information:
#' The Model Performance process will report the following information:
#'
#' \itemize{
#'   \item Index- Summary Page
#'   \item Raw and Model Data Quality
#'   \item Performance for each segment
#'   \item Overall Performance
#' }
#'
#' See vignette("Qual_repoort") for an introduction to these concepts.
#'
#' @param Pathvars a tbl_dbi. containing Folder names and paths of all data files(within basepath)
#' @param basepath a string for path where all metrics are located
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{RenderWebsite.data.frame}}.
#' @examples
#' \donttest{
#' pathvars must contain names- used 'Data' if data quality and 'Performance' if model performance
#' 
#' paths can be duplicated, as long as everything has at least one common subdirectory 
#' 
#' 
#'Pathvars=data.frame(Names=c("Raw Data Active","Raw Data DQ",
#'"Secured Model Data Active","Secured Model Data DQ 30-60",
#'"Secured Model Data DQ 60-90","Secured Model Data DQ 90+",
#'"Unsecured Model Data Active","Unsecured Model Data DQ 30-60",
#'"Unsecured Model Data DQ 60-90","Unsecured Model Data DQ 90+",
#'"Secured Performance Active","Secured Performance DQ 30-60",
#'"Secured Performance DQ 60-90","Secured Performance DQ 90+",
#'"Unsecured Performance Active", "Unsecured Performance DQ 30-60",
#'"Unsecured Performance DQ 60-90","Unsecured Performance DQ 90+"),
#'
#'paths=c( "RawData/Active/","RawData/DQ/",
#'"SECUREDActive0/","SECUREDDQ30/","SECUREDDQ60/","SECUREDDQ90/",
#'"UNSECUREDActive0/","UNSECUREDDQ30/","UNSECUREDDQ60/","UNSECUREDDQ90/" ,
#'"SECUREDActive0/","SECUREDDQ30/","SECUREDDQ60/","SECUREDDQ90/",
#'"UNSECUREDActive0/","UNSECUREDDQ30/","UNSECUREDDQ60/","UNSECUREDDQ90/" )
#',stringsAsFactors = F)
#'
#'
#'basepath="S:/30000/Credit Policy/Nicholas/GitHub/CECL/CECL_CL_PD_EAD/ModelEval/ReportObjects2"
#'
#'RenderWebsite(Pathvars,basepath,output_dir=basepath)
#'
#'
#'}
#' @importFrom knitr knit2pdf
#' @importFrom rmarkdown render
#' @importFrom grDevices cairo_pdf
#' @importFrom xtable xtable
#' @importFrom moments skewness kurtosis
#' @importFrom prettydoc html_pretty
#' @importFrom kableExtra kable_styling
#' @importFrom utils browseURL
#' @importFrom pROC ggroc
#' @method RenderWebsite data.frame
#' @export
RenderWebsite.data.frame <- function(Pathvars=NULL,basepath=NULL,output_dir = tempdir(), browse = TRUE, ...) {
  
  if (is.null(basepath)) {
    stop("Need to specify base path")
  }
  
  if (is.null(Pathvars)) {
    stop("Need to specify base report names and paths")
  }else if(nrow(Pathvars)<2){
    stop("Need at least two reports")
  }
  
  currentwd=getwd()

  Pathvars$basepath=basepath
  Pathvars$filename="NULL"
  datanames=perfnames=NULL
  for(q in 1:nrow(Pathvars)){
    Pathvars$filename[q]=gsub("[^[:alnum:]\\]", "",Pathvars$Names[q] )
    
    if(grepl("Data",Pathvars$Names[q])==TRUE){
      datanames=rbind(datanames,
                      paste0("        - text: ",Pathvars$Names[q]),
                      paste0("          href: ",Pathvars$filename[q],".html"))
    }else if(grepl("Performance",Pathvars$Names[q])==TRUE){
      perfnames=rbind(perfnames,
                      paste0("        - text: ",Pathvars$Names[q]),
                      paste0("          href: ",Pathvars$filename[q],".html"))
    }
    
  }
  
  dir.create(file.path(output_dir))
  
  Rmd_file <- file.path(system.file(package = "ModelEvaluation"), "DataWebsite")
  file.copy(from = Rmd_file, to = output_dir, recursive = TRUE)
  
  output_dir=paste0(output_dir,"/DataWebsite/")
  #create .yml file
  YAMLFILE <- read.csv(paste0(output_dir,"_site.yml"), header=FALSE, sep=";",stringsAsFactors = F)
  
  #"        - text: Raw Data DQ" "          href: index.html"
  YAMLFILE=YAMLFILE%>%slice(1:9)%>%rbind(datanames)%>%rbind(YAMLFILE%>%slice(10:11))%>%rbind(perfnames)%>%rbind(YAMLFILE%>%slice(12:22))
  YAMLFILE$V1=YAMLFILE$V1%>%as.factor()
  
  YAMLFILE=YAMLFILE[,]
  write.csv(YAMLFILE,paste0(output_dir,"_site.yml"),row.names = F,quote = F)
  
  YAMLFILE <- read.csv(paste0(output_dir,"_site.yml"), header=FALSE, sep=";",stringsAsFactors = F)
  nm<-YAMLFILE$V1[2]
  YAMLFILE=YAMLFILE%>%slice(3:nrow(YAMLFILE))
  names(YAMLFILE)=nm
  write.csv(YAMLFILE,paste0(output_dir,"_site.yml"),quote=F,row.names = F)
  
  print("Copying files to directory")
  foldernames=NULL
  for(q in 1:nrow(Pathvars)){
    fp=file.path(Pathvars$basepath[q],Pathvars$paths[q])
    foldername=fp%>%strsplit("/")%>%.[[1]]%>%tail(1)
    if(!foldername%in%foldernames){
    
    file.copy(from = fp, to = file.path(output_dir,"Report Objects"), recursive = TRUE)
      
    }
    foldernames=c(foldernames,foldername)
  }

  Pathvars$paths=foldernames
  Pathvars$basepath=file.path(output_dir,"Report Objects")
   
  setwd(output_dir)
  save(Pathvars,file=paste0(file.path(output_dir,"filepath.RData")))
  rmarkdown::render_site()
  
  path=file.path(output_dir,"docs","index.html")
  if (browse & file.exists(path)) {
   browseURL(path) }
  
  setwd(currentwd)
}
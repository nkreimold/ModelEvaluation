#' @rdname Qual_report.data.frame
#' @export
Qual_report <- function(.data, ...) {
  UseMethod("Qual_report")
}


#' Reporting the information of Data Quality
#'
#' @description The Qual_report() report the information of Exploratory
#' data analysis for object inheriting from data.frame.
#'
#' @details Generate generalized data quality reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for a data frame with a large number of variables
#' than data with a small number of variables.
#' 
#' @section Reported information:
#' The Data Quality process will report the following information:
#'
#' \itemize{
#'   \item Missing and Unique Values
#'   \itemize{
#'     \item Missing Values
#'     \item Unique Values
#'     \item Percent Zero Values
#'     \item Percent Negative Values
#'     \item Percent Outliers
#'   }
#'   \item Descriptive Statistics
#'   \item Categorical Breakdown
#'   \itemize{
#'     \item Percent of Data in each level
#'   }
#'   \item Histograms
#'   \item Population Stability Index
#'   \item Trended Numerical Variables
#'   \item Trended Categorical Variables
#'   \item Correlation Matrix
#' }
#'
#' See vignette("Qual_repoort") for an introduction to these concepts.
#'
#' @param .data a tbl_dbi.
#' @param DataDict dataframe specifying variable, variable description, and variable type.
#' @param DateName Name of date variable in dataset
#' @param datetofilteron a Date. The date to separate training and test for PSI calculation.
#' @param saveoutputs a character path string. If populated, will save png and csv outputs to this folder.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{Qual_report.data.frame}}.
#' @examples
#' \donttest{
#' 
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Qual_report("US", output_format = "html")
#' 
#' # create html file. file name is EDA.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Qual_report(output_format = "html", output_file = "EDA.html")
#'
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Qual_report("Sales", output_format = "html")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Qual_report(output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Qual_report(output_format = "html", output_file = "EDA2.html")
#' }
#'
#' @importFrom knitr knit2pdf
#' @importFrom rmarkdown render
#' @importFrom grDevices cairo_pdf
#' @importFrom xtable xtable
#' @importFrom moments skewness kurtosis
#' @importFrom prettydoc html_pretty
#' @importFrom kableExtra kable_styling
#' @importFrom utils browseURL
#' @method Qual_report data.frame
#' @export
Qual_report.data.frame <- function(.data,
                                   DataDict=NULL,
                                   DateName="",
                                   datetofilteron=NULL, 
                                   saveoutputs=NULL,
                                   output_format = "html",
  output_file = NULL, output_dir = tempdir(), browse = TRUE, ...) {
  
  #output_format <- match.arg(output_format)
  QualReportPath <- output_dir
  
  if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
    latex_main <- "Qual_Report_KR.Rnw"
    latex_sub <- "02_RunEDA_KR.Rnw"
  } else {
    latex_main <- "Qual_Report.Rnw"
    latex_sub <- "02_RunEDA.Rnw"
  }  

  if (DateName=="") {
    warning("Didn't specify a Date variable to use",immediate.  = TRUE)
  }
  
  DeleteFiles="NO"
  if (is.null(saveoutputs)){
    saveoutputs=paste0(getwd(),"/NewFolder/")
    DeleteFiles="Yes"
  }

  if (is.null(DataDict)){
    DataDict=data.frame(Variable=names(as.data.frame(.data)),Description=names(as.data.frame(.data)),stringsAsFactors = F)
    DataDict$VarType=sapply(as.data.frame(.data),"class")
    DataDict=DataDict%>%mutate(VarType=ifelse(VarType=="Date"|VarType=="",VarType,ifelse(VarType=="integer"|VarType=="numeric","Numeric","Factor")))
  }
  
  dir.create(file.path(saveoutputs))
  write.csv(DataDict,file = paste0(saveoutputs,"DataDict.csv"),row.names = F)
  
  
  ModelEvaluation::DataQuality(DataDict,
              DF=as.data.frame(.data),
              DateName=DateName,
              outpath=saveoutputs,
              datetofilteron)
  
  
print(saveoutputs)
  if (output_format == "pdf") {
    installed <- file.exists(Sys.which("pdflatex"))
    
    if (!installed) {
      stop("No TeX installation detected. Please install TeX before running.\nor Use output_format = \"html\"")
    }
    
    if (is.null(output_file))
      output_file <- "Qual_Report.pdf"
    
    Rnw_file <- file.path(system.file(package = "ModelEvaluation"), "report", latex_main)
    file.copy(from = Rnw_file, to = QualReportPath)
    
    Rnw_file <- file.path(system.file(package = "ModelEvaluation"), "report", latex_sub)
    file.copy(from = Rnw_file, to = QualReportPath)
    
    Img_file <- file.path(system.file(package = "ModelEvaluation"), "img")
    file.copy(from = Img_file, to = QualReportPath, recursive = TRUE)
    
    dir.create(paste(QualReportPath, "figure", sep = "/"))
    
    knitr::knit2pdf(paste(QualReportPath, latex_main, sep = "/"), 
      compiler = "pdflatex",
      output = sub("pdf$", "tex", paste(QualReportPath, output_file, sep = "/")))
    
    file.remove(paste(QualReportPath, latex_sub, sep = "/"))
    file.remove(paste(QualReportPath, latex_main, sep = "/"))
    
    fnames <- sub("pdf$", "", output_file)
    fnames <- grep(fnames, list.files(QualReportPath), value = TRUE)
    fnames <- grep("\\.pdf$", fnames, invert = TRUE, value = TRUE)
    
    file.remove(paste(QualReportPath, fnames, sep = "/"))
    
    unlink(paste(QualReportPath, "figure", sep = "/"), recursive = TRUE)
    unlink(paste(QualReportPath, "img", sep = "/"), recursive = TRUE)
  } else if (output_format == "html") {
    if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
      rmd <- "Qual_Report_KR.Rmd"
    } else {
      rmd <- "QualityReport.Rmd"
    }
    
    if (is.null(output_file))
      output_file <- "QualityReport.html"
    
    Rmd_file <- file.path(system.file(package = "ModelEvaluation"), "report", rmd)
    file.copy(from = Rmd_file, to = QualReportPath, recursive = TRUE)
    
    rmarkdown::render(paste(QualReportPath, rmd, sep = "/"),
                      params=list(pathMaster=saveoutputs,
                                  cutoffdate=ifelse(is.null(datetofilteron),"NO","YES")),
      output_file = paste(QualReportPath, output_file, sep = "/"))
    
    file.remove(paste(QualReportPath, rmd, sep = "/"))
  }
  if(DeleteFiles=="Yes"){
    saveoutputs=substr(saveoutputs,1,nchar(saveoutputs)-1)
    unlink(saveoutputs,recursive = TRUE)
  }
  
  if (browse & file.exists(paste(QualReportPath, output_file, sep = "/"))) {
    browseURL(paste(QualReportPath, output_file, sep = "/"))
  }
}




#' @rdname Perf_report.data.frame
#' @export
Perf_report <- function(.data, ...) {
  UseMethod("Perf_report")
}


#' Model Performance Report Generation
#'
#' @description The Perf_report() produces and saves objects for evaluating
#' performance of a two stage modeling process. Object must be of form data.frame.
#'
#' @details Generate generalized Model Performance reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for complex models with many variables you wish to segment
#' performance around.
#' 
#' @section Reported information:
#' The Model Performance process will report the following information:
#'
#' \itemize{
#'   \item PD K-fold Cross Validation
#'   \itemize{
#'     \item PD Model Coefficients / Significance
#'     \item Concordance
#'     \item Recalibration
#'   }
#'   \item PD Model Discrimination
#'   \itemize{
#'     \item KS Tables
#'     \item KS Plots
#'     \item Gini Curve/Lorenz Curve
#'     \item ROC Curve
#'   }
#'   \item PD Backtesting by Bucket
#'   \itemize{
#'     \item Time Point
#'     \item Aggregate
#'     \item PD Final
#'   }
#'   \item Loss Forecast Backtesting
#'   \itemize{
#'     \item Time Point
#'     \item Aggregate Loss
#'     \item Overall Loss Error Rate
#'   }
#' }
#'
#' See vignette("Qual_repoort") for an introduction to these concepts.
#'
#' @param ModelResults a tbl_dbi. with model performance
#' @param SegmentDF a tbl_dbi. with segmentation variables
#' @param DataDict dataframe specifying variable, variable description, and variable type.
#' @param ClassActual Name of classification model actuals
#' @param ClassPrediction Name of classification model prediction
#' @param NumericActual Name of date numeric model actuals
#' @param NumericPrediction Name of numeric model ouput
#' @param grouping_var Name of ID variable in dataset(i.e. Account number, applid)
#' @param time_var Name of date variable in dataset
#' @param cal.model Calibration model to reset PD
#' @param XVALObjectPath Path where cross-validation objects are stored
#' @param outpath a character path string. If populated, will save png and csv outputs to this folder.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{Perf_report.data.frame}}.
#' @examples
#' \donttest{
#' 
#'ModelResults=ModelResults
#'SegmentDF=SegmentDF
#'time_var="Time"
#'ClassActual="CO"
#'ClassPrediction="Prob.Default"
#'NumericPrediction="Pred.Loss"
#'NumericActual="REAL_LOSS"
#'grouping_var="Eval.ID"
#'
#'#load your datadictionary
#'DataDict=read.csv("S:/30000/Credit Policy/Nicholas/GitHub/CECL/CECL_CL_PD_EAD/ModelEval/Report Objects/DataDictionary.csv",stringsAsFactors = F)
#'outpath=paste0(getwd(),"/NewFolder/")#where to save all the performance files- png, csv, etc.
#'output_dir=getwd()#-path where to save output html
#'
#'
#'
#'
#'
#'Perf_report(ModelResults,SegmentDF,
#'            DataDict,
#'            ClassActual,ClassPrediction,
#'            NumericActual,NumericPrediction,
#'            grouping_var,
#'            time_var,
#'            cal.model,
#'            XVALObjectPath="S:/30000/Credit Policy/Nicholas/GitHub/CECL/CECL_CL_PD_EAD/ModelEval/Report Objects/UNSECUREDDQ90/Coefficients.RData"
#'            ,outpath=outpath,
#'            output_dir = output_dir)
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
#' @method Perf_report data.frame
#' @export
Perf_report.data.frame <- function(ModelResults,SegmentDF,
                                   DataDict=NULL,
                                   ClassActual="",ClassPrediction="",
                                   NumericActual="",NumericPrediction="",
                                   grouping_var="",
                                   time_var="",
                                   cal.model=NULL,
                                   XVALObjectPath=NULL,
                                   outpath=NULL,
                                   output_format = "html",
                                   output_file = NULL, output_dir = tempdir(), browse = TRUE, ...) {
  
  
  
  
  
  #output_format <- match.arg(output_format)
  PerfReportPath <- output_dir
  
  if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
    latex_main <- "Perf_report_KR.Rnw"
    latex_sub <- "02_RunEDA_KR.Rnw"
  } else {
    latex_main <- "Perf_report.Rnw"
    latex_sub <- "02_RunEDA.Rnw"
  }  
  
  if (ClassActual==""|ClassPrediction=="") {
    stop("Need to specify classification prediction and/or actual names")
  }
  
  if (time_var=="") {
    warning("Didn't specify a Date variable to use",immediate.  = TRUE)
  }
  
  if (NumericActual==""|NumericPrediction=="") {
    warning("Didn't both numeric model result variable names. Section will be omitted.",immediate.  = TRUE)
    NumericActual=NumericPrediction=""
  }
  

  
  
  DeleteFiles="NO"
  if (is.null(outpath)){
    outpath=paste0(getwd(),"/NewFolder123123/")
    DeleteFiles="Yes"
  }
  dir.create(file.path(outpath))
  

  if(is.null(XVALObjectPath)){
    XVALObjectPath="NULL"
    warning("No cross validation object",immediate.  = TRUE)
    
  }else{
    xvalfile=basename(XVALObjectPath)
    if(!XVALObjectPath==file.path(file.path(outpath),"Coefficients.RData")){
      if(file.exists(file.path(file.path(outpath),"Coefficients.RData"))){
        stop("Trying to overwrite already existing coefficients with different file")
      }
    file.copy(from = XVALObjectPath, to =outpath, recursive = TRUE)
    file.rename(paste0(outpath,xvalfile),to=paste0(outpath,"Coefficients.RData"))
    }
  }
  
  
  if (is.null(DataDict)){
    s=SegmentDF%>%select_all(-grouping_var)
    DataDict=data.frame(Variable=names(as.data.frame(s)),Description=names(as.data.frame(s)),stringsAsFactors = F)
    DataDict$VarType=sapply(as.data.frame(.data),"class")
    DataDict=DataDict%>%mutate(VarType=ifelse(VarType=="Date"|VarType=="",VarType,ifelse(VarType=="integer"|VarType=="numeric","Numeric","Factor")))
  }
  
  
  write.csv(DataDict,file = paste0(outpath,"DataDict.csv"),row.names = F)
  
  
  
  ModelEvaluation::PEval(ModelResults,SegmentDF,
                         ClassActual,ClassPrediction,
                         NumericActual,NumericPrediction,
                         grouping_var,
                         time_var,
                         DataDict,
                         outpath,cal.model)
  
  
  if (output_format == "pdf") {
    installed <- file.exists(Sys.which("pdflatex"))
    
    if (!installed) {
      stop("No TeX installation detected. Please install TeX before running.\nor Use output_format = \"html\"")
    }
    
    if (is.null(output_file))
      output_file <- "Perf_report.pdf"
    
    Rnw_file <- file.path(system.file(package = "ModelEvaluation"), "report", latex_main)
    file.copy(from = Rnw_file, to = PerfReportPath)
    
    Rnw_file <- file.path(system.file(package = "ModelEvaluation"), "report", latex_sub)
    file.copy(from = Rnw_file, to = PerfReportPath)
    
    Img_file <- file.path(system.file(package = "ModelEvaluation"), "img")
    file.copy(from = Img_file, to = PerfReportPath, recursive = TRUE)
    
    dir.create(paste(PerfReportPath, "figure", sep = "/"))
    
    knitr::knit2pdf(paste(PerfReportPath, latex_main, sep = "/"), 
                    compiler = "pdflatex",
                    output = sub("pdf$", "tex", paste(PerfReportPath, output_file, sep = "/")))
    
    file.remove(paste(PerfReportPath, latex_sub, sep = "/"))
    file.remove(paste(PerfReportPath, latex_main, sep = "/"))
    
    fnames <- sub("pdf$", "", output_file)
    fnames <- grep(fnames, list.files(PerfReportPath), value = TRUE)
    fnames <- grep("\\.pdf$", fnames, invert = TRUE, value = TRUE)
    
    file.remove(paste(PerfReportPath, fnames, sep = "/"))
    
    unlink(paste(PerfReportPath, "figure", sep = "/"), recursive = TRUE)
    unlink(paste(PerfReportPath, "img", sep = "/"), recursive = TRUE)
  } else if (output_format == "html") {
    if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
      rmd <- "Perf_report_KR.Rmd"
    } else {
      rmd <- "PerformanceReport.Rmd"
    }

    if (is.null(output_file))
      output_file <- "PerformanceReport.html"
    
    Rmd_file <- file.path(system.file(package = "ModelEvaluation"), "report", rmd)
    file.copy(from = Rmd_file, to = PerfReportPath, recursive = TRUE)
    
    rmarkdown::render(paste(PerfReportPath, rmd, sep = "/"),
                      params=list(pathMaster=outpath),
                      output_file = paste(PerfReportPath, output_file, sep = "/"))
    
    file.remove(paste(PerfReportPath, rmd, sep = "/"))
  }
  if(DeleteFiles=="Yes"){
    outpath=substr(outpath,1,nchar(outpath)-1)
    unlink(outpath,recursive = TRUE)
  }
  
  if (browse & file.exists(paste(PerfReportPath, output_file, sep = "/"))) {
    browseURL(paste(PerfReportPath, output_file, sep = "/"))
  }
}







#' @rdname Forecast_report.data.frame
#' @export
Forecast_report <- function(...) {
  UseMethod("Forecast_report")
}


#' Reporting the information of Forecasting
#'
#' @description The Forecast_report() report the information of Exploratory
#' data analysis for object inheriting from data.frame.
#'
#' @details Generate generalized data quality reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for a data frame with a large number of variables
#' than data with a small number of variables.
#' 
#' @section Reported information:
#' The Data Quality process will report the following information:
#'
#' \itemize{
#'   \item Missing and Unique Values
#'   \itemize{
#'     \item Missing Values
#'     \item Unique Values
#'     \item Percent Zero Values
#'     \item Percent Negative Values
#'     \item Percent Outliers
#'   }
#'   \item Descriptive Statistics
#'   \item Categorical Breakdown
#'   \itemize{
#'     \item Percent of Data in each level
#'   }
#'   \item Histograms
#'   \item Population Stability Index
#'   \item Trended Numerical Variables
#'   \item Trended Categorical Variables
#'   \item Correlation Matrix
#' }
#'
#' See vignette("Forecast_repoort") for an introduction to these concepts.
#'
#' @param .data a tbl_dbi.
#' @param DataDict dataframe specifying variable, variable description, and variable type.
#' @param DateName Name of date variable in dataset
#' @param datetofilteron a Date. The date to separate training and test for PSI calculation.
#' @param saveoutputs a character path string. If populated, will save png and csv outputs to this folder.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by knitr::knit().
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param ... arguments to be passed to methods.
#' 
#' @seealso \code{\link{Forecast_report.data.frame}}.
#' @examples
#' \donttest{
#' 
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # connect DBMS
#' con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' 
#' # copy carseats to the DBMS with a table named TB_CARSEATS
#' copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Forecast_report("US", output_format = "html")
#' 
#' # create html file. file name is EDA.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Forecast_report(output_format = "html", output_file = "EDA.html")
#'
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Forecast_report("Sales", output_format = "html")
#' 
#' # create html file. file name is EDA_Report.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Forecast_report(output_format = "html")
#' 
#' # create html file. file name is EDA2.html
#' con_sqlite %>% 
#'   tbl("TB_CARSEATS") %>% 
#'   Forecast_report(output_format = "html", output_file = "EDA2.html")
#' }
#'
#' @importFrom knitr knit2pdf
#' @importFrom rmarkdown render
#' @importFrom grDevices cairo_pdf
#' @importFrom xtable xtable
#' @importFrom moments skewness kurtosis
#' @importFrom prettydoc html_pretty
#' @importFrom kableExtra kable_styling
#' @importFrom utils browseURL
#' @method Forecast_report data.frame
#' @export
Forecast_report.data.frame <- function(RESULTS,
                                       OverallTimePoint,
                                   DescriptionDF=NULL,
                                   Total=NULL,
                                   HistoricalCurve=NULL,
                                   output_format = "html",
                                   output_file = NULL, output_dir = NULL, browse = TRUE, ...) {
  
  #output_format <- match.arg(output_format)
  
  ForecastReportPath <- output_dir
  if(is.null(ForecastReportPath)){
    ForecastReportPath<-tempdir()
  }else{
    ForecastReportPath=file.path(ForecastReportPath)
  }
  
  
  ForecastMetrics(RESULTS,OverallTimePoint,DescriptionDF,Total,HistoricalCurve,ForecastReportPath)
    
  
  
 if (output_format == "html") {
    if (length(grep("ko_KR", Sys.getenv("LANG"))) == 1) {
      rmd <- "Forecast_Report_KR.Rmd"
    } else {
      rmd <- "ForecastReport.Rmd"
    }
    
    if (is.null(output_file))
      output_file <- "ForecastReport.html"
    
    Rmd_file <- file.path(system.file(package = "ModelEvaluation"), "report", rmd)
    file.copy(from = Rmd_file, to = ForecastReportPath, recursive = TRUE)
    
    rmarkdown::render(paste(ForecastReportPath, rmd, sep = "/"),
                       params=list(pathMaster=ForecastReportPath),
                       output_file = file.path(ForecastReportPath, output_file))
    
    file.remove(paste(ForecastReportPath, rmd, sep = "/"))
  }
  # if(is.null(output_dir)){
  #   unlink(ForecastReportPath,recursive = TRUE)
  # }
  
  if (browse & file.exists(paste(ForecastReportPath, output_file, sep = "/"))) {
    browseURL(paste(ForecastReportPath, output_file, sep = "/"))
  }
}

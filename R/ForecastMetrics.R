
#' @title
#' Forecast Metrics
#' @description
#' \code{Forecast Metrics} calculates trended views, missing data, summary statistics, and histograms for a given data set and saves to a directory.
#' @author Nicholas Reimold
#' @references \href{What is population stability index? - Quora}{https://www.quora.com/What-is-population-stability-index}
#'
#' @param DataDictionary The original set of a measurement, should be a factor or numeric
#' @param DF  The current set of a measurement, should be a factor or numeric
#' @param DateName It won't work if original and current are factors, and it cannot be NULL if original and current are numerical. This function uses this argument to bin \code{original} and \code{current} with left-closed right-open intervals.
#' @param outpath specifies how many levels to split a numeric variable. If \code{cut.points} is provided, this parameter will be ignored. When using this parameter, a numeric variable will be split using \code{cut}.
#' @param datetofilteron specifies how many levels to split a numeric variable. If \code{cut.points} is provided, this parameter will be ignored. When using this parameter, a numeric variable will be split using \code{cut}.
#'
#' @return Saved \code{R} objects
#'
#' @details psi measures the stablity of the population. Usually we can believe the population stays the same as the past if psi is less than 0.1, and a significant shift can be recognised if psi is greater than 0.25. The outcome of this function is a numeric, with details stored as attributes. You can use \code{summary} function to see all of the detailed information. Fot the situation where some of the levels has no element in either original population or current population and the psi does not exist for such levels, the empty levels will not be taken into account and a warning will inform you of this. Again, by using \code{summary} you could know everything inside.
#'
#' @examples
#' for( seg in c("DQ","Active")){
#'   print(seg)
#'   DataDictionary=read.csv("ModelEval/Report Objects/DataDictionary.csv",stringsAsFactors = F )%>%
#'     select(Variable,Description,VarType)
#'   DateName="PERIOD_END_DT"
#'
#'   # Load Data
#'   load(paste0("DataPulls/Sample/FullSnapData.RData"))
#'   DF=get(paste0("Final",seg))
#'   rm(FinalDQ,FinalActive)
#'
#'   outpath=paste0("S:/30000/Credit Policy/Nicholas/GitHub/CECL/CECL_CL_PD_EAD/ModelEval/Report Objects/RawData/",seg,"/")
#'   ForecastMetrics=function(DataDictionary,DF,DateName,outpath,datetofilteron=NULL)
#' }
#'
#'
#' @import magrittr
#' @export
ForecastMetrics=function(RESULTS,OverallTimePoint,DescriptionDF,Total,HistoricalCurve,outpath){
  dir.create(file.path(outpath))
  names(Total)=c("Date","Loss_Rate")
  names(HistoricalCurve)[1:2]=c("Product","Time")
  
  fp=file.path(file.path(outpath),"ForecastMetricData.RData")
  if(file.exists(fp))
    file.remove(fp)
    
  
  save(RESULTS,OverallTimePoint,DescriptionDF,Total,HistoricalCurve,file=file.path(file.path(outpath),"ForecastMetricData.RData"))
  
}






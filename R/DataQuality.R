
#' @title
#' Data Quality
#' @description
#' \code{Data Quality} calculates trended views, missing data, summary statistics, and histograms for a given data set and saves to a directory.
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
#'   DataQuality=function(DataDictionary,DF,DateName,outpath,datetofilteron=NULL)
#' }
#'
#'
#' @import magrittr
#' @export
DataQuality=function(DataDictionary,DF,DateName="",outpath,datetofilteron=NULL){
  options(warn=-1)
  dir.create(file.path(outpath))
  #if(is.null(datetofilteron)){datetofilteron=max(DF[,DateName])-years(3)}
  ##### Load Data###################
  DataDictionary=DataDictionary%>%filter(Variable==DateName|VarType!="Date")%>%
    filter(Variable%in%names(DF))
  colnames=names(DF)[names(DF)%in%DataDictionary$Variable]
  DF=DF[,colnames]
  
  #convert types based on data dictionary
  for(j in 1:ncol(DF)){
    DataDictionary1=DataDictionary%>%filter(Variable==names(DF)[j])%>%.$VarType
    
    if(DataDictionary1=="Numeric"){
      DF[,j]=DF[,j]%>%as.numeric()
    }
    if(DataDictionary1=="Date"){
      DF[,j]=DF[,j]%>%as.Date()
    }
    if(DataDictionary1=="Factor"){
      DF[,j]=DF[,j]%>%as.factor()
    }
    
  }
  
  
  
  if(DateName!=""){
    #Time Varying Plots
    nums <- unlist(lapply(DF, is.numeric))
    dt=DF[,DateName]
    NumericVars=DF[ , nums]%>%as.data.frame()
    
    table_numerical_trended_names=data.frame(Variable=names(NumericVars))%>%left_join(DataDictionary,by="Variable")%>%.$Description
    
    
    jj=1
    for(q in names(NumericVars)){
      
      measure=data.frame(Date=dt,Measure=NumericVars[,q]%>%as.numeric(),group="l")
      measure=measure%>%group_by(Date,group)%>%summarise(Measure=mean(Measure,na.rm=T))
      
      p<-ggplot(measure, aes(x=Date, y=Measure,group=group)) +
        geom_line(aes(color="#69b3a2"))+
        geom_point(aes(color="#e9ecef"))+ theme_minimal() +
        theme(
          plot.title = element_text(size=20),
          axis.text=element_text(size=12),
          axis.title = element_text(size=15)
        )+ theme(legend.position="none")
      
      png(file=paste0(outpath,"NumTrended_",jj,".png"), width = 800, height = 600)
      print(p)
      dev.off()
      #
      # save(p,file=paste0(outpath,"NumTrended_",jj,".RData"))
      
      rm(p)
      jj=jj+1
      
    }
    
    
    
    # Categorical
    CategoricVars=DF[,!names(DF)%in%names(NumericVars)]%>%as.data.frame()
    x2=CategoricVars%>%dplyr::select(-all_of(DateName))
    x1=CategoricVars%>%dplyr::select(all_of(DateName))
    
    CategoricVars=cbind(x1,x2)
    table_cat_trended_names=data.frame(Variable=names(CategoricVars[,2:ncol(CategoricVars)]))%>%left_join(DataDictionary,by="Variable")%>%.$Description
    
    jj=1
    rm(x1,x2)
    
    #period end dt is first column
    for(q in 2:ncol(CategoricVars)){
      
      v=xtabs(~CategoricVars[,1]+CategoricVars[,q], data=CategoricVars)%>%data.frame()
      names(v)<-c("Date","Level","Freq")
      v$Date=as.Date(v$Date)
      
      v_sum=v%>%group_by(Date)%>%summarise(Freq_sum=sum(Freq))
      
      v=v%>%left_join(v_sum,by="Date")%>%mutate(Freq=Freq/Freq_sum*100)
      
      v$Level  <- with(v, reorder(Level, Freq))
      
      g<-ggplot(v, aes(x = Date, y = Freq, fill = Level)) + geom_area(position = 'stack') +
        xlab("Date") + ylab("Frequency(%)")+ theme_minimal()+
        ggtitle(names(CategoricVars)[q]) +
        theme(
          plot.title = element_text(size=20),
          axis.text=element_text(size=15),
          axis.title = element_text(size=20)
        ) #+    ylim(0, 100)
      
      
      #save(g,file=paste0(outpath,"CatTrended_",jj,".RData"))
      png(file=paste0(outpath,"CatTrended_",jj,".png"), width = 800, height = 600)
      print(g)
      dev.off()
      
      jj=jj+1
      
      
      
    }
  }
  
  
  ##missing
  DataDictionary=filter(DataDictionary,VarType!="Date")
  colnames=names(DF)[names(DF)%in%DataDictionary$Variable]
  TempV=diagnose_numeric(DF)%>%mutate(outlier=outlier/nrow(DF)*100,minus=minus/nrow(DF)*100,zero=zero/nrow(DF)*100)%>%dplyr::select(variables,outlier,minus,zero)
  
  table_missing=diagnose(DF)%>%as.data.frame()%>%
    #filter(missing_count>0|unique_count<=20)%>%
    dplyr::select(-unique_rate,-missing_count)%>%
    left_join(TempV,by="variables")%>%
    arrange(desc(missing_percent),desc(unique_count),desc(outlier),desc(minus),desc(zero))
  
  for (j in c(3,5:ncol(table_missing))){
    xx=round(table_missing[,j],2)%>%data.frame()
    
    table_missing[,j]=xx[,1]
  }
  
  table_missing=DataDictionary%>%left_join(table_missing,by=c("Variable"="variables"))%>%dplyr::select(-types)%>%arrange(VarType,missing_percent)
  table_missing<- table_missing[seq(dim(table_missing)[1],1),]
  
  
  
  
  ## Summary Statistics
  prof_num=DF[rowSums(is.na(DF))==0,]
  
  prof_num=profiling_num(prof_num)%>%dplyr::select(variable,std_dev,skewness)%>%rename(variables=variable)
  
  table_summary=diagnose_numeric(DF)%>%left_join(prof_num,by="variables")
  table_summary=table_summary%>%dplyr::select(-outlier,-minus,-zero)#%>%mutate(outlier=outlier/nrow(DF)*100,minus=minus/nrow(DF)*100,zero=zero/nrow(DF)*100)
  
  for (j in 2:ncol(table_summary)){
    xx=round(table_summary[,j],2)%>%data.frame()
    
    table_summary[,j]=xx[,1]
  }
  
  
  table_summary=DataDictionary%>%dplyr::select(-VarType)%>%left_join(table_summary,by=c("Variable"="variables"))%>%arrange(Description)%>%filter(!is.na(mean))
  
  names(table_summary)[c(3,5:6,8:10)]<-c("Min","Mean","Median","Max","Std Dev","Skewness")
  
  kable(table_summary) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),font_size = 12)
  
  
  
  ## Categorical Table Summaries
  category_table=diagnose_category(DF)
  
  category_table=category_table%>%left_join(DataDictionary,by=c("variables"="Variable"))%>%select(-VarType)
  
  
  Format=NULL
  for(j in 1:nrow(category_table)){
    if(j==1){
      c=c(category_table$Description[1],j)
    }else{
      
      if(category_table$Description[j]!=category_table$Description[j-1]){
        c=c(c,(j-1))
        Format=rbind(Format,c)
        c=c(category_table$Description[j],j)
      }
      
      if(j==nrow(category_table)){
        c=c(c,(j))
        Format=rbind(Format,c)
      }
      
    }
  }
  
  
  names(category_table)[2:6]<-c("Levels","N","Frequency","Ratio","Rank")
  Format=data.frame(Format,stringsAsFactors = F)%>%mutate(X2=as.numeric(X2),X3=as.numeric(X3))
  
  table_namescategorical=Format[,1]
  
  for(j in 1:nrow(Format)){
    tab=kable(category_table[Format[j,2]:Format[j,3],]%>%select(-Description), caption = "") %>%
      kable_styling("striped", full_width = F) %>%
      pack_rows(Format[j,1], Format[j,2]-( Format[j,2]-1), Format[j,3]-( Format[j,2]-1))
    
    assign(paste0("table_cat_",j),category_table[Format[j,2]:Format[j,3],]%>%select(-Description,-variables))
    
  }
  
  
  if(!is.null(datetofilteron)){
    
    # Population Stability Index
    index=which(colnames(DF)==DateName)
    
    test=DF%>%filter_at(index,all_vars(.>=datetofilteron))%>%select(-all_of(DateName))
    train=DF%>%filter_at(index,all_vars(.<datetofilteron))%>%select(-all_of(DateName))
    
    table_psi_names=data.frame(Variable=names(train))%>%left_join(DataDictionary,by="Variable")%>%.$Description
    
    for(j in 1:ncol(train)){
      # plot
      
      t=train[[ names(train)[j]]]
      t2=test[[ names(train)[j]]]
      PSI=psi((t),(t2))
      
      
      
      
      PSI=attributes(PSI)$tbl
      
      assign(paste0("table_psi_",j),PSI)
      
    }
    rm(test,train)
    
    
  }
  
  
  # Histograms
  nums <- unlist(lapply(DF, is.numeric))
  
  xx=orig=DF[ , nums]
  
  
  v=data.frame(Variable=names(xx))%>%left_join(DataDictionary,by="Variable")
  names(xx)<-gsub(" ","",v$Description)
  
  table_nameshistogram= names(xx)
  
  
  for(jj in 1:ncol(xx)){
    # plot
    
    
    g <- orig%>%
      ggplot( aes_string(x=names(orig)[jj])) +
      geom_histogram( bins=50, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      theme_minimal()+
      theme(
        plot.title = element_text(size=20),
        axis.text=element_text(size=12),
        axis.title = element_text(size=15)
      ) #+    ylim(0, 100)
    #
    png(file=paste0(outpath,"Histogram_",jj,".png"), width = 800, height = 600)
    print(g)
    dev.off()
    #
  }
  rm(g,orig,xx)
  
  
  
  
  for(q in 1:ncol(DF)){
  if(length(unique(DF[,q]))==1){
  DF[,q]=as.character(DF[,q])
  }
  
  }  
  idx.numeric <- find_class(DF, type = "numerical")  
  
  if (length(idx.numeric) > 2) {
    eps <- 0.5
    
    table_cors <- DF %>%
      correlate %>%
      filter(abs(coef_corr) >= eps) %>%
      filter(as.integer(var1) > as.integer(var2)) %>%
      arrange(desc(abs(coef_corr)))
    
  } 
  
  if (length(idx.numeric) > 2) {
    png(file=paste0(outpath,"CORR.png"), width = 800, height = 600)
    DF %>%
      plot_correlate()
    dev.off()
    table_plot_corr="YES"
  }else{
    table_plot_corr="NO"
    
  }
  
  
  
  write.csv(DataDictionary,file=paste0(outpath,"DataDictionary.csv"),row.names = F)
  vlist=c(ls()[grepl("table_",ls())],"DateName")
  save(list = vlist,file=paste0(outpath,"RawModelData",".RData"))
  
}














#' @title
#' Population Stability Index
#' @description
#' \code{psi} calculates the popolation stability index.
#' @author Nicholas Reimold
#' @references \href{What is population stability index? - Quora}{https://www.quora.com/What-is-population-stability-index}
#'
#' @param original The original set of a measurement, should be a factor or numeric
#' @param current  The current set of a measurement, should be a factor or numeric
#' @param cut.points It won't work if original and current are factors, and it cannot be NULL if original and current are numerical. This function uses this argument to bin \code{original} and \code{current} with left-closed right-open intervals.
#' @param cut.levels specifies how many levels to split a numeric variable. If \code{cut.points} is provided, this parameter will be ignored. When using this parameter, a numeric variable will be split using \code{cut}.
#'
#' @return a \code{psi} object
#'
#' @details psi measures the stablity of the population. Usually we can believe the population stays the same as the past if psi is less than 0.1, and a significant shift can be recognised if psi is greater than 0.25. The outcome of this function is a numeric, with details stored as attributes. You can use \code{summary} function to see all of the detailed information. Fot the situation where some of the levels has no element in either original population or current population and the psi does not exist for such levels, the empty levels will not be taken into account and a warning will inform you of this. Again, by using \code{summary} you could know everything inside.
#'
#' @examples
#' data("iris")
#' train <- sample(nrow(iris), nrow(iris) * .7)
#' train.species <- iris$Species[train]
#' test.species <- iris$Species[-train]
#' p <- psi(train.species, test.species)
#' p
#' summary(p)
#'
#' @import magrittr
#' @export
#'
psi <- function(original,
                current,
                cut.points = NULL,
                cut.levels = ifelse(is.null(cut.points), 5L, NULL)) {
  
  
  if (!is.null(cut.points)) {
    cut.levels <- NULL
  }
  
  if (!is.null(cut.levels)) {
    if (cut.levels < 3) {
      warning("cut.levels must be an interger greater than 3")
      cut.levels <- 3L
    }
  }
  
  # binning numeric
  label.numeric <- function(x, cuts, na.level = NULL) {
    cuts <- sort(cuts)
    n_cut <- length(cuts)
    level.names <- paste0('<= ', cuts) %>% c(paste0('> ', cuts[n_cut]))
    
    if (any(is.null(na.level))) {
      na.level <- any(is.na(x))
    }
    if (na.level) level.names <- c('Missing', level.names)
    else {
      if(any(is.na(x))) stop('x has NA value while na.level is FALSE')
    }
    
    y <- vector('integer', length(x))
    
    for (i in n_cut:1) {
      y[x <= cuts[i]] <- i
    }
    y[x > cuts[n_cut]] <- n_cut + 1
    
    if (na.level) {
      y <- level.names[y + 1]
      y[is.na(x)] <- 'Missing'
    } else {
      y <- level.names[y]
    }
    
    factor(y, level.names)
  }
  
  
  # try to convert original & current to factor
  if (is.numeric(original) & is.numeric(current)) {
    if (is.null(cut.points)) {
      cut.points <- unname(quantile((original),
                                    seq(0, 1, length.out = cut.levels),
                                    type = 1,
                                    na.rm = TRUE))
      
      if(any(duplicated(round(cut.points,3)))==TRUE){
        cut.points <- unname(quantile(unique(original),
                                      seq(0, 1, length.out = cut.levels),
                                      type = 1,
                                      na.rm = TRUE))
      }
      cut.points <- cut.points[-c(1, length(cut.points))]
      cut.points<-round(cut.points,3)
      
    }
    cut.points=unique(cut.points)
    na.level <- any(is.na(c(original, current)))
    # attr(res, 'original.num') <- original
    # attr(res, 'current.num') <- current
    original <- label.numeric(original, cut.points, na.level)
    current  <- label.numeric(current, cut.points, na.level)
  }
  
  if (!is.factor(original) | !is.factor(current)) {
    stop('original and current should be numeric or factor simultaneously.')
  }
  if (any(levels(original) != levels(current))) {
    common_lv <- union(levels(original), levels(current))
    original <- factor(original, levels = common_lv)
    current  <- factor(current,  levels = common_lv)
    
  }
  
  
  levels.name <- levels(original)
  org.stat.tbl <- tapply(X = original,
                         INDEX = original,
                         FUN = length,
                         simplify = TRUE) %>%
    sapply(function(x) ifelse(is.na(x), 0, x))
  cur.stat.tbl <- tapply(X = current,
                         INDEX = current,
                         FUN = length,
                         simplify = TRUE)%>%
    sapply(function(x) ifelse(is.na(x), 0, x))
  
  tbl <- data.frame(Levels = levels.name,
                    OrgCnt = org.stat.tbl,
                    CurCnt = cur.stat.tbl,
                    OrgPct = org.stat.tbl / sum(org.stat.tbl),
                    CurPct = cur.stat.tbl / sum(cur.stat.tbl))
  tbl$Index <- (tbl$CurPct - tbl$OrgPct) * log(tbl$CurPct / tbl$OrgPct)
  
  psi <- sum(tbl$Index[tbl$OrgCnt != 0 & tbl$CurCnt != 0])
  res <- psi
  tbl <- rbind(tbl, data.frame(Levels = 'Total',
                               OrgCnt = sum(org.stat.tbl),
                               CurCnt = sum(cur.stat.tbl),
                               OrgPct = 1,
                               CurPct = 1,
                               Index = psi))
  rownames(tbl) <- NULL
  
  tbl$OrgPct <- round(tbl$OrgPct, 4)
  tbl$CurPct <- round(tbl$CurPct, 4)
  tbl$Index  <- round(tbl$Index,  4)
  
  attr(res, 'tbl') <- tbl
  # attr(res, 'original') <- original
  # attr(res, 'current')  <- current
  if (any(tbl$OrgCnt == 0 | tbl$CurCnt == 0)) {
    attr(res, 'Empty Levels') <- tbl$Levels[tbl$OrgCnt == 0 | tbl$CurCnt == 0] %>% as.character()
    warning('Some of the levels are empty, and PSI may be inaccurate. Please use `summary` to see the details.')
  }
  class(res) <- c('psi', 'numeric')
  res
}


#' @export
print.psi <- function(x, ...) {
  cat('PSI :', round(x, 4), '\n')
  # NextMethod('print')
}

#' @export
summary.psi <- function(object, ...) {
  cat('PSI:', round(object, 4), '\n\n')
  print(attr(object, 'tbl'))
  
  if (!is.null(attr(object, 'Empty Levels'))) {
    cat('\nEmpty Levels: ', paste0(attr(object, 'Empty Levels'), collapse = ', '), '\n')
  }
  # NextMethod('summary')
}

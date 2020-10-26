
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
PEval=function(ModelResults,SegmentDF,
                     ClassActual="",ClassPrediction="",
                     NumericActual="",NumericPrediction="",
                     grouping_var="",
                     time_var="",
                     DataDict,
                     outpath,cal.model=NULL){

  DataDict=DataDict%>%filter(Variable%in%names(SegmentDF)|Variable%in%names(ModelResults))

  
  forecast.window=max(ModelResults[,time_var],na.rm=T)
  
  options(warn=-1)
  
  ModelResults=ModelResults%>%ungroup()%>%data.frame()
  ModelResults$Time=ModelResults[,time_var]%>%as.numeric()
  ModelResults$Actual=ModelResults[,ClassActual]%>%as.numeric()
  ModelResults$Prediction=ModelResults[,ClassPrediction]%>%as.numeric()
  
  if(NumericPrediction!=""){
    
  ModelResults$NumericPrediction=ModelResults[,NumericPrediction]%>%as.numeric()
  ModelResults$NumericActual=ModelResults[,NumericActual]%>%as.numeric()
  
  }
  ModelResults$grouping_var=ModelResults[,grouping_var]%>%as.character()
  
  
  
    SegmentDF=SegmentDF%>%ungroup()%>%data.frame()
    SegmentDF$grouping_var=SegmentDF[,grouping_var]%>%as.character()
    
  
    namesomit=names(ModelResults)[!names(ModelResults)%in%"grouping_var"]
    namestokeep=names(SegmentDF)[!names(SegmentDF)%in%namesomit]
    SegmentDF=SegmentDF[,namestokeep]
  
    
    ModelResults=ModelResults%>%mutate(Actual=ifelse(is.na(Actual),0,Actual),
                                       Prediction=ifelse(is.na(Prediction),0,Prediction))
    
    
    if(NumericPrediction!=""){
      ModelResults=ModelResults%>%mutate(NumericPrediction=ifelse(is.na(NumericPrediction),0,NumericPrediction),
                                       NumericActual=ifelse(is.na(NumericActual),0,NumericActual))
}
  catList=DataDict%>%filter(VarType=="Factor")%>%.$Variable
  catListDesc=DataDict%>%filter(VarType=="Factor")%>%.$Description
  if(!is.null(cal.model)){
    cal.model_coef=getCoef(cal.model);
    cal.model_coef$`Pr(>|z|)`=round(cal.model_coef$`Pr(>|z|)`,4)
    
    #add significant sign
    alpha=0.05
    ind=which(regexpr('Pr',names(cal.model_coef))>0);#"Pr(>|z|)" or "Pr...z.."
    cal.model_coef$is_Significant=cal.model_coef[[ind]] <=alpha
    #cal.model_coef$is_Significant=cal.model_coef$"Pr...z.."<=alpha
    
    
    cal.model_coef=cal.model_coef%>%rename(Variable=variable)%>%
      left_join(DataDict%>%select(Variable,Description))%>%
      mutate(Description=ifelse(is.na(Description),Variable,Description))
    #add desc
    
    
    
    addDescFun=function(cal.model_coef,catList,catListDesc){
      yy=cal.model_coef$variable;
      cal.model_coef$Description=yy
      for(j in 1:nrow(cal.model_coef)){
        for(i in 1:length(catList)){
          if(catList[i]==yy[j]) cal.model_coef$Description[j]=catListDesc[i]
          else{
            reg= regexpr(catList[i],yy[j]);
            ind1=as.vector(reg)
            ind2=attributes(reg)$match.length-ind1+1
            if(ind1>0) {cal.model_coef$Description[j]=paste0(catListDesc[i],' ', substring(yy[j],ind2+1,999))}
          }
        }
      }
      cal.model_coef
    }
    table_cal.model_coef = cal.model_coef#addDescFun(cal.model_coef,catList,catListDesc)
  }
  
  
if(time_var!=""){
    table_KS_alltimes=NULL
    for(timeWD in 1:forecast.window){
      KS_tbl=ModelResults%>%filter(Time==timeWD)%>%
        mutate(Prediction=ifelse(is.na(Prediction),0,Prediction),
               Actual=ifelse(is.na(Actual),0,Actual),
               decile=dplyr::ntile(Prediction,10))%>%dplyr::select(decile,Actual)%>%group_by(decile)%>%
        mutate(CO_1=sum(Actual),tt_obs=length(Actual),CO_0=tt_obs-CO_1)%>%distinct(decile,CO_1,tt_obs,CO_0)%>%
        arrange(-decile)%>%ungroup%>%mutate(decileUp=11-decile)%>%rename(decileDown=decile)%>%
        mutate(tt_co_1=sum(CO_1),co_rate=round(CO_1/tt_co_1*100,2),cumulative_co_rate=cumsum(co_rate),
               tt_co_0=sum(CO_0),non_co_rate=round(CO_0/tt_co_0*100,2),cumulative_non_co_rate=cumsum(non_co_rate),
               KS=cumulative_co_rate-cumulative_non_co_rate,
               KS_Max=max(KS))%>%mutate(Time=timeWD)
      
      isMax=KS_tbl$KS==KS_tbl$KS_Max
      
      table_KS_alltimes=rbind(table_KS_alltimes,KS_tbl)
    }
    
    
    
    #KS summary across time
    KS_tbl_all_dist=table_KS_alltimes%>%distinct(Time,KS_Max)
    KS_Max_df=data.frame(matrix(KS_tbl_all_dist$KS_Max,nrow=1))
    names(KS_Max_df)=KS_tbl_all_dist$Time
    nm=names(KS_Max_df)
    KS_Max_df=data.frame('Time'='KS(%)',data.frame(KS_Max_df))
    names(KS_Max_df)[-1]=nm
    table_KS_Max_df_summary_across_time=KS_Max_df
    
    
    vlist=c()
    #KS Heatmap
    for(timeWD in 1:forecast.window){
      KS_tbl=table_KS_alltimes%>%filter(Time==timeWD)
      if(nrow(KS_tbl)==0) next;
      KS_tbl$cumulative_co_rate=pmin(100,KS_tbl$cumulative_co_rate)
      KS_tbl$cumulative_non_co_rate=pmin(100,KS_tbl$cumulative_non_co_rate)
      isMax=KS_tbl$KS==KS_tbl$KS_Max
      KS_tbl_heat=KS_tbl%>%dplyr::select(-tt_co_1,-tt_co_0,-KS_Max)
      names(KS_tbl_heat)=c('DecileDown','Number of Chargedoff Accounts','Total Accounts' ,'Number of Non-Chargedoff Accounts', 'DecileUp', 'Chargedoff Rate', 'Cumulative Chargedoff Rate','Non-Chargedoff Rate','Cumulative Non-Chargedoff Rate','KS','Time')
      
      KS_tbl_heat$KS=round(KS_tbl_heat$KS,2)
      KS_tbl_heat$KS=ifelse(isMax==TRUE, cell_spec(KS_tbl_heat$KS, color='red'),KS_tbl_heat$KS)
      KS_tbl_heat=KS_tbl_heat%>%
        kable(escape = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),font_size = 12)
      assign(KS_tbl_heat,x=paste0("KS_tbl_heat_",timeWD))
      vlist=c(vlist,paste0("KS_tbl_heat_",timeWD))
    } 
    save(list= ls()[ls()%in%vlist],file=paste0(outpath,'KS_tbl_heatmaps.RData'))
    
    
    
  }

  timeseq=seq(from=6,to=forecast.window,by=6)
  

  
    #Print KS plots Every 6 months
  for(timeWD in timeseq){
    g_KS=KS_plot(timeWD,table_KS_alltimes)
    if(is.null(g_KS)){ 
    }else {
      png(paste0(outpath,'PD_g_KS_',timeWD,'.png'))
      print(g_KS)
      dev.off()
    }      
    
    g_out=Gini_plot(table_KS_alltimes,timeWD, is_gini1=T,is_gini2=T,is_pietra_index=T)
    
    if(!is.null(g_out$g1_gini)) {
      png(paste0(outpath,'PD_gini1_',timeWD,'.png'));
      print(g_out$g1_gini);
      dev.off()
      
    } 
    
    if(!is.null(g_out$g2_gini_pietra)) {
      
      png(paste0(outpath,'PD_gini2_',timeWD,'.png'))
      print(g_out$g2_gini_pietra)
      dev.off()
      
    }
    
  }
  
 
  
  for(timeWD in timeseq){
    if(ModelResults%>%filter(Time==timeWD)%>%nrow()!=0 &
       ModelResults%>%filter(Time==timeWD)%>%.$Actual%>%unique()%>%length()>1) {
      
      g_roc_auc=ROC_AUC(ModelResults%>%filter(Time==timeWD),timeWD)
      if(!is.null(g_roc_auc)){
        png(paste0(outpath,'PD_g_roc_auc_',timeWD,'.png'))
        print(g_roc_auc)
        dev.off()
        
      }
    }
  }
  rm(g_roc_auc)
  
  #Across all time slots
  if(1){
    table_TimeWD_all=paste(range(ModelResults$Time),collapse='-')

    g_roc_auc=ROC_AUC(ModelResults%>%dplyr::select(Actual,Prediction),TimeWD=table_TimeWD_all);
    png(paste0(outpath,'PD_g_roc_auc_',table_TimeWD_all,'.png'))
    print(g_roc_auc)
    dev.off()
    #track table/plot names
  }
  rm(g_roc_auc)
  
  
  
   
  #combine DF with segmentation info
  SegmentDF=SegmentDF%>%filter(!duplicated(grouping_var))
  SegmentDF=ModelResults%>%left_join(SegmentDF,by="grouping_var")
  
  #--------------------------Backtesting Bucket by Time & categorical variables
  
  
  bucket_out_agg_all=NULL
  
  bucket_out_agg=pd_backtest_bucket_agg(SegmentDF,bucket_by="Time")
  table_PD_bucket_out_agg_Time=bucket_out_agg
  names(bucket_out_agg)[1]='Levels'
  bucket_out_agg=data.frame(VariableName="Time",bucket_out_agg)
  bucket_out_agg_all=rbind(bucket_out_agg_all,bucket_out_agg)
  
  #exclude those with only unique categories
  catList=c(time_var,catList)
  catListDesc=c(time_var,catListDesc)
  ctlist2=c()
  for(ct in catList){
    
    Results2=SegmentDF%>%distinct(Time,get(ct))%>%group_by(Time)%>%summarise(n=n())%>%.$n%>%min()
    if(ct==time_var|(Results2>1&
       unique(SegmentDF[,ct])%>%length()>1 &
       unique(SegmentDF[,ct])%>%length()<=11)
       ){
      bucket_out_list=try(pd_backtest_bucket(SegmentDF,bucket_by=ct),silent = TRUE)
      if(is.character(bucket_out_list)){
        next
      }
      
      assign(paste0("table_bucket_pval_",ct),bucket_out_list$bucket_pval)
      #assign(paste0("table_wide_diff_pct2_",ct),bucket_out_list$wide_diff_pct2)
      assign(paste0("table_is_sig_df_",ct),bucket_out_list$is_sig_df)
      assign(paste0("table_bucket_out_",ct),bucket_out_list$bucket_out)
      
      plot_buckets(PD_Bucket=bucket_out_list$bucket_out,
                   bucket_by=ct, bucket_by_desc=catListDesc[match(ct,catList)])
      
      #------------------------------------------PD Backtesting Bucket by categorical variables aggregate
      
      bucket_out_agg=pd_backtest_bucket_agg(SegmentDF,bucket_by=ct)
      assign(paste0("table_PD_bucket_out_agg_",ct),bucket_out_agg)
  
      names(bucket_out_agg)[1]='Levels'
      bucket_out_agg=data.frame(VariableName=ct,bucket_out_agg)
      bucket_out_agg_all=rbind(bucket_out_agg_all,bucket_out_agg)
      ctlist2=c(ctlist2,ct)
    }
  }
  
  table_PD_bucket_out_agg_all=bucket_out_agg_all
  
  
  #PD backtesting final
  
  PD_Error_final=  SegmentDF%>%ungroup%>%
    mutate(PD_AVG=mean(Prediction),PD_Nbr=sum(Actual),Account_Nbr=length(unique(grouping_var)),  PD_SUM=sum(Prediction,na.rm=T),
           Percent_Default=PD_Nbr/Account_Nbr,
           pd_err_r_pct=case_when(PD_SUM==0 & PD_Nbr==0 ~ 0,
                                  PD_Nbr==0 ~ 100*(PD_SUM/1.0-1),
                                  TRUE ~ 100*(PD_SUM/PD_Nbr-1)))%>% 
    dplyr::select(PD_AVG,PD_SUM, PD_Nbr, Account_Nbr,  Percent_Default,pd_err_r_pct) %>% distinct()%>%
    mutate(PD_AVG=round(PD_AVG,4), PD_SUM=round(PD_SUM,2), Percent_Default=round(Percent_Default,4), pd_err_r_pct=round(pd_err_r_pct,2))
  table_PD_Error_final=PD_Error_final

  
  
  
  
  
  
  #-----------------------------------------Dollar Loss Error rate bucket
  if(NumericPrediction!=""){
  
  for(ct in ctlist2){
    #ct_desc=catListDesc[match(ct,catList)]
    assign(paste0('table_Loss_ErrorRate_bucket_',ct),Loss_ErrorRate_bucket_f(SegmentDF,bucket_by=ct))
  }
  
  
  #Dollar Loss vs. Time plot
  table_ErrorRate_bucket_Time=Loss_ErrorRate_bucket_f(SegmentDF,bucket_by='Time')
  Error_long <- gather(table_ErrorRate_bucket_Time%>%dplyr::select(Time,Pred.Losses, Actual.Losses), 
                       key='condition', value='measurement', Pred.Losses, Actual.Losses,  factor_key=TRUE)
  Error_long$measurement_1000=Error_long$measurement/1000
  
  facColor=c('orange','black')
  uniq_fac=c('Predictive Losses','Actual Losses')
  g_DollarLoss=ggplot(Error_long,aes(x=Time, y=measurement_1000, group=condition, color=condition)) +
    geom_line(size=1)+
    ggtitle(paste0("Dollar Loss"))+
    scale_color_manual(name='Loss Type', values=facColor,labels=uniq_fac) +
    scale_x_continuous(name = "Time (Month)",breaks=1:max(Error_long$Time)) +
    theme_bw() +
    scale_y_continuous(name = "Dollar Loss (*1000)")
  
  png(paste0(outpath,'g_DollarLoss.png'))
  print(g_DollarLoss)
  dev.off()
  
  
  #error rate with time
  g_ErrorRate_Time=ggplot(table_ErrorRate_bucket_Time,aes(x=Time, y=Error)) +
    geom_line(size=1)+
    geom_point(size=2)+
    ggtitle(paste0("Error Rate with Time"))+
    #  scale_color_manual(name='Loss Type', values=facColor,labels=uniq_fac) +
    scale_x_continuous(name = "Time (Month)",breaks=1:max(Error_long$Time)) +
    theme_bw() +
    scale_y_continuous(name = "Error Rate") +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "gray", size=1)
  
  png(paste0(outpath,'g_ErrorRate_Time.png'))
  print(g_ErrorRate_Time)
  dev.off()
  
  
  # Final Loss Error Rate
  table_Loss_ErrorRate=SegmentDF%>% 
    summarise(Accounts = length(unique(grouping_var)),
              AUC = round(as.numeric(pROC::auc(roc(Actual, Prediction))),2),
              Pred.Losses = sum(NumericPrediction,na.rm=T),
              Actual.Losses = sum(NumericActual,na.rm=T),
              Error = round(Pred.Losses/Actual.Losses - 1 ,4)           )
  
  
}

  vlist=c(ls()[grepl("table_",ls())],"forecast.window","catList","catListDesc")
  save(list = vlist,file=paste0(outpath,"PerformanceTables.RData"))
  
}






KS_plot=function(timeWD,table_KS_alltimes){
  
  
  table_KS_alltimes=table_KS_alltimes%>%filter(Time==timeWD)
  
  
  if(nrow(table_KS_alltimes)==0) return (NULL)
  
  KS_Max=max(table_KS_alltimes$KS)
  if(is.na(KS_Max)) return (NULL)
  
  #from wide to long
  out_long <- gather(table_KS_alltimes%>%dplyr::select(decileDown,decileUp,cumulative_co_rate, cumulative_non_co_rate), 
                     key='condition', value='measurement', cumulative_co_rate, cumulative_non_co_rate, factor_key=TRUE)
  
  cum_diff=table_KS_alltimes$cumulative_co_rate-table_KS_alltimes$cumulative_non_co_rate
  ind=which(cum_diff==max(cum_diff))
  x0=ind
  y1=table_KS_alltimes$cumulative_co_rate[ind]
  y0=table_KS_alltimes$cumulative_non_co_rate[ind]
  
  if(1){#new
    add_0=rbind(head(out_long,1),tail(out_long,1))
    add_0$decileDown=c(11,11);
    add_0$decileUp=11-add_0$decileDown
    add_0$measurement=0
  }
  
  if(0){#raw
    add_0=rbind(head(out_long,2),tail(out_long,2))
    add_0$decileDown=c(11,0,11,0);
    add_0$decileUp=11-add_0$decileDown
    add_0$measurement=0
  }
  out_long2=rbind(out_long,add_0)%>%arrange(condition,-decileDown)%>%data.frame
  target_color='red'
  non_target_color='black'
  out_long2=out_long2%>%mutate(ks_color=ifelse(condition=='cumulative_co_rate',target_color,non_target_color))
  ks_color=out_long2$ks_color
  #KS interpretation:
  #For Time=12 case, top 1~4 decile (top 40% of risky) accounts 
  #capture 73.1% of chargedoffs; 
  #capture 39.7% of non-chargedoffs; 
  #KS=73.1%-39.7%=33.4% is the maximum difference
  
  g_KS=
    ggplot(out_long2, aes(x=decileUp, y=measurement,group=condition,linetype=condition,color=condition)) + 
    geom_line(color=ks_color,size=1,linetype='solid') + 
    
    geom_point(color=ks_color,size=2) + 
    geom_text(aes(label=round(measurement,1)),color='black',position = position_nudge(x = -.4)) +
    scale_x_continuous(limits=c(0,10),name = "Decile", breaks = 0:10) +
    #scale_y_continuous(name = "CO_1",limits=c(1,10)) 
    scale_y_continuous(name = "Cumulative Rate (%)", breaks = seq(0,100,by=10))+
    #coord_cartesian(xlim = c(0, 10),breaks = 0:10) + 
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = 'black') +
    geom_point(aes(x = x0[1] , y= y0[1]), color=non_target_color, size=1) +
    geom_point(aes(x = x0[1] , y= y1[1]), color=target_color, size=1) +
    ggtitle(paste0("Kolomogorov-Smirnov Chart, Time=",timeWD))+
    geom_line()+
    annotate(
      geom = "text", x = x0[1], y =(y0[1]+y1[1])/2, 
      label = paste0('KS=',round(KS_Max,1),'%'), hjust = 1, vjust = 1, size = 3.5,color='black') +
    scale_color_manual(name="condition", 
                       labels = c('cumulative_co_rate', 'cumulative_non_co_rate'), 
                       values = c('cumulative_co_rate'=target_color, 
                                  'cumulative_non_co_rate'=non_target_color))+
    scale_linetype_manual(name="condition", 
                          values = c('solid', 'solid'))+
    theme(legend.position = c(0.8,0.1))+
    #theme_bw()+
    #theme(plot.background = element_rect(
    #                        fill = "white",
    #                       colour = "black",
    #                      size = 1))+
    theme( axis.line = element_line(colour = "black", 
                                    size = 1, linetype = "solid"))
  
  #      theme(panel.grid.major = element_blank(), 
  #        panel.grid.minor = element_blank(),
  #       panel.background = element_rect(colour = "black", size=4))
  
  g_KS 
}



Gini_plot=function(table_KS_alltimes,timeWD, is_gini1=T,is_gini2=T,is_pietra_index=T){
  
  
  table_KS_alltimes=table_KS_alltimes %>%filter(Time==timeWD)
  
  
  g1_gini=NULL;g2_gini=NULL;g2_gini_pietra=NULL;
  
  if(nrow(table_KS_alltimes)==0) return (NULL)
  
  KS_Max=max(table_KS_alltimes$KS)
  if(is.na(KS_Max)) return (NULL)
  
  fillColor=c('brown','red','orange','yellow','green','blue', 'darkviolet','darkgray','lightgrey','skyblue');#'indigo','turqoise'
  
  table_KS_alltimes$cum_co_1_down=cumsum(table_KS_alltimes$CO_1)
  table_KS_alltimes$cum_co_1_up=cumsum(rev(table_KS_alltimes$CO_1))
  
  #prepare for Pietra_Ind calculation
  table_KS_alltimes$equalLine_co_rate=10
  table_KS_alltimes$equalLine_co_rate_cum=cumsum(table_KS_alltimes$equalLine_co_rate)
  table_KS_alltimes$cdf_co_rate=table_KS_alltimes$cum_co_1_up/table_KS_alltimes$tt_co_1*100
  table_KS_alltimes$Pietra=table_KS_alltimes$equalLine_co_rate_cum-table_KS_alltimes$cdf_co_rate
  table_KS_alltimes$Pietra_Ind=max(table_KS_alltimes$Pietra)
  table_KS_alltimes$Pietra_Ind_x0=which(table_KS_alltimes$Pietra==table_KS_alltimes$Pietra_Ind)%>%tail(1)
  Pietra_Ind_x0=which(table_KS_alltimes$Pietra==table_KS_alltimes$Pietra_Ind)%>%tail(1)
  Pietra_Ind_y0=table_KS_alltimes$cum_co_1_up[Pietra_Ind_x0]
  
  #y=a+b*x
  cum_co_1_up_min=min(table_KS_alltimes$cum_co_1_up)
  cum_co_1_up_max=max(table_KS_alltimes$cum_co_1_up)
  decileUp_min=min(table_KS_alltimes$decileUp)
  decileUp_max=max(table_KS_alltimes$decileUp)
  b=(cum_co_1_up_max-cum_co_1_up_min)/(decileUp_max-decileUp_min)
  a=cum_co_1_up_min-b*decileUp_min
  Pietra_Ind_y1=a+b*Pietra_Ind_x0
  
  
  if(is_gini1){
    max_cum_co_1_up=max(table_KS_alltimes$cum_co_1_up)
    g1_gini <- 
      ggplot(table_KS_alltimes, 
             # keep all aesthetics in one place
             aes(x = decileDown, y = CO_1, fill =fillColor ,label  =CO_1)) +
      # replacement of geom_bar(stat = "identity")
      geom_col(fill=fillColor) +
      # avoid overlap of text and bar to make text visible as bar and text have the same colour 
      geom_text(nudge_y = 0.75,color='black',vjust=-0.1) + 
      # alternatively, print text inside of bar in discriminable colour
      # geom_text(nudge_y = -1, color = "black") + 
      ggtitle("Chargedoff Number per Decile") + 
      #xlab("Decile Down") + ylab("Number of ChargedOffs") +
      theme_bw() + theme(legend.position = "none") + 
      theme(plot.title = element_text(hjust = 0))+
      scale_x_continuous(name = "Decile Down", breaks = 1:10)+
      scale_y_continuous(name = "Number of Chargedoff", limits=c(0,max_cum_co_1_up) )
  }
  if(is_gini2 | is_pietra_index){
    
    tmp=table_KS_alltimes%>%arrange(decileDown)
    idx <- rep(1:nrow(tmp), times=10:1)
    tmp2=tmp[idx,]%>%group_by(decileDown)%>%mutate(new_decileDown=1:length(decileDown))%>%data.frame()
    #tmp3=tmp2%>%mutate(new_decileDown2=11-new_decileDown)%>%arrange(new_decileDown2,decileDown)
    tmp3=tmp2%>%mutate(new_decileDown2=11-new_decileDown)%>%arrange(-decileDown,new_decileDown2)
    tmp3=tmp3%>%group_by(new_decileDown2)%>%mutate(group_sum=sum(CO_1))
    group_sum_min=min(tmp3$group_sum)
    group_sum_max=max(tmp3$group_sum)
    new_decileDown2_max=max(tmp3$new_decileDown2)
    new_decileDown2_min=min(tmp3$new_decileDown2)
    g2_gini=ggplot(tmp3, 
                   # keep all aesthetics in one place
                   aes(x = new_decileDown2, y = CO_1, fill =factor(decileDown), label=CO_1, order=-decileDown)) +
      #geom_bar(stat='identity') +
      geom_col(position = position_stack(reverse=T)) +
      scale_fill_manual(values = rev(fillColor))+
      scale_x_continuous(name = "Decile", breaks = 1:10) +
      geom_text(aes(label=group_sum,y=group_sum),size=3,vjust=-.25)+
      geom_line(aes(new_decileDown2,y=group_sum))+
      scale_y_continuous(name = "Cumulative Number of Chargedoff" )+
      geom_segment(x=new_decileDown2_min,y=group_sum_min,xend=new_decileDown2_max,yend=group_sum_max, linetype='dashed', color='black')+
      theme_bw() + theme(legend.position = "none") +
      ggtitle("Cumulative Chargedoff Number per Decile") 
  }
  if(is_pietra_index){#pietra index
    Pietra_Ind=round(max(table_KS_alltimes$Pietra/100),2)
    g2_gini_pietra= g2_gini+
      geom_segment(x=Pietra_Ind_x0,y=Pietra_Ind_y0,xend=Pietra_Ind_x0,yend=Pietra_Ind_y1, linetype='dashed', color='red')+
      ggtitle("Cumulative Chargedoff Number per Decile with Pietra Index") +
      annotate(geom = "text", x = Pietra_Ind_x0, y =Pietra_Ind_y1, 
               label = paste0('Pietra Index=',Pietra_Ind), hjust = 1, vjust = 1, size = 3,color='red')
  }
  return(list('g1_gini'=g1_gini,'g2_gini'=g2_gini,'g2_gini_pietra'=g2_gini_pietra))
}


ROC_AUC=function(ModelResults,TimeWD){
  pROC_obj <- pROC::roc(as.factor(ModelResults$Actual),ModelResults$Prediction,
                        smoothed = TRUE,
                        # arguments for ci
                        ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                        # arguments for plot
                        plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                        print.auc=TRUE, show.thres=TRUE)
  auc_out=pROC::auc(pROC_obj)
  
  g_roc_auc = ggroc(pROC_obj)+ 
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
    theme_minimal() + ggtitle(paste0('Time=',TimeWD)) + 
    annotate(geom = "text", x = 0.5, y =0.5, 
             label = paste0('AUC=',round(auc_out,2)), hjust = 1, vjust = 1, size = 3.5,color='black')+
    #geom_sf(aes(fill = AREA))+
    theme_bw() 
  if(1==2){#take long time to run
    sens.ci <- ci.se(pROC_obj)
    plot(sens.ci, type="shape", col="lightblue")
    ## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
    ## definition shape.
    plot(sens.ci, type="bars")
  }
  
  return(g_roc_auc)
}



#--------------------------Backtesting Bucket by Time & categorical variables


#plot_buckets
plot_buckets=function(PD_Bucket,bucket_by='TIER_SNP',bucket_by_desc){
  ind=match(bucket_by,names(PD_Bucket))
  uniq_fac=unique(PD_Bucket[[ind]]);#PD_Bucket=bucket_out
  uniq_fac_n=length(uniq_fac)
  
  facColor=c('red','brown','orange','yellow','green','blue','darkviolet','darkgray','lightgrey','black','skyblue')%>%head(uniq_fac_n);
  if(bucket_by=="Time"){
    facColor_df=data.frame("Time",facColor[1])
    
  }else{
    facColor_df=data.frame(uniq_fac,facColor)
  }
  names(facColor_df)[1]=bucket_by
  
  if(bucket_by=="Time"){
    PD_Bucket2=PD_Bucket%>%mutate(color="red")
    
    }else{
      PD_Bucket2=PD_Bucket%>%left_join(facColor_df)
      
    }
  
  ind=match(bucket_by,names(PD_Bucket2))
  if(is.numeric(PD_Bucket2[[ind]])&bucket_by!="Time"){      
    PD_Bucket2[[ind]]=as.factor(as.character(PD_Bucket2[[ind]]))
  }
  if(bucket_by!="Time"){      
    PD_Bucket2[[ind]]=as.factor(as.character(PD_Bucket2[[ind]]))
  # Actual Default Percent 
  g_Actual_D_Pct_Bucket=ggplot(PD_Bucket2,aes(x=Time, y=Percent_Default, group=get(bucket_by), color=get(bucket_by))) +
    geom_line(size=1)+
    ggtitle(paste0("Actual Default Percent by ",bucket_by_desc))+
    scale_color_manual(name=bucket_by_desc, values=facColor,labels=uniq_fac) +
    scale_x_continuous(name = "Time (Month)",breaks=1:max(PD_Bucket2$Time)) +
    theme_bw() +
    scale_y_continuous(name = "Actual Default Percent")
  
  png(paste0(outpath,'g_Actual_D_Pct_Bucket_',bucket_by,'.png'))
  print(g_Actual_D_Pct_Bucket)
  dev.off()
  
  #Predicted Default Average
  g_PD_Bucket_AVG=ggplot(PD_Bucket2,aes(x=Time, y=PD_AVG, group=get(bucket_by), color=get(bucket_by))) +
    geom_line(size=1)+
    ggtitle(paste0("Predicted Default Average by ",bucket_by_desc))+
    scale_color_manual(name=bucket_by_desc, values=facColor,labels=uniq_fac) +
    scale_x_continuous(name = "Time (Month)",breaks=1:max(PD_Bucket2$Time)) +
    theme_bw() +
    #+ theme(legend.position = "none") + 
    #      theme(plot.title = element_text(hjust = 0))+
    #      scale_x_continuous(name = "Decile Down", breaks = 1:10)+
    scale_y_continuous(name = "Predicted Default Average")
  
  
  png(paste0(outpath,'g_PD_Bucket_AVG_',bucket_by,'.png'))
  print(g_PD_Bucket_AVG)
  dev.off()
  }else{
    # Actual Default Percent 
    g_Actual_D_Pct_Bucket=ggplot(PD_Bucket2,aes(x=Time, y=Percent_Default)) +
      geom_line(size=1)+
      ggtitle(paste0("Actual Default Percent by ",bucket_by_desc))+
      scale_color_manual(name=bucket_by_desc, values=facColor,labels=uniq_fac) +
      scale_x_continuous(name = "Time (Month)",breaks=1:max(PD_Bucket2$Time)) +
      theme_bw() +
      scale_y_continuous(name = "Actual Default Percent")
    
    png(paste0(outpath,'g_Actual_D_Pct_Bucket_',bucket_by,'.png'))
    print(g_Actual_D_Pct_Bucket)
    dev.off()
    
    #Predicted Default Average
    g_PD_Bucket_AVG=ggplot(PD_Bucket2,aes(x=Time, y=PD_AVG)) +
      geom_line(size=1)+
      ggtitle(paste0("Predicted Default Average by ",bucket_by_desc))+
      scale_color_manual(name=bucket_by_desc, values=facColor,labels=uniq_fac) +
      scale_x_continuous(name = "Time (Month)",breaks=1:max(PD_Bucket2$Time)) +
      theme_bw() +
      #+ theme(legend.position = "none") + 
      #      theme(plot.title = element_text(hjust = 0))+
      #      scale_x_continuous(name = "Decile Down", breaks = 1:10)+
      scale_y_continuous(name = "Predicted Default Average")
    
    
    png(paste0(outpath,'g_PD_Bucket_AVG_',bucket_by,'.png'))
    print(g_PD_Bucket_AVG)
    dev.off()
  } 
}


pd_backtest_bucket_agg=function(SegmentDF,bucket_by='TIER_SNP'){
  #Aggregate pd results by bucket_by
  
  PD_Bucket=  SegmentDF%>%group_by(get(bucket_by))%>%
    mutate(PD_AVG=mean(Prediction),PD_Nbr=sum(Actual),Account_Nbr=length(unique(grouping_var)),  PD_SUM=sum(Prediction,na.rm=T),
           Percent_Default=PD_Nbr/Account_Nbr) %>% 
    dplyr::select("get(bucket_by)", PD_AVG,PD_SUM, PD_Nbr, Account_Nbr,   Percent_Default) %>% distinct()
  ind=match("get(bucket_by)",names(PD_Bucket))
  names(PD_Bucket)[ind]=bucket_by
  #ind2=match(c('Time',bucket_by),names(PD_Bucket))
  
  bucket_out=PD_Bucket%>%data.frame;#%>%arrange_at(ind2)
  
  bucket_out=bucket_out%>%mutate(
    Diff_pct=case_when(PD_AVG==0 & Percent_Default==0 ~ 0,
                       Percent_Default==0 ~ round(100*(PD_AVG/1.0-1),2),
                       TRUE ~ round(100*(PD_AVG/Percent_Default-1),2)))
  
  bucket_out=bucket_out%>%mutate(
    pd_err_r_pct=case_when(PD_SUM==0 & PD_Nbr==0 ~ 0,
                           PD_Nbr==0 ~ 100*(PD_SUM/1.0-1),
                           TRUE ~ 100*(PD_SUM/PD_Nbr-1)))
  
  return(bucket_out)
}

#-----------------------------------------Dollar Loss Error rate bucket
Loss_ErrorRate_bucket_f=function(SegmentDF,bucket_by='TIER_SNP'){
  ErrorRate_bucket=SegmentDF%>% filter(Time>0)%>% group_by( get(bucket_by))%>%
    summarise(Accounts = length(unique(grouping_var)), CO_1_sum=sum(Actual),
              #AUC = as.numeric(pROC::auc(roc(Actual, Prediction))),
              Pred.Losses = sum(NumericPrediction,na.rm=T),
              Actual.Losses = sum(NumericActual,na.rm=T),
              Error = round(Pred.Losses/Actual.Losses - 1,4)
    )
  ind=match("get(bucket_by)",names(ErrorRate_bucket))
  names(ErrorRate_bucket)[ind]=bucket_by
  return(ErrorRate_bucket)
}


coef_sig_heat=function(f,colName,crit1=0.95,crit2=1.05,coef_sig){
  #https://stackoverflow.com/questions/51569560/dplyrs-mutate-at-each-column-separately-with-a-custom-function-of-several-param
  #example: out=coef_sig%>%dplyr::select(2,3) %>% mutate_all(funs(coef_sig_heat(., quo_name(quo(.)) )))
  sig_colName=sub('coef','sig',colName);
  sig_f=coef_sig[,sig_colName]
  coef_All=coef_sig$coef_All
  c1=crit1*coef_All
  c2=crit2*coef_All
  
  bkgd=ifelse(sig_f==TRUE , '','lightgray')
  txt_col=ifelse(is.na(f) | (pmin(c1,c2)<f & f<pmax(c1,c2)),'black', 'red');
  f=case_when(bkgd=='lightgray' & txt_col=='red'   ~ cell_spec(round(f,3), background = 'lightgray', color='red'),
              bkgd=='lightgray' & txt_col=='black' ~ cell_spec(round(f,3), background = 'lightgray', color='black'),
              bkgd=='' & txt_col=='red'   ~ cell_spec(round(f,3), background = '', color='red'),
              bkgd=='' & txt_col=='black' ~ cell_spec(round(f,3), background = '', color='black'),
              TRUE ~ cell_spec(round(f,3), background = '', color='black'))
}

compressPDCoeff=function(pdPREV){
  
  #PD Coefficients
  #add significant sign
  alpha=0.05
  ind=which(regexpr('Pr',names(pdPREV))>0);#"Pr(>|z|)" or "Pr...z.."
  pdPREV$is_Significant=pdPREV[[ind]] <=alpha
  
  #from long to wide
  #data_wide=data_wide_coef
  data_wide_coef <- tidyr::spread(pdPREV%>%dplyr::select(variable, coef,fold), fold, coef)
  names(data_wide_coef)[-1]=paste0('coef_f',names(data_wide_coef)[-1])
  names(data_wide_coef)[ncol(data_wide_coef)]='coef_All'
  data_wide_sig<- spread(pdPREV%>%dplyr::select(variable, is_Significant,fold), fold, is_Significant)
  names(data_wide_sig)[-1]=paste0('sig_f',names(data_wide_sig)[-1])
  names(data_wide_sig)[ncol(data_wide_coef)]='sig_All'
  coef_sig=data_wide_coef%>%left_join(data_wide_sig,by='variable')
  return(coef_sig)
}

makePDSigTable=function(coef_sig){
  crit1=0.95;crit2=1.05
  ind=which(regexpr('coef_',names(coef_sig))>0)
  coef_sig_heat_out = coef_sig[,ind] %>% mutate_all(funs(coef_sig_heat(., quo_name(quo(.)) ,crit1=0.95,crit2=1.05,coef_sig)))
  coef_sig_heat_out = cbind(variable=coef_sig$variable,coef_sig_heat_out)%>%
    kable(escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),font_size = 12)
  
  
  
  return(coef_sig_heat_out)
  
} 

getCoef=function(fit){
  #Get coefficients of a model, for example PD and EAD models
  coef =summary(fit)$coefficients
  #data.frame(VariableName=names(coef),Estimate=as.vector(coef))
  tibble::rownames_to_column(as.data.frame(coef), var='variable')
}



pd_backtest_bucket=function(SegmentDF,bucket_by){
  #Aggregate pd results by bucket_by

  PD_Bucket=  SegmentDF%>%group_by(Time,get(bucket_by))%>%
    mutate(PD_AVG=mean(Prediction,na.rm=T),PD_Nbr=sum(Actual,na.rm=T),
           Account_Nbr=length(unique(grouping_var)),  PD_SUM=sum(Prediction,na.rm=T),
           Percent_Default=PD_Nbr/Account_Nbr) %>% 
    dplyr::select(Time, "get(bucket_by)", PD_AVG,PD_SUM, PD_Nbr, Account_Nbr,   Percent_Default) %>% distinct()
  

  
  
  ind=match("get(bucket_by)",names(PD_Bucket))
  names(PD_Bucket)[ind]=bucket_by
  ind2=match(c('Time',bucket_by),names(PD_Bucket))
  
  
  #if bucketing by time dont want to group by time twice
  if(bucket_by=="Time"){
    PD_Bucket=  SegmentDF%>%group_by(Time)%>%
      mutate(PD_AVG=mean(Prediction,na.rm=T),PD_Nbr=sum(Actual,na.rm=T),
             Account_Nbr=length(unique(grouping_var)),  PD_SUM=sum(Prediction,na.rm=T),
             Percent_Default=PD_Nbr/Account_Nbr) %>% 
      dplyr::select(Time,  PD_AVG,PD_SUM, PD_Nbr, Account_Nbr,   Percent_Default) %>% distinct()
    ind2=ind2[1]
  }
  
  
  
  bucket_out=PD_Bucket%>%data.frame%>%arrange_at(ind2)
  
  bucket_out=bucket_out%>%mutate(
    Diff_pct=case_when(PD_AVG==0 & Percent_Default==0 ~ 0,
                       Percent_Default==0 ~ round(100*(PD_AVG/1.0-1),2),
                       TRUE ~ round(100*(PD_AVG/Percent_Default-1),2)))
  
  
  if(bucket_by!="Time"){
  #t test, p_value and significant
  alpha=0.05; 
  bucket_pval=bucket_out%>%group_by(Time)%>%
    mutate(PD_diff=PD_AVG-Percent_Default,
           t_test_Pval=t.test(PD_diff,mu=0)$p.value)%>%
    distinct(Time,t_test_Pval)%>%
    mutate(is_significant=as.integer(t_test_Pval<=alpha))
  
  is_sig_df=data.frame(matrix(bucket_pval$is_significant,nrow=1))
  names(is_sig_df)=bucket_pval$Time
  nm=names(is_sig_df)
  is_sig_df=data.frame('Variable'=bucket_by,'Time'='is_Significant',data.frame(is_sig_df))
  names(is_sig_df)[-c(1:2)]=nm
  }else{
    bucket_pval=is_sig_df=NULL
}
  PD_Bucket=bucket_out
  
  ind=match(bucket_by,names(PD_Bucket))
  wide_diff_pct <- tidyr::spread(PD_Bucket%>%dplyr::select(Time,ind,Diff_pct), Time, Diff_pct)
  #wide_diff_pct <- tidyr::spread(PD_Bucket[,ind], Time, Diff_pct)
  added=PD_Bucket%>%dplyr::select(Time,ind,PD_SUM,PD_Nbr)%>%group_by(Time)%>%mutate(
    tt_pd_sum=round(sum(PD_SUM),3),tt_pd_nbr=round(sum(PD_Nbr),0))%>%distinct(Time,tt_pd_sum,tt_pd_nbr)%>%
    mutate(pd_err_r_pct=case_when(tt_pd_sum==0 & tt_pd_nbr==0 ~ 0,
                                  tt_pd_nbr==0 ~ round(100*(tt_pd_sum/1.0-1),2),
                                  TRUE ~ round(100*(tt_pd_sum/tt_pd_nbr-1),2)))
  
  added_t=data.frame('total'=c('tt_prob_default','tt_default_nbr','pd_err_r_pct'),rbind(added$tt_pd_sum,added$tt_pd_nbr,added$pd_err_r_pct))
 # wide_diff_pct2=as.data.frame(rbind(as.matrix(wide_diff_pct), as.matrix(added_t)))
  
  return(list(wide_diff_pct=wide_diff_pct,#wide_diff_pct2=wide_diff_pct2,
              bucket_out=bucket_out,bucket_pval=bucket_pval,is_sig_df=is_sig_df))
}

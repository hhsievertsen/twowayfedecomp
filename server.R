library("shiny")
library("lfe")
source("simulate_data.R")
library("bacondecomp")
library("tidyverse")
library("patchwork")
library("kableExtra")
library("ggpubr")
server <- function(input, output) {
  
output$distPlot <- renderPlot({       
##################################### Simulate data  ##############################################
  T2<-input$group2treatment
  T3<-input$group3treatment
  G2=input$group2size
  G3=input$group3size
  df<-simulate_data(
    seed=input$seed,   
    T2=T2,                                                       # Treatment timing group 2
    T3=T3,                                                       # Treatment timing group 3
    m2=input$group2treatmenteffect,                              # Treatment effect group 2
    m3=input$group3treatmenteffect,                              # Treatment effect group 3
    mt2=input$group2timeeffect,                                  # Treatment effect growth rate  group 2
    mt3=input$group3timeeffect,                                  # Treatment effect growth rate  group 3
    G2=G2,                                                       # Size group 2
    G3=G3                                                        # Size group 3
  )
############################################Output######################################################

  df_for_output<-select(df,id,t,G,D,y)
  output$download <- downloadHandler(
    filename = function(){"simulated_data.csv"}, 
    content = function(fname){
      write.csv(df_for_output, fname,row.names=FALSE)
    })
##################################### Estimate 2way FE  ######################################################
  beta_twowayDD<-felm(y ~ D | G+ t, df) # estimate two way DD

  output$RegSum2 <- renderPrint(summary(beta_twowayDD))              # Post output from regression
##################################### DGP  ##################################################################
  # By group
  df_dgp<-df%>%group_by(G)%>%filter(D==1)%>%summarise(ATT_DGP=round(mean(mean)-1,3))%>%
    mutate(type=ifelse(G==2,"Group 2 ATT", "Group 3 ATT"),population_weight=ifelse(G==2,round(G2/(G2+G3),3),round(G3/(G2+G3),3)),
           order=ifelse(G==2,2,5))%>%
    ungroup()%>%select(-G)%>%select(type,ATT_DGP,population_weight,order)
  # Overall
  df_dgp<-rbind(df_dgp,
                df_dgp%>%mutate(wDgp=ATT_DGP*population_weight)%>%summarise(ATT_DGP=round(sum(wDgp),3))%>%mutate(type="ATT",population_weight="",order=1))

##################################### Goodman-Bacon Decomposition ####################################### #### 

      if (T2!=T3){
      # Decompose
       df_bacon <- bacon(y ~ D,data = df,id_var = "id",time_var = "t")    # Bacon decomp
      # Adjust table to have same format
      df_bacon_clean<-df_bacon%>%
          mutate(type=ifelse(row_number()==1,"-Group 2 as T & never treated as C",
                      ifelse(row_number()==2,"-Group 3 as T & never treated as C",
                      ifelse(row_number()==3,"-Group 3 as T & group 2 as C",
                      "-Group 2 as T & group 3 as C" ))),
                      order=ifelse(row_number()==1,3,ifelse(row_number()==2,6,ifelse(row_number()==3,7,4))),
                 estimate=round(estimate,3),weight=round(weight,3))%>%
          select(type,estimate,weight,order)%>%mutate(ATT_DGP="",population_weight="") 
      
      # Calculate group estimated ATT
      df_bacon_att<-df_bacon%>%mutate(watt=weight*estimate)%>%
        group_by(treated)%>%summarise(weight=sum(weight),att=sum(watt))%>%
        mutate(estimate=att/weight,type = ifelse(treated==T2, "Group 2 ATT", "Group 3 ATT"),order=ifelse(treated==T2,2,5))%>%
        mutate(weight=round(weight,3))%>%select(type,estimate,weight,order)
      # Append overall estimated ATT (two way FE)
      df_bacon_att<-rbind(df_bacon_att,
                          tibble(type="ATT",estimate=beta_twowayDD$beta[1,1],weight="",order=1))
    
##################################### Create overall table #########################################
      #Append DGP and ATTs
      merged<-merge(df_bacon_att,df_dgp,by=c("type","order"),all.x = TRUE)%>%
        mutate(estimate=round(estimate,3))
      #Append Goodman Decomp
      df_disp<-rbind(df_bacon_clean,merged)
      # Final stuff
      df_disp<-df_disp%>%arrange(order)%>%select(type,ATT_DGP,population_weight,estimate,weight,order)%>%
          mutate(weight=ifelse(row_number()==1,"",weight))
      # Format table
      # Overall
      overall<-df_disp%>%filter(order%in%c(1,2,5))%>%
        mutate(estimate=ifelse(order==1,estimate,""))%>%select(-weight,-order)
      #%>%
       # knitr::kable("html",align = c("l","c","c","c","c"),col.names = c(" ","DGP","Pop share","Two FE  Estimate")) %>%
      #kable_styling("striped", full_width = F) 
      #
      bacon<-df_disp%>%filter(row_number(weight)>2)%>%
        select(-order,-ATT_DGP,-population_weight)
      #%>%
       # knitr::kable("html",align = c("l","c","c","c"),col.names = c(" ","DD Estimate           ",
        #                                                                     "Weight        ")) %>%
        #kable_styling("striped", full_width = F) 
      
      # Bacon
      output$bacon <- renderTable(                                   # Post Bacon decomposition
        bacon )
      # overall
      output$RegSum1 <- renderTable(                                   # Post overall
        overall )
      }
      else{
        # No variation in timing just report 2way FE
        df_bacon_att<-tibble(type="ATT",estimate=beta_twowayDD$beta[1,1],weight="",order=1)
        #Append DGP 
        df_bacon_att<-merge(df_bacon_att,df_dgp,by=c("type","order"),all.x = TRUE)%>%
          mutate(estimate=round(estimate,3),weight=round(estimate,3))
        df_disp<-rbind(df_bacon_att)
        df_disp<-df_disp%>%arrange(order)%>%select(type,ATT_DGP,-order,estimate,-population_weight,-weight)%>%filter(row_number()==1)
        overall<-df_disp%>%knitr::kable("html",align = c("l","c","c"),col.names = c(" ","DGP","Two FE  Estimate")) %>%
          kable_styling("striped", full_width = F)
        output$RegSum1 <- renderPrint(                                   # Post overall
          overall )
        bacon<-tibble(c="Not applicable")
        #%>%knitr::kable("html",align = c("l"),col.names = c(" ")) %>%
          kable_styling("striped", full_width = F)
        # Bacon
        output$bacon <-  renderTable( cdtab )
      }
      # Overall
  
   
##################################### Event study 
        
        # event study
        df_es<-df%>%mutate(time_to_treatment=ifelse(G==2,t-T2,ifelse(G==3,t-T3,NA)))
        df_es<-df_es%>%group_by(time_to_treatment)%>%mutate(ymean=mean(y)-1)%>%
          group_by(id)%>%mutate(y=ifelse(G==2,y-y[T2-1],ifelse(G==3,y=y-y[T3-1],NA)))%>%
          mutate(post=ifelse(time_to_treatment<1,0,1))%>%
          group_by(post)%>%mutate(ymean_prepost=mean(y))
        c2<-ggplot(df_es)+geom_jitter(aes(x=time_to_treatment,y=y,colour=as.factor(G)),alpha=0.1)+
          geom_point(aes(x=time_to_treatment,y=ymean),size=4,colour="blue",shape=1)+
          geom_line(df_es%>%filter(post==0),mapping=aes(x=time_to_treatment,y=ymean_prepost),size=0.5,linetype="dashed")+
          geom_line(df_es%>%filter(post==1),mapping=aes(x=time_to_treatment,y=ymean_prepost),size=0.5,linetype="dashed")+
          geom_text(df_es%>%ungroup()%>%filter(row_number()==nrow(df_es))%>%mutate(label=paste("Post mean:",round(ymean_prepost,3))),
                    mapping=aes(x=median(df_es$time_to_treatment,na.rm=TRUE),y=ymean_prepost,label=label),nudge_y=+0.25)+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                plot.title = element_text(hjust = 0.5),
                legend.position="none")+labs(y=expression(y[t]-y["t=-1"]))+labs(title="(b) Event study chart")+ scale_color_brewer(palette="Dark2")
        
      
##################################### Chart showing DDs  ##############################################
        # calculate weights for CD
        #
        
        cd<-df
        m<-lm(D~as.factor(t)+as.factor(G),data=cd)
        cd$res<-m$residuals
        # 
        w<-cd%>%filter(G!=1)%>%group_by(t,G)%>%summarise(d=mean(D),res=mean(res),y=mean(y))%>%filter(d==1)%>%
          ungroup()%>%mutate(weight=(1/n())*res/sum((1/n())*res))
        # Table
        cdtab<-w%>%group_by(G)%>%mutate(neg_weight=ifelse(weight<0,weight,0),neg_weight_i=ifelse(weight<0,1,0),weight_i=1)%>%summarise(sum_total=sum(weight),sum_neg=sum(neg_weight),count_total=sum(weight_i),count_neg=sum(neg_weight_i))%>%
          mutate(G=ifelse(G==2,"-Group 2","-Group 3"))%>%select(G,count_total,count_neg,sum_total,sum_neg)%>%mutate(sum_total=round(sum_total,digits=3),sum_neg=round(sum_neg,digits=3))         
        cdtot<-cdtab%>%ungroup()%>%summarise(sum_total=sum(sum_total),sum_neg=sum(sum_neg),count_total=sum(count_total),count_neg=sum(count_neg))%>%
          mutate(G="Total")%>%select(G,count_total,count_neg,sum_total,sum_neg)%>%mutate(sum_total=round(sum_total,digits=3),sum_neg=round(sum_neg,digits=3))
        cdtab<-rbind(cdtot,cdtab)
        cdtabf<-cdtab%>%   knitr::kable("html",align = c("l","c","c","c","c"),col.names = c(" ","Weights",
                                                                                            "Neg. weights","Sum weights", "Sum neg. weights")) %>%
          kable_styling("striped", full_width = F) 
        output$cd <- renderTable( cdtab )
       
        # main chart
        c1<-ggplot(df,aes(x=t,y=y,colour=as.factor(G)))+geom_jitter(alpha=0.1)+
          geom_step(aes(x=t,y=ybar),size=2) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                legend.position="top",legend.text = element_text(size=10, 
                                                                 face="bold"),
                legend.key=element_blank(),legend.title = element_text(size=10,face="bold"),
                plot.title = element_text(hjust = 0.5))+
          labs(colour="Group",title="(a) Outcome value (y) over time (t)  ")+ scale_color_brewer(palette="Dark2")+
          geom_point(data=w,mapping=aes(x=t,y=y,shape=as.factor(round(weight,digits=6))),size=3,color="black")+
          scale_shape(solid = FALSE)+
          labs(colour="Group:",shape="CD cell weights:")
        output$cdout<-   renderText({"Notes: the cell weight indicators are positioned at the mean value of the outcome (y) at the corresponding period (t) for the group (g)." })
       
        ggarrange(c1,c2,nrow=1, common.legend = TRUE, legend="top")
   
    })
    
}
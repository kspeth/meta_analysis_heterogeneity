
library(shiny)
library(tidyverse)
# library(dplyr)
# library(knitr)
# library(ggplot2)
# library(hrbrthemes)
# library(plotly)
# library(heatmaply)
# library(stats)
# library(reshape2)
# library(readxl)
# library(stats)
library(DT)



# Define UI for application that draws a histogram
ui <- navbarPage("Meta-Analysis Heterogeneity Statistic Calculator",
                 tabPanel("Overview",
                          fluidPage(
                              mainPanel(h1("Meta-analysis Heterogeneity Statistic Calculator"),
                                        h5("Developed by Qinghua Song and Kelly Speth, Kite Data and Statistical Sciences"),
                                        h5("In collaboration with Jennifer Sun, Allen Xue"),
                                        h6("Version date: 25-Aug-2021"),
                                        h6("Following Higgins and Thompson (2002), this application estimates heterogeneity statistics associated with met-analyses."),
                                        h6("Simply upload a .csv file with the study ID, Ns, ORs, and/or OR CIs.")#,
                                        # h6("The data include 142 subjects from ZUMA-1 Cohorts 1, 2, and 4 (Phase 2 subjects only), and serum samples collected at Baseline, Day 0, Day 1, Day 3, and Day 5.")
                              )
                          )
                 ),
                 tabPanel("Data Upload and Calculations",
                          fluidPage(
                              mainPanel(tabsetPanel(type = "tabs",
                              #                       tabPanel("Input Values",
                              #                                numericInput("num_studies",
                              #                                            "Number of Studies in Meta-Analysis",
                              #                                            min=5,
                              #                                            max=100,
                              #                                            value = 10,
                              #                                            step = 1),
                              #                                selectInput("stat",
                              #                                            "Statistic of Interest",
                              #                                            choices = c("Odds Ratio"),
                              #                                            selected = "Odds Ratio"),
                              #                                # selectInput("visit",
                              #                                #             "Visit",
                              #                                #             choices = VisitIDs,
                              #                                #             selected = "DAY0"),
                              #                                selectInput("stat_input",
                              #                                            "Data to Input",
                              #                                            choices = c("ORs", "Ns"),
                              #                                            selected = "Ns"),
                              #                                conditionalPanel(
                              #                                    condition = "input.stat_input == 'Ns'",
                              #                                    numericInput("N1",
                              #                                                 "Exposed Cases",
                              #                                                 min=1,
                              #                                                 max=5000,
                              #                                                 value = 100,
                              #                                                 step = 1),
                              #                                    numericInput("N2",
                              #                                                 "Exposed Controls",
                              #                                                 min=1,
                              #                                                 max=5000,
                              #                                                 value = 100,
                              #                                                 step = 1),
                              #                                    numericInput("N3",
                              #                                                 "Not Exposed Cases",
                              #                                                 min=1,
                              #                                                 max=5000,
                              #                                                 value = 100,
                              #                                                 step = 1),
                              #                                    numericInput("N4",
                              #                                                 "Not Exposed Controls",
                              #                                                 min=1,
                              #                                                 max=5000,
                              #                                                 value = 100,
                              #                                                 step = 1))#,
                              #                       # tabPanel("Table with P < Threshold", 
                              #                       #          DTOutput('tbl'),
                              #                       #          downloadButton('downloadData', 'Download')),
                              #                       # tabPanel("Output Values", 
                              #                       #          plotlyOutput("plot"))
                              # ),
                              tabPanel("Data Upload",
                                       selectInput("filetype",
                                                   "File Type",
                                                   choices = c(".csv"),#, ".xlsx"
                                                   selected = ".csv"),
                                       fileInput("file1", "Choose File", accept = c(".csv")),#,".xlsx"
                                       checkboxInput("header", "Header", TRUE)
                                       # selectInput("stat",
                                       #             "Statistic of Interest",
                                       #             choices = c("Odds Ratio"),
                                       #             selected = "Odds Ratio"),
                                       # selectInput("stat_input",
                                       #             "Data to Input",
                                       #             choices = c("ORs", "Ns"),
                                       #             selected = "Ns")
                              ),
                              tabPanel("Data Table with Input Values",
                                       #tableOutput("contents"),
                                       tableOutput("contents2")#,
                                       # selectInput("stat",
                                       #             "Statistic of Interest",
                                       #             choices = c("Odds Ratio"),
                                       #             selected = "Odds Ratio"),
                                       # selectInput("stat_input",
                                       #             "Data to Input",
                                       #             choices = c("ORs", "Ns"),
                                       #             selected = "Ns")
                              ),
                              tabPanel("Calculated Statistics",
                                       tableOutput("output_table")
                              )
                              )
                          )
                 )
                 
                 
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # output$contents <- renderTable({
    #     file <- input$file1
    #     ext <- tools::file_ext(file$datapath)
    #     
    #     req(file)
    #     validate(need(ext == "csv", "Please upload a csv file"))
    #     
    #     read.csv(file$datapath, header = input$header)
    # })
    
    output$contents2 <- renderTable({
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        dat3 <- read.csv(file$datapath, header = input$header)
        if(sum(is.na(dat3$Num_Cases) == 0)){
            dat4 <- dat3 %>% mutate(CasesNoExp = Num_Cases - Exposed_Cases,
                                  ControlsNoExp = Num_Controls - Exposed_Controls)
            dat4 <- dat4 %>% mutate(OR_calc = (Exposed_Cases*ControlsNoExp)/(Exposed_Controls*CasesNoExp))
            dat4 <- dat4 %>% mutate(logOR_calc = log(OR_calc),
                                  logOR_SEapprox = sqrt((1/Exposed_Cases) + (1/Exposed_Controls) + (1/CasesNoExp) + (1/ControlsNoExp)))
            
            return(dat4[,-c(which(colnames(dat2)=="OR"),which(colnames(dat2)=="OR_lower"),which(colnames(dat2)=="OR_upper"))])
        } else(return(dat3))
    })
    
    data <- reactive({
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        dat <- read.csv(file$datapath, header = input$header) 
        #return(dat)
        ## process data
        if(sum(is.na(dat$Num_Cases)) == 0){
            dat <- dat %>% mutate(CasesNoExp = Num_Cases - Exposed_Cases,
                                  ControlsNoExp = Num_Controls - Exposed_Controls)
            dat <- dat %>% mutate(OR_calc = (Exposed_Cases*ControlsNoExp)/(Exposed_Controls*CasesNoExp))
            dat <- dat %>% mutate(logOR_calc = log(OR_calc),
                                    logOR_SEapprox = sqrt((1/Exposed_Cases) + (1/Exposed_Controls) + (1/CasesNoExp) + (1/ControlsNoExp)))
            k <- length(dat$StudyID)
            sub <- dat %>% 
                dplyr::select(StudyID, logOR_calc, logOR_SEapprox) %>% 
                mutate(weight_iv = 1/(logOR_SEapprox^2)) %>% 
                mutate(numerator = weight_iv*logOR_calc)
            
            ## calculate the inverse variance weighted average logOR
            w_mean_logOR <- sum(sub$numerator)/sum(sub$weight_iv)
            
            ## calculate the IV weighted average estimated OR for fixed effect
            FE_estOR <- exp(w_mean_logOR) 
            
            ## Next apply the RE estimation
            
            ###### calculate the Random effects estimated OR
            ## Q represents the total variance relative to the within study variance,
            ## where weight_iv is the weight given to the study, and logOR is the FE estimate,
            ## and w_mean_logOR is the combined estimate from the FE analysis, and k = number of studies
            Q1 <- sum(sub$weight_iv*(sub$logOR_calc - w_mean_logOR)^2)
            #Q1 ## this is the Q value from Higgins et al (2003) Table 2, Heterogeneity test
            df <- k-1
            #df ## this is the df from Higgins et al (2003) Table 2, Heterogeneity test
            ## Q has a chisquare distribution under the null hypothesis of no heterogeneity, 
            ## and df = k-1
            ## And C is a scaling factor to address the fact that Q is a weighted sum of squares,
            ## which ensures that tau_sq is on the same scale as the within-studies variance
            C <- sum(sub$weight_iv) - ((sum(sub$weight_iv^2))/(sum(sub$weight_iv)))
            
            ## is the between-studies variance, representing our estimate of the variance in the
            ## true effect among studies
            ## if Q<= k-1, then tau_sq = 0
            tau_sq <- ifelse(Q1 > (df), ((Q1-(df))/C), 0)
            
            ## Then under the RE model, the variance is newVar and calculated as follows.
            ## Then the new weights are 1/newVar
            sub <- sub %>% 
                mutate(newVar = logOR_SEapprox^2 + tau_sq) %>% 
                mutate(newWeight_iv = 1/newVar)
            
            ##RE combined estimate is similar to that of the FE estimate above
            ## RE_est_logOR is combined RE estimate
            RE_est_logOR <- sum(sub$newWeight_iv*sub$logOR_calc)/sum(sub$newWeight_iv)
            ## RE_var is the variance of the combined estimate
            RE_var <- 1/(sum(sub$newWeight_iv))
            RE_SE <- sqrt(RE_var)
            
            ## The CIs then for the combined RE estimate for the logOR uses the standard normal approximation
            CIs_logOR <- RE_est_logOR + c(-1,1)*qnorm(0.975)*RE_SE
            
            ## Finally, transform to get combined RE estimate of OR, with CIs
            OR_RE_est <- exp(RE_est_logOR)
            OR_lower <- exp(CIs_logOR[1])
            OR_upper <- exp(CIs_logOR[2])
            
            ## Combined RE estimate of OR, with 95% CI
            #print(c(OR_RE_est,OR_lower,OR_upper)) ## These are the estimates reported in Angelillo and Villali (1999)
            ## Table 2, Wiring Config codes, OR estimate Overall (bottom right cell)
            
            ### Now generate H^2 and I^2 with reference intervals
            ## p-value corresponding to Cochran's Q for heterogeneity
            Q_pval <- pchisq(Q1,df,lower.tail=FALSE) ## This is found in Higgins et al (2003) Table 2 for All Studies
            ## p-value
            
            ## H-statistic, referenced in Higgins in Thompson (2002)
            H_sq <- (Q1/(df))
            H <- max(sqrt(H_sq),1)
            #H ## H=1 indicates homogeneity of treatment effects. 
            
            ## calculate reference interval for H per Higgins and Thompson (2002)
            SE_logH <- ifelse(Q1>k, (1/2)*((log(Q1) - log(df))/(sqrt(2*Q1)-sqrt(2*k-3))),
                              sqrt((1/(2*(k-2)))*(1 - (1/(3*(k-2)^2)))) )
            H_CI <- exp(log(H) + c(-1,1)*qnorm(0.975)*SE_logH)
            #print(c(H, CI[1], CI[2]))
            ## this reference interval describes the variability associated with the value of H for studies with precisions identical to those observed in the current meta-analysis
            
            ## I-Statistic - per Higgins and Thompson (2002), can be calculated using either Q and k or H
            I_sq1 <- (Q1-(df))/(Q1)
            I_sq2 <- (H^2-1)/H^2
            #100*I_sq1 ## indicating that nearly 2/3 of the observed variation in the effect estimates among trials is due to real heterogeneity
            ## proportion of total variation in the estimates of treatment effect that is due to heterogeneity between studies
            ## (similar in concept to the intraclass correlation coefficient in cluster sampling)
            
            ### Use reference interval for H to get reference interval for I^2
            I_sq <- function(H,low,hi){
                est <- ((H^2-1)/(H^2))
                lower <- max(0,((low^2-1)/(low^2)))
                higher <- min(1,((hi^2-1)/(hi^2)))
                return(c(est,lower,higher))
            }
            
            ## I^2 estimate and 95% CI
            #100*I_sq(H,CI[1],CI[2]) ## These estimates of I^2 and uncertainty intervals are found in Table 2 of Higgins et al (2003) for All Studies
            data <- data.frame(k = k,
                                FE_estOR = FE_estOR,
                                CochranQ = Q1,
                                df = df,
                                Q_pval = round(Q_pval,digits=3),
                                #tau_sq = tau_sq,
                                OR_RE_est = OR_RE_est,
                                OR_lower = OR_lower,
                                OR_upper = OR_upper,
                                H = H,
                                H_CI_lower = H_CI[1],
                                H_CI_upper = H_CI[2],
                                I_sq_est = 100*I_sq(H,H_CI[1],H_CI[2])[1],
                                I_sq_lower = 100*I_sq(H,H_CI[1],H_CI[2])[2],
                                I_sq_upper = 100*I_sq(H,H_CI[1],H_CI[2])[3]
                                )
            return(data)
        } else if(sum(is.na(dat$OR)) == 0){
            dat <- dat %>% mutate(SELogOR = (logUpper-logOR)/qnorm(0.975),
                                    SELogOR_2 = (-1)*(logLower-logOR)/qnorm(0.975))
            dat <- dat %>% mutate(SELogOR_avg = (SELogOR + SELogOR_2)/2)
            k <- length(dat$StudyID)
            
        }
        
    })
    
    # output$output_table = renderDT({
    #     data()
    # })
    
    output$output_table = renderTable({
        data()
    },digits=3)
    
    # plot1 <- reactive({
    #     ## read in data
    #     name <- paste0(input$outcome,input$visit,".csv")
    #     sav6 <- read.csv(name)
    #     ## 
    #     df2 <- as.data.frame(sav6[,c('lr_pval', "NPX_Diff",'Assay')])
    #     colnames(df2) <- c("P", "EFFECTSIZE", "Assay")
    #     df2$highlight <- ifelse( (df2$P < input$pval) & (abs(df2$EFFECTSIZE) > 1), "Possible DE","Not DE" )
    #     vall <- round(-1*log10(input$pval),digits=3)
    #     p <- ggplot(data = df2,
    #                 aes(x = EFFECTSIZE,
    #                     y = -log10(P),
    #                     colour=highlight,
    #                     label = Assay)) +
    #         geom_point(size = 0.8) + ## alpha=0.4, size=3.5
    #         scale_color_manual(values=c("black", "red")) +
    #         #xlim(c(-4.5, 4.5)) +
    #         geom_vline(xintercept=c(-1,1),lty=4,col="black",lwd=0.5) +
    #         geom_hline(yintercept = vall,lty=4,col="black",lwd=0.5) +
    #         labs(x="NPX(Outcome=Yes) - NPX(Outcome=No)",
    #              y=paste0("-log10(p) - unadjusted for FDR, (-log10(" , input$pval , ") = ", vall,")"),
    #              title=paste0("Univariate Analysis: NPX", input$visit, " ", input$outcome))  +
    #         theme_bw()+
    #         theme(plot.title = element_text(hjust = 0.5),
    #               legend.position="right",
    #               legend.title = element_blank())
    #     return(p)
    #     #return(df2)
    # })
    
    # output$plot <- renderPlotly({
    #     # generate bins based on input$bins from ui.R
    #     ggplotly(plot1())
    #     
    # })
    
    # df <- reactive({
    #     name <- paste0(input$outcome,input$visit,".csv")
    #     sav6 <- read.csv(name)
    #     sav6 <- sav6[,-1]
    #     df <- sav6 %>% filter(lr_pval < input$pval & abs(NPX_Diff) >=1) %>% select(-c(IDs))
    #     df <- df %>% mutate(Missing.Percent = round(Missing.Percent,digits=3),
    #                         lr_pval = round(lr_pval,digits=3),
    #                         wc_pval = round(wc_pval,digits=3),
    #                         median_ratio = round(median_ratio,digits=3),
    #                         LR.BHadjust = round(LR.BHadjust,digits=3),
    #                         WC.BHadjust = round(WC.BHadjust,digits=3),
    #                         MedVal_NoOutcome = round(NO_Tox,digits=3),
    #                         MedVal_YesOutcome = round(YES_Tox,digits=3),
    #                         Med_NPXDiff = round(NPX_Diff,digits=3)) %>% select(-c(NO_Tox, YES_Tox, NPX_Diff))
    #     #df <- round(df, digits=3)
    #     return(df)
    # })
    
    # output$tbl = renderDT({
    #     df()  
    # })
    
    # output$downloadData <- downloadHandler(
    #     filename = function(){paste0("possibleDE_", input$outcome, "_", input$visit,".csv")}, 
    #     content = function(fname){
    #         write.csv(df(), fname)
    #     }
    # )
}
# Run the application 
shinyApp(ui = ui, server = server)

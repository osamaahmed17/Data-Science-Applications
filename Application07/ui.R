#Application 07

library(shiny)

shinyUI(fluidPage(
        titlePanel("Application 07"),
        sidebarLayout(
          sidebarPanel(
            fileInput("files", "Choose File From Application 07 Folder", multiple=TRUE),
            h4("The File is about the IQ of the foster child which was recorded in Foster and the IQ of the biologically-raised child in Biological. The socieoeconomic status of the biological family was also recorded in Social.")
            ),
          mainPanel(
            h2("Hypothesis Testing"),
            tabsetPanel(
              tabPanel("Q01",
                  h3("Question 01:"),
                  em("An outbreak of Salmonella-related illness was attributed to ice cream produced at a certain factory. 
                      Scientists measured the level of Salmonella in 9 randomly sampled batches of ice cream. The levels (in MPN/g) were:
                     0.593 0.142 0.329 0.691 0.231 0.793 0.519 0.392 0.418 Is there evidence that the mean level of Salmonella in the ice cream is greater than 0.3 MPN/g?"),h4("The one-sample t-test is used to determine whether a sample comes from a population with a specific mean. This population mean is not always known, but is sometimes hypothesized.The Data taken from the question was stored in X and Mean along with t value, degree of freedom, p-value is shown."),
                  verbatimTextOutput("q0"),br()),
              
              tabPanel("Q02",
                   h3("Question 02:"),
                   em(""),h4("A hypothesis test that is used to determine questions related to the mean in situations where data is collected from two random data samples. The two sample T-test is often used for evaluating the means of two variables or distinct groups, providing information as to whether the means between the two populations differs.
                             This population mean is not always known, but is sometimes hypothesized.The Data taken from the question was stored in X and Mean along with t value, degree of freedom, p-value is shown."),
                   verbatimTextOutput("q1"),br()),
                   
              tabPanel("Q03",
                    h3("Question 03:"),
                    em("Use the dataset twins.csv provided to you on your course on google classroom.  
                        The data were generated on the topic that if a person's intelligence is primarily influenced by genetics or the environment, 
                        a long standing debatable issue. The study was set up to investigate the IQ scores of twins who were raised either with their biological parents or in foster families. You are required to perform a t-test to test the Null hypothesis. 
                        Use typical significance level. Conclude the results.
                        You are required to perform a t-test to test the Null hypothesis. 
                        Use typical significance level. Conclude the results."),
                    verbatimTextOutput("q2"),br(),
                    actionButton("go", "T-Test"),br(),
                    tableOutput("filetable")),
              
              tabPanel("Q04",
                    h3("Question 04:"),
                    em("A study was performed to test whether cars get better mileage on Hi-octane than on Petrol. 
                        Each of 10 cars was first filled with either petrol or hi-octane, decided by a coin toss, 
                        and the mileage for that tank was recorded. 
                        The mileage was recorded again for the same cars using rest kind of petroleum.
                        Perform a paired t-test to determine whether cars get significantly better mileage with premium gas.
                        H0: ??1-??2=0
                        Ha: ??1-??2>0
                        Regular = 16, 20, 21, 22, 23, 22, 27, 25, 27, 28 
                        Premium = 19, 22, 24, 24, 25, 25, 26, 26, 28, 32"),h4("The paired sample t-test, sometimes called the dependent sample t-test, is a statistical procedure used to determine whether the mean difference between two sets of observations is zero. In a paired sample t-test, each subject or entity is measured twice, resulting in pairs of observations."),
                    verbatimTextOutput("q3"),br()),
              
              tabPanel("Q05",
                    h3("Question 05:"),
                    em("A researcher hypothesizes that electrical stimulation of the lateral habenula will 
                        result in a decrease in food intake (in this case, chocolate chips) in rats. 
                        Rats undergo stereotaxic surgery and an electrode is implanted in the right lateral habenula.
                        Following a ten day recovery period, rats (kept at 80 percent body weight) 
                        are tested for the number of chocolate chips consumed during a 
                        10 minute period of time both with and without electrical stimulation. 
                        The testing conditions are counter balanced. Compute the appropriate t-test for the data provided below.
                        Stimulation 12, 7, 3, 11, 8, 5, 14, 7, 9, 10
                        No Stimulation 8, 7, 4, 14, 6, 7, 12, 5, 5, 8"),h4("In statistics, Welch's t-test, or unequal variances t-test, is a two-sample location test which is used to test the hypothesis that two populations have equal means."),
                     verbatimTextOutput("q4"),br()),
              
              tabPanel("Q06",
                    h3("Question 06"),
                    em("Sleep researchers decide to test the impact of REM sleep deprivation on a computerized assembly line task. 
                        Subjects are required to participate in two nights of testing. 
                        On the nights of testing EEG, EMG, EOG measures are taken. On each night of testing the subject is allowed a total of four hours of sleep. 
                        However, on one of the nights, the subject is awakened immediately upon achieving REM sleep. 
                        On the alternate night, subjects are randomly awakened at various times throughout the 4 hour total sleep session. 
                        Testing conditions are counterbalanced so that half of the subject experience REM deprivation 
                        on the first night of testing and half experience REM deprivation on the second night of testing. 
                        Each subject after the sleep session is required to complete a computerized assembly line task. 
                        The task involves five rows of widgets slowly passing across the computer screen. 
                        Randomly placed on a one/five ratio are widgets missing a component that must be fixed by the subject? 
                        Number of missed widgets is recorded. Compute the appropriate t-test for the data provided below.
                        REM Deprived 26, 15, 8, 44, 26, 13, 38, 24, 17, 29
                        Control Condition 20, 4, 9, 36, 20, 3, 25, 10, 6, 14"),
                    verbatimTextOutput("q5"),br()),
                                
              tabPanel("Q07",
                    h3("Question 07"),
                    em("Researchers want to examine the effect of perceived control on health complaints 
                        of geriatric patients in a long-term care facility. 
                        Thirty patients are randomly selected to participate in the study. 
                        Half are given a plant to care for and half are given a plant but the care is conducted by the staff. 
                        Number of health complaints is recorded for each patient over the following seven days. 
                        Compute the appropriate t-test for the data provided below.
                        Control Over Plant 23 12 6 15 18 5 21 18 34 10 23 14 19 23 8
                        No Control Over Plant 35 21 26 24 17 23 37 22 16 38 23 41 27 24 32"),
                    verbatimTextOutput("q6"),br()),
              
              tabPanel("Q08",
                      h3("Question 08"),
                      em("Dataset: mtcars from R datasets
                         Statistic: two group t-test
                         Claim: 4 & 6 cyl cars have equal mpg
                         Variances: Unequal Variances
                         Level of significance: 5%
                         Reject Hyp: true difference in means is not equal to 0.
                         P-value: 0.0004048"),
                      verbatimTextOutput("q7"),
                      verbatimTextOutput("q8"),br(),br())            
                                
            )
            
          
  ))
))


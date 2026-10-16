# LoanRisk Analytics | IT3081 group project | Expert-informed Version 4
# Run locally: shiny::runApp('dashboard') from repo/ (or runApp('.') here)
# All data shipped with this app are aggregated results; no individual loan records.

if (!requireNamespace('shiny', quietly = TRUE)) {
  stop("Install Shiny first: install.packages('shiny')")
}
library(shiny)

read_local <- function(name) read.csv(file.path('data', name), stringsAsFactors = FALSE, check.names = FALSE)
models <- read_local('model_comparison.csv')
feedback_url <- Sys.getenv('LOANRISK_FEEDBACK_URL', unset = '')
if (nzchar(feedback_url) && !grepl('^https://', feedback_url)) feedback_url <- ''
pca <- read_local('pca_variance.csv')
segments <- list(
  'Region' = read_local('by_region.csv'),
  'Gender / application type' = read_local('by_gender.csv'),
  'Age group' = read_local('by_age.csv'),
  'Loan purpose' = read_local('by_loan_purpose.csv'),
  'Credit type' = read_local('by_credit_type.csv')
)
segment_cols <- c('Region'='Region', 'Gender / application type'='Gender', 'Age group'='age', 'Loan purpose'='loan_purpose', 'Credit type'='credit_type')

pct <- function(x) sprintf('%.1f%%', 100 * x)
num <- function(x) format(x, big.mark=',', scientific=FALSE, trim=TRUE)

ui <- fluidPage(
  tags$head(
    tags$meta(name='viewport', content='width=device-width, initial-scale=1'),
    tags$title('LoanRisk Analytics | Research Dashboard'),
    tags$style(HTML("\n      body { background:#f3f6fb; color:#15243b; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }\n      .container-fluid { max-width:1450px; padding:25px 28px; }\n      .brand { padding:18px 24px; background:#102847; color:#fff; border-radius:14px; margin-bottom:19px; }\n      .brand h2 { margin:0; font-weight:750; letter-spacing:-0.7px; }\n      .brand p { color:#c4d8ed; margin:8px 0 0; font-size:14px; }\n      .kpi { background:white; border:1px solid #e2e8f2; border-radius:12px; padding:20px 22px; margin-bottom:16px; box-shadow:0 2px 7px rgba(20,40,70,.035); min-height:116px; }\n      .kpi .value { font-size:31px; color:#153965; font-weight:750; letter-spacing:-0.8px; }\n      .kpi .label { color:#697a8b; font-size:13px; font-weight:600; text-transform:uppercase; letter-spacing:.7px; }\n      .panel { background:white; border:1px solid #e2e8f2; padding:24px; border-radius:14px; margin-bottom:18px; box-shadow:0 2px 7px rgba(20,40,70,.035); }\n      .panel h3 { font-size:21px; margin-top:0; font-weight:720; }\n      .note { border-left:4px solid #d39a3a; padding:13px 17px; background:#fff9ef; border-radius:4px; margin:14px 0; color:#6e501d; }\n      .subtle { color:#65768b; font-size:13px; line-height:1.55; }\n      .nav-tabs>li>a { color:#395674; font-weight:650; }\n      .nav-tabs>li.active>a { color:#163d6b; }\n      table { font-size:13px; }\n      .table>thead>tr>th { background:#edf3fa; border-color:#e1eaf4; }\n      .shiny-input-container { max-width:460px; }\n      .btn-primary { background:#1a4e82; border-color:#1a4e82; }\n      .panel .table { width:100%; margin-top:12px; }
      .metric-guide { display:grid; grid-template-columns:repeat(4,minmax(160px,1fr)); gap:12px; margin:12px 0; }
      .metric-tile { background:#f3f7fc; border:1px solid #e1e9f4; border-radius:10px; padding:14px; line-height:1.5; }
      .metric-tile strong { display:block; color:#173c64; margin-bottom:4px; }
      .intro-title { font-size:22px; font-weight:750; margin-top:0; color:#102847; }
      .intro-lead { font-size:16px; line-height:1.6; color:#263c56; max-width:1100px; }
      .how-grid { display:grid; grid-template-columns:repeat(3,minmax(200px,1fr)); gap:12px; margin-top:18px; }
      .how-step { background:#f2f6fc; border:1px solid #e0e8f2; border-radius:10px; padding:14px; line-height:1.5; }
      .how-step strong { display:block; color:#173c64; margin-bottom:6px; }
      .insight-panel { border:1px solid #cfddec; padding:15px 18px; background:#f7faff; border-radius:9px; line-height:1.55; margin-top:14px; }
      .mini-label { color:#506684; text-transform:uppercase; letter-spacing:.4px; font-size:12px; font-weight:750; }
      details.explainer { border-top:1px solid #e7edf4; margin-top:12px; padding-top:12px; }
      details.explainer summary { color:#1b517f; cursor:pointer; font-weight:700; }
      details.explainer p { margin-top:9px; line-height:1.5; }
      .score-card { background:#f3f7fc; padding:12px 16px; border-radius:8px; border:1px solid #e0e8f2; }
      @media(max-width:900px) { .how-grid { grid-template-columns:1fr; } }
      .fact { background:#eef5fc; border-left:4px solid #28699c; padding:12px 16px; border-radius:5px; margin:12px 0; }
      .two-col { display:grid; grid-template-columns:1fr 1fr; gap:14px; }
      .legend-note { font-size:13px; color:#536985; }
      @media(max-width:900px) { .metric-guide,.two-col { grid-template-columns:1fr; } }
      @media(max-width:800px) { .container-fluid { padding:13px; } .brand h2 { font-size:24px; } .kpi .value { font-size:25px; } }\n    "))
  ),
  div(class='brand',
      h2('LoanRisk Analytics'),
      p('IT3081 Statistical Modelling  •  Loan default analytics and model evaluation  •  Expert review prototype')
  ),
  div(class='note', strong('Important research limitation: '),
      'The project defines Status 1 as default and Status 0 as non-default. Results come from 2019 loan records and the models show limited discrimination and many false alarms. This is an educational prototype, not a tool for approving or rejecting loans.'),
  tabsetPanel(id='tab',
    tabPanel('Overview', br(),
      div(class='panel',
        h3(class='intro-title','What does LoanRisk Analytics do?'),
        p(class='intro-lead', 'This student research dashboard explores patterns in loan records, tests whether groups differ, and compares six models predicting the project-defined default outcome. It helps explain the evidence and its limitations; it does not approve, reject, or score real borrowers.'),
        div(class='how-grid',
          div(class='how-step',strong('1 | Explore the records'),
              'See how the default rate varies by region, age group, or application type.'),
          div(class='how-step',strong('2 | Compare the models'),
              'See which method identifies more defaulted loans and how often it incorrectly flags non-defaulted loans.'),
          div(class='how-step',strong('3 | Understand limitations'),
              'Review missing values, prediction errors, and what a bank would need before operational use.')
        ),
        tags$details(class='explainer',
          tags$summary('How is loan default defined in this project?'),
          p(strong('Status 1 = Default; Status 0 = Non-default. '),
            'These are the target labels used throughout the project analyses. Before using such predictions operationally, a bank would need to verify the source definition and track actual repayment outcomes over time.')
        )
      ),
      fluidRow(
        column(3, div(class='kpi',div(class='label','Loan observations'),div(class='value',num(148670)))),
        column(3, div(class='kpi',div(class='label','Loan default rate'),div(class='value','24.64%'))),
        column(3, div(class='kpi',div(class='label','Top validation F1 model'),div(class='value','Naive Bayes'))),
        column(3, div(class='kpi',div(class='label','Top model test AUC'),div(class='value','0.6208')))
      ),
      div(class='panel',h3('Explore loan default rates by group'),
        selectInput('segment','Group observations by:', choices=names(segments), selected='Region'),
        plotOutput('segmentPlot',height='350px'),
        p(class='subtle','Percentages use existing aggregated results from Person B’s descriptive analysis. Categories are shown as recorded and are not necessarily demographic labels.'),
        tableOutput('segmentTable')
      ),
      div(class='panel',h3('What this means for a bank'),
        div(class='insight-panel',
          strong('Research insight, not a lending recommendation. '),
          'The dashboard can highlight data-quality issues and demonstrate how different models behave. It cannot yet be used to decide whether an applicant should receive a loan. The outcome definition, historical payment records, and external validation are still needed.'
        )
      ),
      div(class='panel',h3('Project at a glance'),
        p('Descriptive analysis, hypothesis tests, five logistic regression variants, Naive Bayes, principal component analysis, and simulated CRD/RCBD experiments.'),
        p(class='subtle','Project data has one recorded calendar year (2019), so genuine loan-default time-series forecasting was not possible.')
      )
    ),
    tabPanel('Model Comparison',br(),
      div(class='panel',h3('Six-model held-out evaluation'),
        selectInput('metric','Compare models by:',
          choices=c('Balance of correct flags (F1 score)'='F1',
                    'Ability to distinguish groups (ROC-AUC)'='ROC_AUC',
                    'How often positive flags are right (Precision)'='Precision',
                    'How many defaults we catch (Recall)'='Recall',
                    'How many non-defaults we clear (Specificity)'='Specificity',
                    'Overall correct classifications (Accuracy)'='Accuracy'),
          selected='F1'),
        plotOutput('modelPlot',height='380px'),
        tableOutput('modelTable'),
        div(class='metric-guide',
          div(class='metric-tile',strong('F1 score'),'One number summarizing the trade-off between detecting defaults and avoiding false alarms. Higher is better.'),
          div(class='metric-tile',strong('ROC-AUC'),'How well a model ranks defaulted loans above non-defaulted loans. Around 0.50 is similar to random ordering.'),
          div(class='metric-tile',strong('Recall'),'Of every 100 defaulted loans, roughly how many were flagged?'),
          div(class='metric-tile',strong('Precision'),'Of every 100 loans flagged as defaults, how many actually have the default label?')
        ),
        div(class='insight-panel',
          div(class='mini-label','Key finding explained simply'),
          p(strong('Naive Bayes was the highest-ranked candidate in this research comparison.'),
            ' In the held-out test set, it correctly flagged 5,715 of 7,329 defaulted loans, but also wrongly flagged 13,044 non-defaulted loans.'),
          p('Its ROC-AUC was 0.6208. This measures ranking ability; it is not 62% accuracy, and the output is not a validated individual default probability.'),
          p(class='subtle','Selecting the best of these six experimental models is not evidence that any of them is ready for banking operations.')
        ),
        p(class='subtle','All models use the same 60/20/20 stratified partitions. Thresholds were chosen on validation F1. The test figures remain exploratory because earlier examination of the dataset influenced model development.')
      ),
      div(class='panel',h3('Why might this matter to decision-makers?'),
        p('A model that detects many defaults can still create a large workload by incorrectly flagging non-defaulted loans. Financial institutions would need an explicit review process, accurate labels, fair outcomes, and a quantified cost for each kind of error.'),
        tags$details(class='explainer',
          tags$summary('Explain the four confusion-matrix counts'),
          p('Correctly flagged default: the model predicts default and the record is defaulted. False alarm: the model predicts default but the loan is non-defaulted. Correctly cleared loan: the model predicts non-default and the record is non-defaulted. Missed default: the model predicts non-default but the record is defaulted.')
        )
      ),
      div(class='panel',h3('Inspect one model'),
        selectInput('chosenModel','Model:',choices=models$Model, selected='Naive Bayes'),
        uiOutput('modelDetails'),
        plotOutput('confusionPlot',height='280px')
      )
    ),
    tabPanel('Statistical Findings',br(),
      div(class='panel',h3('In simple terms: what did the statistical tests show?'),
        p('Loan records differ in income, loan purpose, and the spread of loan amounts. With a large dataset, small differences can be statistically significant. These results describe associations—not the causes of repayment outcomes.'),
        div(class='how-grid',
          div(class='how-step',strong('Income'),'Non-defaulted loans had higher average reported income than defaulted loans, but the difference was small relative to overall variation.'),
          div(class='how-step',strong('Loan purpose'),'Average requested loan amounts differed across recorded purposes.'),
          div(class='how-step',strong('Data quality'),'Missing financial values vary strongly between defaulted and non-defaulted loans, affecting which records a model can learn from.')
        )
      ),
      div(class='panel',h3('Task 4: Statistical inference'),
        tags$table(class='table table-striped',
          tags$thead(tags$tr(tags$th('Test'),tags$th('Finding'),tags$th('Interpretation'))),
          tags$tbody(
            tags$tr(tags$td('Welch t-test'),tags$td('Income differs by default outcome; p < 2.2e-16'),tags$td('Small effect size (Cohen’s d ≈ 0.15)')),
            tags$tr(tags$td('Categorical association'),tags$td('Gender/application grouping vs default; p < 2.2e-16'),tags$td('Weak association (Cramér’s V ≈ 0.084)')),
            tags$tr(tags$td('Welch ANOVA'),tags$td('Loan amount varies by loan purpose; p < 2.2e-16'),tags$td('Group differences do not prove causal effects')),
            tags$tr(tags$td('Variance comparison'),tags$td('Loan-amount variances differ by default outcome; p < 2.2e-16'),tags$td('Variance ratio (non-default / default) ≈ 0.703'))
          )
        )
      ),
      div(class='panel', h3('Why statistical significance is not enough'),
        fluidRow(
          column(6, div(class='kpi',div(class='label','Mean income, non-default'),div(class='value','7,204.01'))),
          column(6, div(class='kpi',div(class='label','Mean income, default'),div(class='value','6,231.81')))
        ),
        div(class='fact',strong('Interpretation: '),'The income difference is statistically significant, but Cohen’s d ≈ 0.15 is small. Large datasets can detect modest effects. None of these comparisons establishes causation.'),
        p(class='subtle','All amounts use the original dataset units; a specific currency should not be assumed.')
      ),
      div(class='panel',h3('Data quality and sensitivity'),
        tags$ul(
          tags$li('Complete-case analysis changed the default rate from 24.64% to approximately 16.33%.'),
          tags$li('LTV and DTI missingness differed sharply between defaulted and non-defaulted loans.'),
          tags$li('Five unusually high LTV records were traced to property values of 8,000.'),
          tags$li('Capping LTV at the training 99.9th percentile improved test F1 slightly but did not improve ROC-AUC.')
        ),
        div(class='note','Statistical significance does not establish causality. Missingness may be informative and may bias model evaluation.')
      )
    ),
    tabPanel('PCA & Experiments',br(),
      div(class='panel',h3('PCA: reducing the number of measurements'),
        p('PCA creates combined indicators from several numerical features. It can simplify a dataset, but capturing most of the variation in loan features does not guarantee better default predictions.'),
        div(class='fact',strong('Key idea: '),' Five of seven components cover 91.42% of numerical-feature variation. That is not a prediction accuracy score.')
      ),
      div(class='panel',h3('Task 7: Principal Component Analysis'),
        plotOutput('pcaPlot',height='350px'),
        plotOutput('pcaCumulativePlot',height='300px'),
        tableOutput('pcaTable'),
        div(class='fact',strong('PCA in plain language: '),'PCA combines correlated numeric measurements into new summary dimensions. Five of seven components account for 91.42% of observed variation; this does not mean they explain 91.42% of loan defaults or increase model accuracy.'),
        p(class='subtle','PCA used standardized numerical features on complete observations; complete-case selection may change the analyzed population.')
      ),
      div(class='panel',h3('Task 6: CRD and RCBD — SIMULATED'),
        fluidRow(
          column(6, h4('CRD'), tags$p('Random assignment of 90 hypothetical borrowers to three reminder treatments.'),tags$p('Treatment ANOVA: F = 6.50, p = 0.00234.')),
          column(6, h4('RCBD'), tags$p('Thirty matched blocks; treatments randomized within blocks.'),tags$p('Treatment ANOVA: F = 19.424, p = 3.49e-07.'))
        ),
        plotOutput('experimentPlot',height='350px'),
        div(class='note','These are simulated experiment outcomes, NOT observations from the loan dataset or evidence that reminder strategies work in a real bank. CRD and RCBD were generated as distinct experiments; differences between their residual variances are not a controlled efficiency estimate.')
      )
    ),
    tabPanel('Expert Review',br(),
      div(class='panel',h3('What we learned from an industry expert'),
        p('One senior-level banking officer with more than 20 years of experience reviewed the earlier dashboard and provided preliminary feedback. The respondent permitted anonymous quotation.'),
        div(class='two-col',
          div(class='score-card', div(class='mini-label','Relevance for exploring loan-risk patterns'),h3('2 / 5')),
          div(class='score-card', div(class='mini-label','Clarity of charts and explanations'),h3('2 / 5'))
        ),
        tags$blockquote('Make it more friendly version and briefly explain what is all about . People can’t understand technical jargons .'),
        p(strong('Expert concern at the time of review: '),'The outcome variable needed a clear, documented definition. The project uses Status 1 = default and Status 0 = non-default; source-level documentation is still recommended before operational use.'),
        p(strong('Additional data requested: '),'Historical repayment records and employment / income history.'),
        p(class='subtle','Feedback from one respondent is informative, not statistically representative. These ratings describe the earlier version, not an independent assessment of this update.')
      ),
      div(class='panel',h3('What changed after the feedback'),
        tags$ul(
          tags$li(strong('Clearer purpose:'),' A new overview explains the dashboard in everyday language.'),
          tags$li(strong('Easier metrics:'),' Model performance includes practical questions and examples, not just abbreviations.'),
          tags$li(strong('Practical meaning:'),' The model comparison explains false flags and why high recall alone is not enough.'),
          tags$li(strong('Transparency:'),' Default and non-default are defined clearly; no personal loan scores or approval advice are issued.')
        ),
        div(class='note','Still needed for banking use: original-source documentation of the outcome, historical repayment records, probability calibration, fair-lending checks, and independent validation.'),
        p(class='subtle','A repeat review is needed before claiming that the revised interface received better expert ratings.')
      ),
      div(class='panel',h3('Purpose of the expert review'),
        p('We seek feedback on practical usefulness, realistic inputs, interpretability, responsible communication, and the information required for a valid credit-risk tool.'),
        p('This dashboard presents exploratory analyses and model evaluation. It does not issue individual loan decisions or live risk predictions because the models have limited predictive performance and have not been validated for operational use.'),
        h4('Questions for the industry expert'),
        tags$ol(
          tags$li('Is our operational definition (Status 1 = default; Status 0 = non-default) sufficiently documented for banking use?'),
          tags$li('Are the five modelling predictors appropriate and available before a lending decision?'),
          tags$li('Which costs matter most: missed defaults, false flags, or manual-review workload?'),
          tags$li('Are the charts and results understandable for banking decision-makers?'),
          tags$li('What additional data (including repayment histories) would most improve usefulness?'),
          tags$li('What model-validation, compliance, fairness and security checks would be required?'),
          tags$li('What are your top three recommended improvements?')
        ),
        downloadButton('downloadQuestions','Download expert-review questionnaire',class='btn-primary'),
        uiOutput('feedbackLink'),
        p(class='subtle','The questionnaire can be shared with an expert. Ask permission before storing their name or feedback. Responses are not collected or stored by this dashboard.')
      ),
      div(class='panel',h3('Review process'),
        tags$ol(
          tags$li('Ask a real finance, banking, or credit-risk expert to review this dashboard.'),
          tags$li('Share the downloadable questions; avoid providing borrower-level personal data.'),
          tags$li('Collect the expert’s actual comments and consent before quoting them.'),
          tags$li('Record the expert’s role, review date, suggested changes, and your team’s response.')
        ),
        div(class='fact','This dashboard does not itself store feedback. To enable an external Google Form, set the LOANRISK_FEEDBACK_URL environment variable to your actual HTTPS form link; never substitute a made-up URL.')
      ),
      div(class='panel',h3('Methodological disclosures'),
        tags$ul(
          tags$li('Source: Kaggle Loan Default dataset, 148,670 observations in 2019.'),
          tags$li('Project target coding: Status 1 = default; Status 0 = non-default. Original-source documentation is recommended before banking deployment.'),
          tags$li('Naive Bayes test F1 = 0.4381, AUC = 0.6208: limited discrimination and many false positives.'),
          tags$li('No longitudinal histories in this dataset; time-series improvement is hypothetical.'),
          tags$li('Not a real-time operational bank decision system.')
        )
      )
    )
  ),
  tags$hr(),
  p(class='subtle','LoanRisk Analytics • Academic research prototype • Aggregate project outputs only • No personal applicant-level data published')
)

server <- function(input, output, session) {
  selected_segment <- reactive(segments[[input$segment]])
  output$segmentPlot <- renderPlot({
    d <- selected_segment(); category <- segment_cols[[input$segment]]
    d <- d[order(d$Default_Rate), , drop=FALSE]
    old <- par(mar=c(5, 13, 2, 2)); on.exit(par(old))
    cols <- rep('#3975b8',nrow(d))
    barplot(d$Default_Rate, names.arg=d[[category]], horiz=TRUE, las=1, col=cols, border=NA,
            xlab='Loan default rate (%)', xlim=c(0,max(100,max(d$Default_Rate,na.rm=TRUE)+10)), cex.names=.9)
    abline(v=24.64,col='#d88a35',lty=2,lwd=2)
    legend('bottomright', legend='Overall default rate: 24.64%', lty=2,col='#d88a35',bty='n',cex=.85)
  })
  output$segmentTable <- renderTable({
    d <- selected_segment(); d[order(-d$Total), , drop=FALSE]
  }, striped=TRUE, digits=2)
  output$modelPlot <- renderPlot({
    v <- models[[input$metric]]
    ord <- order(v, decreasing=FALSE)
    vals <- v[ord]
    labels <- models$Model[ord]
    highlight <- ifelse(labels == 'Naive Bayes','#176a96','#92b4d4')
    old <- par(mar=c(4,15,2,4)); on.exit(par(old))
    xmax <- max(0.68, max(vals,na.rm=TRUE)*1.20)
    b <- barplot(vals, names.arg=labels, horiz=TRUE, las=1, col=highlight, border=NA, xlim=c(0,xmax),
                 cex.names=1.0, xlab=paste('Test',gsub('_',' ',input$metric)))
    text(vals + xmax*.018, b, labels=sprintf('%.4f',vals), pos=4, cex=.88, xpd=NA)
    if (input$metric == 'ROC_AUC') abline(v=.5,lty=2,col='#cb8940',lwd=2)
  })
  output$modelTable <- renderTable({
    d <- models[, c('Model','Threshold','Validation_F1','Accuracy','Precision','Recall','F1','ROC_AUC')]
    for (j in 2:ncol(d)) d[[j]] <- sprintf('%.4f',d[[j]])
    d
  }, striped=TRUE, width='100%', align=c('l',rep('r',7)))
  chosen <- reactive(models[models$Model == input$chosenModel, , drop=FALSE])
  output$modelDetails <- renderUI({
    d <- chosen()
    tags$p(strong(paste0('Validation-selected threshold: ',sprintf('%.2f',d$Threshold))),
           paste0(' • Test F1: ',sprintf('%.4f',d$F1),
                  ' • Test AUC: ',sprintf('%.4f',d$ROC_AUC)))
  })
  output$confusionPlot <- renderPlot({
    d <- chosen(); counts <- c('Detected defaults'=d$TP,'False alarms'=d$FP,'Correct non-defaults'=d$TN,'Missed defaults'=d$FN)
    old <- par(mar=c(7,5,2,1)); on.exit(par(old))
    b <- barplot(counts,col=c('#237e71','#d18b45','#4677ae','#b95865'),border=NA,las=2,ylab='Test cases',ylim=c(0,max(counts)*1.18))
    text(b,counts+max(counts)*.035,labels=num(counts),cex=.9)
  })
  output$pcaPlot <- renderPlot({
    old <- par(mar=c(5,5,2,4)); on.exit(par(old))
    b <- barplot(pca$Explained_Percent,names.arg=pca$PC,col='#4f85bc',border=NA,ylab='Explained variance (%)',ylim=c(0,40))
    text(b,pca$Explained_Percent+1,labels=paste0(round(pca$Explained_Percent,1),'%'),cex=.9)
  })
  output$pcaCumulativePlot <- renderPlot({
    old <- par(mar=c(5,5,2,2)); on.exit(par(old))
    plot(seq_len(nrow(pca)),pca$Cumulative_Percent,type='b',pch=19,col='#1c709d',lwd=3,
      axes=FALSE, xlab='Principal component count',ylab='Cumulative explained variance (%)', ylim=c(0,105))
    axis(1,at=seq_len(nrow(pca)),labels=pca$PC)
    axis(2,at=seq(0,100,20),las=1)
    box(bty='l')
    abline(h=80,lty=2,col='#d18d37')
    text(5,94, '5 PCs: 91.42%', col='#14507a',pos=4,cex=.9)
  })
  output$experimentPlot <- renderPlot({
    # These treatment means come from the clearly labelled simulated Task 6 outputs.
    crd <- c('Standard SMS'=70.99808, 'Personalized SMS'=75.85663, 'SMS + call'=80.92858)
    rcbd <- c('Standard SMS'=72.43002, 'Personalized SMS'=77.05814, 'SMS + call'=83.18877)
    vals <- rbind(CRD=crd, RCBD=rcbd)
    old <- par(mar=c(5,5,2,2)); on.exit(par(old))
    b <- barplot(vals,beside=TRUE,col=c('#407fb6','#d6a05c'),ylim=c(0,100),
      names.arg=names(crd),ylab='Simulated mean on-time payment (%)',cex.names=.88)
    text(b,vals+2,labels=sprintf('%.1f',vals),cex=.8)
    legend('topleft',legend=c('CRD (simulated)','RCBD (simulated)'),fill=c('#407fb6','#d6a05c'),bty='n')
  })
  output$pcaTable <- renderTable({
    pca[,c('PC','Explained_Percent','Cumulative_Percent')]
  }, striped=TRUE)
  output$feedbackLink <- renderUI({
    if (!nzchar(feedback_url)) {
      p(class='subtle','No online submission form is configured yet; use the downloaded questionnaire.')
    } else {
      tags$p(tags$a('Open expert feedback form',href=feedback_url,target='_blank',rel='noopener noreferrer',
        class='btn btn-primary',style='margin-top:12px'))
    }
  })
  output$downloadQuestions <- downloadHandler(
    filename=function() 'loanrisk_expert_review_questions.txt',
    content=function(file) {
      lines <- c(
        'LoanRisk Analytics — Industry Expert Review',
        'Academic prototype. Participation is voluntary. Do not provide sensitive borrower data.', '',
        'Expert role (optional): __________________',
        'Date: __________________', '',
        '1. Is the project target definition (Status 1 = default, Status 0 = non-default) supported by suitable documentation?',
        '2. Are loan amount, income, LTV, DTI and credit score adequate, timely predictors?',
        '3. How should false positives and missed defaults be weighed?',
        '4. Are the charts and explanations useful and clear?',
        '5. What historical repayment or economic data would help?',
        '6. What security, fairness, calibration and external validation is required?',
        '7. What are your top three improvements?', '',
        'Overall assessment: __________________',
        'Consent to quote anonymized feedback in academic presentation? Yes / No',
        'Notes: __________________'
      )
      writeLines(lines,file)
    }
  )
}

shinyApp(ui, server)


# This file is a generated template, your changes will not be overwritten

BrawSimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawSimClass",
  inherit = BrawSimBase,
  private = list(
    .init = function() {
    },
    .run = function() {
      # debug information
      # self$results$debug$setVisible(TRUE)
      # self$results$debug$setContent(c(self$options$showExploreBtn,is.null(dataStore$exploreResult)))

      # initialization code
      if (!exists("braw.env")) {
        BrawOpts(fontScale = 1.35)
        statusStore<-list(lastOutput="System",
                          showSampleType="Sample",
                          showInferParam="Basic",
                          showMultipleParam="Basic",
                          showExploreParam="r"
        )
        braw.env$statusStore<-statusStore
      }
      statusStore<-braw.env$statusStore
      
      # get some flags for later
      makeSampleNow<-self$options$makeSampleBtn
      showSampleType<-self$options$showSampleType
      showInferParam<-self$options$showInferParam
      if (showInferParam=="2D") {
        dimensionInfer<-"2D"
        showInferParam<-paste0(self$options$singleVar1,";",self$options$singleVar2)
      } else dimensionInfer<-"1D"
      
      makeMultipleNow<-self$options$makeMultipleBtn
      showMultipleParam<-self$options$showMultipleParam
      if (showMultipleParam=="2D") {
        dimensionMultiple<-"2D"
        showMultipleParam<-paste0(self$options$multipleVar1,";",self$options$multipleVar2)
      } else dimensionMultiple<-"1D"
      whichShowMultipleOut<-self$options$whichShowMultiple

      makeExploreNow<-self$options$makeExploreBtn
      typeExplore<-self$options$typeExplore
      showExploreParam<-self$options$showExploreParam
      whichShowExploreOut<-self$options$whichShowExplore
      
      outputNow<-statusStore$lastOutput
      if (self$options$showHypothesisBtn) outputNow<-"System"

      if (showExploreParam != statusStore$showExploreParam) outputNow<-"Explore"
      if (showMultipleParam != statusStore$showMultipleParam) outputNow<-"Multiple"
      if (showInferParam != statusStore$showInferParam) outputNow<-"Infer"
      if (showSampleType != statusStore$showSampleType) outputNow<-showSampleType

      # make all the standard things we need
      locals<-list(hypothesis=NULL,design=NULL,evidence=NULL,
                   sample=NULL,analysis=NULL,explore=NULL)
      
      DV<-makeVariable(self$options$DVname,self$options$DVtype,
                       mu=self$options$DVmu,sd=self$options$DVsd,skew=self$options$DVskew,kurtosis=self$options$DVkurt,
                       ncats=self$options$DVncats,proportions=self$options$DVprops,
                       nlevs=self$options$DVnlevs,iqr=self$options$DViqr)
      IV<-makeVariable(self$options$IVname,self$options$IVtype,
                       mu=self$options$IVmu,sd=self$options$IVsd,skew=self$options$IVskew,kurtosis=self$options$IVkurt,
                       ncats=self$options$IVncats,proportions=self$options$IVprops,
                       nlevs=self$options$IVnlevs,iqr=self$options$IViqr)
      if (self$options$IV2on) {
        IV2<-makeVariable(self$options$IV2name,self$options$IV2type,
                          mu=self$options$IV2mu,sd=self$options$IV2sd,skew=self$options$IV2skew,kurtosis=self$options$IV2kurt,
                          ncats=self$options$IV2ncats,proportions=self$options$IV2props,
                          nlevs=self$options$IV2nlevs,iqr=self$options$IV2iqr)
      } else {
        IV2<-NULL
      }
      
      effect<-makeEffect(rIV=self$options$EffectSize1,
                         rIV2=self$options$EffectSize2,
                         rIVIV2<-self$options$EffectSize3,
                         rIVIV2DV<-self$options$EffectSize12,
                         Heteroscedasticity=self$options$Heteroscedasticity,
                         ResidDistr=self$options$Residuals,
                         world=makeWorld(worldOn=self$options$WorldOn,
                                         populationPDF=self$options$WorldPDF,
                                         populationRZ = self$options$WorldRZ,
                                         populationPDFk = self$options$Worldk,
                                         populationNullp = self$options$WorldNullP
                         )
      )
      
      oldH<-braw.def$hypothesis
      hypothesis<-makeHypothesis(IV,IV2,DV,effect)
      changedH<- !identical(oldH,hypothesis)

      oldD<-braw.def$design
      design<-makeDesign(sN=self$options$SampleSize,
                         sNRand=self$options$SampleSpread=="yes",sNRandK=self$options$SampleGamma,
                         sMethod=makeSampling(self$options$SampleMethod),
                                sIV1Use=self$options$SampleUsage1,
                                sIV2Use=self$options$SampleUsage2,
                                sDependence=self$options$Dependence,
                                sOutliers=self$options$Outliers,
                                sCheating=self$options$Cheating,sCheatingAttempts=self$options$CheatingAttempts,
                         Replication=makeReplication(On=self$options$ReplicationOn,
                                                     Power=self$options$ReplicationPower,
                                                     Repeats=self$options$ReplicationAttempts,
                                                     Keep=self$options$ReplicationDecision,
                                                     RepAlpha=self$options$ReplicationAlpha,
                                                     PowerPrior=self$options$ReplicationPrior
                                                     )
                         )
      changedD<- !identical(oldD,design)
      
      oldE<-braw.def$evidence
      evidence<-makeEvidence(Welch=self$options$Welch=="yes",
                                    Transform=self$options$Transform
                                    )
      changedE<- !identical(oldE,evidence)
      braw.env$alphaSig<<-self$options$alphaSig
      
      braw.def$hypothesis<<-hypothesis
      braw.def$design<<-design
      braw.def$evidence<<-evidence
      
      if (changedH || changedD) {
        braw.res$result<<-NULL
        braw.res$expected<<-NULL
        braw.res$explore<<-NULL
        outputNow<-"System"
      }
      if (changedE) {
        braw.res$result<<-doAnalysis(sample=braw.res$result)
        braw.res$expected<<-NULL
        braw.res$explore<<-NULL
        outputNow<-showSampleType
      }
      # did we ask for a new sample?
      if (makeSampleNow) {
        # make a sample
        result<-doResult()
        outputNow<-showSampleType
      }
      
      # did we ask for new multiples?
      if (makeMultipleNow) {
        numberSamples<-self$options$numberSamples
        expectedResult<-doExpected(nsims=numberSamples,expectedResult=braw.res$expected,
                                     doingNull=self$options$multipleDoingNull=="yes")
        outputNow<-"Multiple"
      }
      
      # did we ask for new explore?
      if (makeExploreNow) {
        if ( !is.null(braw.res$explore) &&
            (self$options$typeExplore!=braw.res$explore$explore$exploreType ||
            self$options$exploreNPoints!=braw.res$explore$explore$exploreNPoints ||
            self$options$exploreMaxN!=braw.res$explore$explore$max_n ||
            self$options$exploreXLog!=braw.res$explore$explore$xlog) 
            ){
          braw.res$explore<<-NULL
        }

        numberExplores<-self$options$numberExplores
        exploreResult<-doExplore(nsims=numberExplores,exploreResult=braw.res$explore,
                                 exploreType=typeExplore,
                                 exploreNPoints=self$options$exploreNPoints,
                                 max_n=self$options$exploreMaxN,
                                 xlog=self$options$exploreXLog,
                                 doingNull=self$options$exploreDoingNull=="yes")
        outputNow<-"Explore"
      }
      
      # what are we showing?
      statusStore$lastOutput<-outputNow
      
      
      # end of actions      
      statusStore$showSampleType<-self$options$showSampleType
      statusStore$showInferParam<-self$options$showInferParam
      statusStore$showMultipleParam<-self$options$showMultipleParam
      statusStore$showExploreParam<-self$options$showExploreParam
      # save everything for the next round      
      braw.env$statusStore<-statusStore
      
      # main results graphs/reports
      if (!is.null(outputNow))     
        switch(outputNow,
               "Sample"={
                 self$results$graphPlot$setState(outputNow)
                 self$results$reportPlot$setState(outputNow)
               },
               "Describe"={
                 self$results$graphPlot$setState(outputNow)
                 self$results$reportPlot$setState(outputNow)
               },
               "Infer"={
                 self$results$graphPlot$setState(c(outputNow,showInferParam,dimensionInfer))
                 self$results$reportPlot$setState(c(outputNow,showInferParam))
               },
               "Multiple"={
                 self$results$graphPlot$setState(c(outputNow,showMultipleParam,dimensionMultiple,whichShowMultipleOut))
                 self$results$reportPlot$setState(c(outputNow,showMultipleParam))
               },
               "Explore"={
                 self$results$graphPlot$setState(c(outputNow,showExploreParam,whichShowExploreOut))
                 self$results$reportPlot$setState(c(outputNow,showExploreParam))
               },
               {
                 self$results$graphPlot$setState(outputNow)
               }
        )
    },
    
    .plotGraph=function(image, ...) {
      outputGraph <- image$state[1]
      if (!is.null(outputGraph)) {
        switch(outputGraph,
               "System"    =outputGraph<-showSystem(),
               "Hypothesis"=outputGraph<-showHypothesis(),
               "Design"    =outputGraph<-showDesign(),
               "Population"=outputGraph<-showPopulation(),
               "Prediction"=outputGraph<-showPrediction(),
               "Sample"    =outputGraph<-showSample(),
               "Describe"  =outputGraph<-showDescription(),
               "Infer"     =outputGraph<-showInference(showType=image$state[2],dimension=image$state[3]),
               "Multiple"  =outputGraph<-showExpected(showType=image$state[2],dimension=image$state[3],effectType=image$state[4]),
               "Explore"   =outputGraph<-showExplore(showType=image$state[2],effectType=image$state[3])
        )
        print(outputGraph)
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    
    .plotReport=function(image, ...) {
      outputGraph <- image$state[1]
      if (!is.null(outputGraph)) {
        switch(outputGraph,
               "Sample"    =outputGraph<-reportSample(),
               "Describe"  =outputGraph<-reportDescription(),
               "Infer"     =outputGraph<-reportInference(),
               "Multiple"  =outputGraph<-reportExpected(showType=image$state[2]),
               "Explore"   =outputGraph<-reportExplore(showType=image$state[2])
        )
        print(outputGraph)
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )
)

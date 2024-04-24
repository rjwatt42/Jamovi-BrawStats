
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
      val1<-runif(1)
      val2<-runif(1)
      
      # initialization code
      if (!exists("braw.env")) {
        BrawOpts(fontScale = 1.35)
        statusStore<-list(lastOutput="Hypothesis",
                          showHypothesis="Hypothesis",
                          showSample="Sample",
                          showInfer="Basic",
                          showMultiple="Basic",
                          showExplore="r"
        )
        braw.env$statusStore<-statusStore
      }
      
      statusStore<-braw.env$statusStore
      # val1<-length(braw.res$expected$result$rIV)
      
      # get some flags for later
      showHypothesisType<-self$options$showHypothesis
      
      makeSampleNow<-self$options$makeSampleBtn
      showSampleType<-self$options$showSample
      showInfer<-self$options$showInfer
      if (showInfer=="2D") {
        dimensionInfer<-"2D"
        showInfer<-"Basic"
      } else dimensionInfer<-"1D"
      
      makeMultipleNow<-self$options$makeMultipleBtn
      showMultipleOut<-self$options$showMultiple
      if (showMultipleOut=="2D") {
        dimensionMultiple<-"2D"
        showMultipleOut<-"Basic"
      } else dimensionMultiple<-"1D"
      whichShowMultipleOut<-self$options$whichShowMultiple

      makeExploreNow<-self$options$makeExploreBtn
      typeExplore<-self$options$typeExplore
      showExploreOut<-self$options$showExplore
      whichShowExploreOut<-self$options$whichShowExplore
      
      outputNow<-statusStore$lastOutput
      if (self$options$showHypothesisBtn) outputNow<-"Hypothesis"
      if (self$options$showSampleBtn) outputNow<-"Sample"
      if (self$options$showMultipleBtn) outputNow<-"Multiple"
      if (self$options$showExploreBtn) outputNow<-"Explore"

      if (self$options$showExplore != statusStore$showExplore) outputNow<-"Explore"
      if (self$options$showMultiple != statusStore$showMultiple) outputNow<-"Multiple"
      if (self$options$showInfer != statusStore$showInfer) outputNow<-"Sample"
      if (self$options$showSample != statusStore$showSample) outputNow<-"Sample"
      if (self$options$showHypothesis != statusStore$showHypothesis) outputNow<-"Hypothesis"

      if (self$options$showSampleBtn) makeSampleNow<-TRUE
      if (self$options$showMultipleBtn) makeMultipleNow<-TRUE
      if (self$options$showExploreBtn) makeExploreNow<-TRUE
      
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
                         world=makeWorld(worldOn=self$options$WorldOn,
                                         populationPDF=self$options$WorldPDF,
                                         populationRZ = self$options$WorldRZ,
                                         populationPDFk = self$options$Worldk,
                                         populationNullp = self$options$WorldNullP
                         )
      )
      
      oldH<-braw.def$hypothesis
      hypothesis<-makeHypothesis(IV,IV2,DV,effect)
      changed<- !identical(oldH,hypothesis)
      
      oldD<-braw.def$design
      design<-makeDesign(sN=self$options$SampleSize,
                                sMethod=makeSampling(self$options$SampleMethod),
                                sIV1Use=self$options$SampleUsage1,
                                sIV2Use=self$options$SampleUsage2,
                                sDependence=self$options$Dependence,
                                sOutliers=self$options$Outliers,
                                sCheating=self$options$Cheating,sCheatingAttempts=self$options$CheatingAttempts)
      changed<- changed || !identical(oldD,design)
      
      oldE<-braw.def$evidence
      evidence<-makeEvidence(Welch=self$options$Welch=="yes",
                                    Transform=self$options$Transform,
                                    shortHand=self$options$shorthand=="yes"
                                    )
      changed<- changed || !identical(oldE,evidence)
      
      braw.def$hypothesis<<-hypothesis
      braw.def$design<<-design
      braw.def$evidence<<-evidence
      # outputNow<-"Hypothesis"
      if (changed) {
        braw.res$result<<-NULL
        braw.res$expected<<-NULL
        braw.res$explore<<-NULL
      }
      madeSample<-"no"
      # did we ask for a new sample?
      if (makeSampleNow) {
        # make a sample
        result<-doResult()
        outputNow<-"Sample"
        madeSample<-"yes"
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
        numberExplores<-self$options$numberExplores
        exploreResult<-doExplore(nsims=numberExplores,exploreResult=braw.res$explore,exploreType=typeExplore,
                                          exploreNPoints=self$options$exploreNPoints,
                                          doingNull=self$options$exploreDoingNull=="yes")
        outputNow<-"Explore"
      }
      
      # are we showing the sample?
      # outputText<-NULL
      outputGraph<-NULL
      if (!is.null(outputNow)) {
        switch(outputNow,
               "Hypothesis"=outputGraph<-showHypothesisType,
               "Sample"=outputGraph<-showSampleType,
               "Multiple"=outputGraph<-"Multiple",
               "Explore"=outputGraph<-"Explore"
        )
      }
      statusStore$lastOutput<-outputNow
      
      
      # end of actions      
      statusStore$showHypothesis<-self$options$showHypothesis
      statusStore$showSample<-self$options$showSample
      statusStore$showInfer<-self$options$showInfer
      statusStore$showMultiple<-self$options$showMultiple
      statusStore$showExplore<-self$options$showExplore
      # save everything for the next round      
      # self$results$debug$setState(statusStore)
      braw.env$statusStore<-statusStore
      
      self$results$debug$setContent("no")
      self$results$debug$setVisible(TRUE)
      # main results graphs/reports
      # if (!is.null(outputText))      self$results$reportPlot$setState(outputText)
      if (!is.null(outputGraph))     
        switch(outputGraph,
               "Infer"=self$results$graphPlot$setState(c(outputGraph,showInfer,dimensionInfer)),
               "Multiple"=self$results$graphPlot$setState(c(outputGraph,showMultipleOut,dimensionMultiple,whichShowMultipleOut)),
               "Explore"=self$results$graphPlot$setState(c(outputGraph,showExploreOut,whichShowExploreOut)),
               {
                 self$results$graphPlot$setState(c(outputGraph,runif(1)))
                 }
        )
    },
    
    .plotGraph=function(image, ...) {
      self$results$debug$setContent("maybe")
      self$results$debug$setVisible(TRUE)
      outputGraph <- image$state[1]
      if (!is.null(outputGraph)) {
        switch(outputGraph,
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
        self$results$debug$setContent("yes")
        self$results$debug$setVisible(TRUE)
        print(outputGraph)
        return(TRUE)
      } else {
        FALSE
      }
    }
  )
)

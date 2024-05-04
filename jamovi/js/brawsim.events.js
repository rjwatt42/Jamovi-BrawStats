module.exports =  {

    // in here is where the event functions go

    onChange_typeExplore: function(ui) {
      let value = ui.typeExplore.value();
      switch (value) {
        case "n":
          ui.exploreMinVal.setValue(10);
          ui.exploreMaxVal.setValue(250);
          ui.exploreXLog.setValue(false);
          break;
        case "rIV":
        case "rIV2":
        case "rIVIV2":
        case "rIVIV2DV":
          ui.exploreMinVal.setValue(-0.9);
          ui.exploreMaxVal.setValue(0.9);
          ui.exploreXLog.setValue(false);
          break;
        case "IVskew":
        case "DVskew":
        case "Heteroscedasticity":
        case "Dependence":
        case "Outliers":
        case "IVRange":
        case "DVRange":
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(1);
          ui.exploreXLog.setValue(false);
          break;
        case "IVkurtosis":
        case "DVkurtosis":
          ui.exploreMinVal.setValue(1.5);
          ui.exploreMaxVal.setValue(100000);
          ui.exploreXLog.setValue(true);
          break;
        case "IVprop":
        case "DVprop":
          ui.exploreMinVal.setValue(0.2);
          ui.exploreMaxVal.setValue(0.8);
          ui.exploreXLog.setValue(false);
          break;
        case "IVcats":
        case "DVcats":
          ui.exploreMinVal.setValue(2);
          ui.exploreMaxVal.setValue(6);
          ui.exploreXLog.setValue(false);
          break;
        case "IVlevels":
        case "DVlevels":
          ui.exploreMinVal.setValue(3);
          ui.exploreMaxVal.setValue(10);
          ui.exploreXLog.setValue(false);
          break;
        case "WithinCorr":
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(1);
          ui.exploreXLog.setValue(false);
          break;
        case "Alpha":
          ui.exploreMinVal.setValue(0.001);
          ui.exploreMaxVal.setValue(0.5);
          ui.exploreXLog.setValue(true);
          break;
        case "Power":
          ui.exploreMinVal.setValue(0.1);
          ui.exploreMaxVal.setValue(0.9);
          ui.exploreXLog.setValue(false);
          break;
        case "Repeats":
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(8);
          ui.exploreXLog.setValue(false);
          break;
        case "pNull":
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(1);
          ui.exploreXLog.setValue(false);
          break;
        case "lambda":
          ui.exploreMinVal.setValue(0.1);
          ui.exploreMaxVal.setValue(1);
          ui.exploreXLog.setValue(false);
          break;
        case "CheatingAmount":
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(0.8);
          ui.exploreXLog.setValue(false);
          break;
        case "ClusterRad":
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(1);
          ui.exploreXLog.setValue(false);
          break;
        case "SampleGamma":
          ui.exploreMinVal.setValue(1);
          ui.exploreMaxVal.setValue(10);
          ui.exploreXLog.setValue(false);
          break;
        case "IVType":
        case "DVType":
        case "PDF":
        case "Method":
        case "Usage":
        case "Cheating":
        case "Transform":
        case "Welch":
        case "Keep":
          ui.exploreMinVal.setValue("");
          ui.exploreMaxVal.setValue("");
          ui.exploreXLog.setValue(false);
          break;
        default: 
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(1);
          ui.exploreXLog.setValue(false);
          break;
      }
      switch (value) {
        case "IVType":
          ui.exploreNPoints.setValue("5");
          break;
        case "DVType":
          ui.exploreNPoints.setValue("5");
          break;
        case "PDF":
          ui.exploreNPoints.setValue("7");
          break;
        case "Method":
          ui.exploreNPoints.setValue("5");
          break;
        case "Usage":
          ui.exploreNPoints.setValue("2");
          break;
        case "Cheating":
          ui.exploreNPoints.setValue("6");
          break;
        case "Transform":
          ui.exploreNPoints.setValue("3");
          break;
        case "Keep":
          ui.exploreNPoints.setValue("5");
          break;
        case "Welch":
          ui.exploreNPoints.setValue("2");
          break;
        default: 
          ui.exploreNPoints.setValue("13");
          break;
      }
    },

    onChange_presetHypothesis: function(ui) {
      let presetH = ui.presetHypothesis.value();
      switch(presetH) {
        case "psych":
          ui.WorldOn.setValue(true);
          ui.WorldPDF.setValue("Exp");
          ui.WorldRZ.setValue("z");
          ui.WorldLambda.setValue(0.3);
          ui.WorldNullP.setValue(0.75);
          break;
        case "simple":
          ui.WorldOn.setValue(false);
          ui.rIV.setValue(0.3);
          break;
      }
    },
    onChange_presetDesign: function(ui) {
      let presetD = ui.presetDesign.value();
      switch(presetD) {
        case "psych":
          ui.SampleSize.setValue(52);
          ui.SampleSpread.setValue("yes");
          ui.SampleGamma.setValue(1.56);
          break;
        case "simple":
          ui.SampleSize.setValue(42);
          ui.SampleSpread.setValue("no");
          break;
      }
    }
};

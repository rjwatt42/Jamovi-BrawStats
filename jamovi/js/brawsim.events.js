module.exports =  {

    // in here is where the event functions go

// "rIV","rIV2","rIVIV2","rIVIV2DV"
//                 "n"
//                 "WithinCorr"
//                "IVskew","DVskew","CheatingAmount" ,"Heteroscedasticity","Dependence","Outliers","IVRange","DVRange" ,"IVprop", "DVprop" 
//                "Alpha"
//                "IVkurtosis","DVkurtosis" 
//                "Power","Repeats" 

//                 "pNull","k" 

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
        case "k":
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
        case "Keep":
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(0);
          ui.exploreXLog.setValue(false);
          break;
        default: 
          ui.exploreMinVal.setValue(0);
          ui.exploreMaxVal.setValue(1);
          ui.exploreXLog.setValue(false);
          break;
      }

    }
};

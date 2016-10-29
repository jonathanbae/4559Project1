#Code similar schools to the same id: 
for(i in 1:nrow(date)){
  if(!is.na(date[i,c("undergra")])){
    temp <- date[i,c("undergra")]
    if(temp == "Brown University"){
      #(Brown, Brown University)
      temp <- "Brown"
    }else if(temp == "ColumbiaU" || temp == "Columbia University" || 
             temp == "Columbia College" || temp == "Columbia Business School"||
             temp == "Columbia College, CU"){
      #(Columbia, ColumbiaU, Columbia University, Columbia College, Columbia Business School, "Columbia College, CU")
      temp <- "Columbia"
      date[i,c("mn_sat")] <- 1430
      date[i,c("tuition")] <- 26908
    }else if(temp=="Conneticut College"){
      #(Connecticut College, Conneticut College)
      temp <- "Connecticut College"
    }else if(temp=="Cornell University"){
      #(Cornell, Cornell University)
      temp <- "Cornell"
    }else if(temp=="Ecole Polytechnique (France)"){
      #(ecole polytechnique, Ecole Polytechnique (France) )
      temp <- "ecole polytechnique"
    }else if(temp=="Fudan University, Shanghai, China"){
      #(Fudan, Fudan University, Shanghai, China)
      temp <- "Fudan"
    }else if(temp=="Georgetown University"){
      #(Georgetown, Georgetown University)
      temp <- "Georgetown"
    }else if(temp=="Harvard University" || temp=="Harvard College"){
      temp <- "Harvard"
    }else if(temp=="Holy Cross College"){
      #(Holy Cross, Holy Corss College)
      temp <- "Holy Cross"
    }else if(temp=="Loyola College in Maryland"){
      #(Loyola, Loyola College in Maryland)
      temp <- "Loyola"
    }else if(temp=="nyu"){
      #(New York University, nyu)
      temp <- "New York University"
    }else if(temp=="oberlin"){
      #(Oberlin College, Oberlin)
      temp <- "Oberlin College"
    }else if(temp=="Oxford"){
      #(Oxford University, Oxford)
      temp <- "Oxford University"
    }else if(temp=="Princeton U." || temp=="Princeton University"){
      #(Princeton, Princeton U., Princeton University)
      temp <- "Princeton"
    }else if(temp=="Rice University"){
      #(Rice, Rice University)
      temp <- "Rice"
      date[i,c("mn_sat")] <- 1410
    }else if(temp=="Rutgers College" || temp=="Rutgers University - New Brunswick"){
      temp <- "Rutgers University"
    }else if(temp=="Stanford University"){
      temp <- "Stanford"
    }else if(temp=="UC, IRVINE!!!!!!!!!"){
      temp <- "UC Irvine"
    }else if(temp=="University of California at Santa Cruz"){
      temp <- "UC Santa Cruz"
    }else if(temp=="U of Michigan" || temp=="University of Michigan-Ann Arbor"){
      temp <- "University of Michigan"
    }else if(temp=="umass" || temp=="University of Massachusetts-Amherst"){
      temp <- "University of Massachusetts-Amherst"
      date[i,c("mn_sat")] <- 1050
    }else if(temp=="U.C. Berkeley" || temp=="Cal Berkeley"){
      temp <- "UC Berkeley"
    }else if(temp=="California State University Los Angeles"){
      temp <- "UCLA"
    }else if(temp=="University of California at Santa Barbara"){
      temp <- "UCSB"
    }else if(temp=="UPenn" || temp == "Univ of Pennsylvania"){
      temp <- "University of Pennsylvania"
    }else if(temp=="University of Wisconsin-Madison"){
      temp <- "UW Madison"
    }else if(temp=="Washington U. in St. Louis" || 
             temp=="washington university in st louis"){
      temp <- "Washington University in St. Louis"
    }else if(temp=="Yale University"){
      temp <- "Yale"
    }
    date[i,c("undergra")] <- temp
  }
}
date$undergra <- factor(date$undergra)
date$undergra

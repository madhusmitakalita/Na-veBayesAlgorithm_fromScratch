#Reading the files
TR=read.csv("Training.csv")
TE=read.csv("Testing.csv")

# converting last column to positive & negavtive
AssignPositiveNegaive=function(df)
{
  c1=nrow(df)
  for(i in 1:c1)
  {
    if(df$IsActiveMember[i]==0)
    {
      df$IsActiveMember[i]='negative'
    }
    else
    {
      df$IsActiveMember[i]='positive'
    }
  }
  return(df)
}

TR=AssignPositiveNegaive(TR)
TE=AssignPositiveNegaive(TE)



a=nrow(TR[TR$IsActiveMember== "positive",])
a1=nrow(TR[TR$IsActiveMember== "negative",])
n=nrow(TR[])

# probability of positive and negative 
P_yes=a/n
P_No=a1/n

# function for feature wise probaility using m-estimate
probabilityMEstimateYes=function(i,f,TE,TR)
{
  
  g=TE[i,f]
  print(g)
  if(f==1)
  {
    n=nrow(TR[TR$Geography== g,])
    Nc=nrow(TR[TR$Geography== g & TR$IsActiveMember== "positive",])
    m=3  #3 class values
  }
  else if (f==2)
  {
    n=nrow(TR[TR$Gender== g,])
    Nc=nrow(TR[TR$Gender== g & TR$IsActiveMember== "positive",])
    m=2 #2 class values
  }
  else
  {
    n=nrow(TR[TR$HasCrCard== g,])
    Nc=nrow(TR[TR$HasCrCard== g & TR$IsActiveMember== "positive",])
    m=2 #2 class values
  }
  #print(n)
  #print(Nc)
  
  #assume uniform prior = 1/(number of possible values of the feature)
  P=1/m
  
  # m-estiamte probability
  PFinal=(Nc+(m*P))/(n+m)
  return(PFinal)
}


probabilityMEstimateNo=function(i,f,TE,TR)
{
  
  g=TE[i,f]
  print(g)
  if(f==1)
  {
    n=nrow(TR[TR$Geography== g,])
    Nc=nrow(TR[TR$Geography== g & TR$IsActiveMember== "negative",])
    m=3
  }
  else if (f==2)
  {
    n=nrow(TR[TR$Gender== g,])
    Nc=nrow(TR[TR$Gender== g & TR$IsActiveMember== "negative",])
    m=2
  }
  else
  {
    n=nrow(TR[TR$HasCrCard== g,])
    Nc=nrow(TR[TR$HasCrCard== g & TR$IsActiveMember== "negative",])
    m=2
  }
  #print(n)
  #print(Nc)
  
  #assume uniform prior = 1/(number of possible values of the feature)
  
  P=1/m 
  PFinal=(Nc+(m*P))/(n+m)
  return(PFinal)
}

e=nrow(TE[])
TE1=TE

for(i in 1:e)
{
  PP1=probabilityMEstimateYes(i,1,TE,TR)
  PN1=probabilityMEstimateNo(i,1,TE,TR)
  PP2=probabilityMEstimateYes(i,2,TE,TR)
  PN2=probabilityMEstimateNo(i,2,TE,TR)
  PP3=probabilityMEstimateYes(i,3,TE,TR)
  PN3=probabilityMEstimateNo(i,3,TE,TR)
  Pyes=P_yes*PP1*PP2*PP3
  Pno=P_No*PN1*PN2*PN3
  if(Pyes > Pno)
  {
    TE1$Prediction[i]="positive"
  }
  else
  {
    TE1$Prediction[i]="negative"
  }
}

###saving the prediction file


write.csv(TE1,"predictions.csv")


TP=0
TN=0
FP=0
FN=0

# for calculating the TP,TN,FP,FN values  
for (i in 1:e) {
  if (TE1$Prediction[i]==TE1$IsActiveMember[i])
  {
    if(TE1$Prediction[i]=="positive")
    {
      TP=TP+1
    }
    else
    {
      TN=TN+1
    }
  }
  else
  {
    if(TE1$Prediction[i]=="positive")
    {  
      FP=FP+1
    }
    else
    {
      FN=FN+1
    }
  }
}
Accuracy=(TP+TN)/e
AccuracyP=Accuracy*100

sensitivity=TP/(TP+FN)
sensitivityp=sensitivity*100


specificity=TN/(TN+FP)
specificityp=TN/(TN+FP)*100

print(paste0("Accuracy is: ",Accuracy))
print(paste0("Accuracy in percentage is: ",AccuracyP,"%"))
print(paste0("sensitivity is: ",sensitivity))
print(paste0(" sensitivity in percentage is: ",sensitivityp,"%"))
print(paste0("specificity is: ",specificity))
print(paste0("specificity in percentage is: ",specificityp,"%"))








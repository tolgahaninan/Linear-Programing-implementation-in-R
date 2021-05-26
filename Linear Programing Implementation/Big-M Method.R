#Defining problem
#problemType="max";  
#X1_obj=3; X2_obj=9;   
#X1_1=1; X2_1=4; firstSign="<="; XB_1=8;
#X1_2=1; X2_2=2; secondSign="<="; XB_2=4;

problemType="min";  
X1_obj=1; X2_obj=1;   
X1_1=2; X2_1=4; firstSign=">="; XB_1=4;
X1_2=1; X2_2=7; secondSign=">="; XB_2=7;


#Checking For Sign Of equations
if(firstSign== ">="){
  M_1=1; 
  S1_obj= 0;  A1_obj= M_1; 
  S1_1 = -1 ; S2_1=0;
  A1_1 = 1 ; A2_1=0;
}else if(firstSign == "="){
  M_1=1; 
  S1_obj= 0;  A1_obj= M_1;
  S1_1 = 0 ; S2_1=0;
  A1_1 = 1 ; A2_1=0;
}else if(firstSign == "<="){
  M_1=0; 
  S1_obj= 0;  A1_obj= M_1; 
  S1_1 = 1 ; S2_1=0;
  A1_1 = 0 ; A2_1=0;
}



if(secondSign == ">="){
  M_2=1;
  S2_obj= 0;
  A2_obj= M_2;
  S1_2 = 0 ; S2_2=-1;
  A1_2 = 0 ; A2_2=1;
}else if(secondSign == "="){
  M_2=1;
  S2_obj= 0;
  A2_obj= M_2;
  S1_2 = 0 ; S2_2=0;
  A1_2 = 0 ; A2_2=1;
}else if(secondSign == "<="){
  M_2=0;
  S2_obj= 0;
  A2_obj= M_2;
  S1_2 = 0 ; S2_2=1;
  A1_2 = 0 ; A2_2=0;
}
#Calculating ZJ AND ZJ_CJ Variables

CB_1=M_1; CB_2=M_2;

ZJ_1=(X1_1 * CB_1) + (X1_2 * CB_2);
ZJ_2=(X2_1 * CB_1) + (X2_2 * CB_2);
ZJ_3=(S1_1 * CB_1) + (S1_2 * CB_2);
ZJ_4=(S2_1 * CB_1) + (S2_2 * CB_2);
ZJ_5=(A1_1 * CB_1) + (A1_2 * CB_2);
ZJ_6=(A2_1 * CB_1) + (A2_2 * CB_2);

ZJ_CJ1 = (ZJ_1)- X1_obj;
ZJ_CJ2 = (ZJ_2)- X2_obj;
ZJ_CJ3 = (ZJ_3)- S1_obj;
ZJ_CJ4 = (ZJ_4)- S2_obj;
ZJ_CJ5 = (ZJ_5)- A1_obj;
ZJ_CJ6 = (ZJ_6)- A2_obj;

B <- matrix(c("CJ",X1_obj,X2_obj,S1_obj,S2_obj,M_1,M_1,
              "XB","X1","X2","S1","S2","A1","A2",
              XB_1,X1_1,X2_1,S1_1,S2_1,A1_1,A2_1,
              XB_2,X1_2,X2_2,S1_2,S2_2,A1_2,A2_2,
              "ZJ",ZJ_1,ZJ_2,ZJ_3,ZJ_4,ZJ_5,ZJ_6,
              "ZJ-CJ",ZJ_CJ1,ZJ_CJ2,ZJ_CJ3,ZJ_CJ4,ZJ_CJ5,ZJ_CJ6
)
,ncol =7,byrow = T)

#Assigning them to the matrix

C <- matrix(c("Entering Variable","Leaving Variable Row"),ncol=2)
A <- matrix(c("iteration","pivot","Entering"),ncol=3)

IterationCount=0;
while(1){
  IterationCount = IterationCount +1;
  
  #Checking problem Type Max Or Min
  #If problem type is Max finds max zj-cj value
  if(problemType=="max"){
    i=7 ;
    max =0 ;
    keyColumnIndex=2;
    while (i >= 2) {
      
      if(as.double(B[6,i])<max){
        max=as.double(B[6,i]);
        keyColumnIndex=i;
        
      }
      i=i-1;
      
    }
    #If problem type is Min finds min zj-cj value
  }else if(problemType=="min"){
    i=2 ;
    max =0 ;
    keyColumnIndex=2;
    while (i <= 7) {
      
      if(as.double(B[6,i])>max){
        max=as.double(B[6,i]);
        keyColumnIndex=i;
        
      }
      i=i+1;
      
    }
    
  }
  
  #Calculating min ratio
  minRatio1 = (as.double(B[3,1])/as.double(B[3,keyColumnIndex]))
  minRatio2 = (as.double(B[4,1])/as.double(B[4,keyColumnIndex]))
  
  keyRowIndex=0;
  
  #According to the min ratio finding key row index
  if(minRatio1>minRatio2){
    keyRowIndex = 4;
  }else if (minRatio1<minRatio2){
    if(minRatio1<0){
      
      keyRowIndex = 4; 
    }else{
      keyRowIndex = 3;
    }
    
  }else if (minRatio1==minRatio2){
    if((as.double(B[3,keyColumnIndex])) < (as.double(B[4,keyColumnIndex]))){
      
      keyRowIndex = 3;
    }else{
      keyRowIndex = 4;
    }
    
  }
  
  #Finding Pivot and Entering Variable
  Pivot = as.double(B[keyRowIndex,keyColumnIndex]);
  EnterinVariable=B[2,keyColumnIndex];
  
  #Saving entering variable into the table
  C <- rbind(C, c(EnterinVariable,keyRowIndex))
  
  print(B)
  
  
  A <- rbind(A, c(IterationCount,Pivot,EnterinVariable));
  print(A)
  print("***********************************************************************")
  
  
  
  
  #According to the key row index change matrix
  if(keyRowIndex==3){
    
    
    i=1
    while(i<=6){
      #Finding values
      B[keyRowIndex,i]=as.double(B[keyRowIndex,i])/Pivot;
      i=i+1
    }
    
    
    oldValue=as.double(B[keyRowIndex+1,keyColumnIndex])
    i=1
    while(i<=7){
      #Finding values
      B[keyRowIndex+1,i]=as.double(B[keyRowIndex+1,i])-(oldValue)*(as.double(B[keyRowIndex,i]));
      i=i+1
    }
    
    
    
    
    
    CB_1=as.double(B[1,keyColumnIndex])
    CB_2
    
    
    i=2
    while(i<=7){
      B[5,i]=(CB_1*as.double(B[3,i])) + (CB_2*as.double(B[4,i]))
      i=i+1
    }
    
    
    
    
    
    i=2
    while(i<=7){
      B[6,i]=as.double(B[5,i])-as.double(B[1,i])
      i=i+1
    }
    
    print(B)
    print("***********************************************************************")
    
    #Checking for finish condition. Checks all the ZJ-CJ Values
    #According to the problem type "Max" or "Min" checks if all the ZJ-CJ Values bigger or smaller than 0
    #If all the ZJ-CJ values bigger or smaller it breaks the loop
    if(problemType == "max"){
      if((as.double(B[6,2]) >= 0) && (as.double(B[6,3]) >= 0) && (as.double(B[6,4]) >= 0) && (as.double(B[6,5]) >= 0) && (as.double(B[6,6]) >= 0) && (as.double(B[6,7]) >= 0)){
        break;
      }
    }else if(problemType == "min"){
      if((as.double(B[6,2]) <= 0) && (as.double(B[6,3]) <= 0) && (as.double(B[6,4]) <= 0) && (as.double(B[6,5]) <= 0) && (as.double(B[6,6]) <= 0) && (as.double(B[6,7]) <= 0)){
        break;
      }
    }
    
    
    #According to the key row index change matrix
  }else if(keyRowIndex==4){
    

    i=1
    while(i<=6){
      #Finding values
      B[keyRowIndex,i]=as.double(B[keyRowIndex,i])/Pivot;
      i=i+1
    }
    #Finding values
    oldValue=as.double(B[keyRowIndex-1,keyColumnIndex])
    i=1
    while(i<=7){
      B[keyRowIndex-1,i]=as.double(B[keyRowIndex-1,i])-(oldValue)*(as.double(B[keyRowIndex,i]));
      i=i+1
    }
    CB_2=as.double(B[1,keyColumnIndex])
    CB_1
    
    i=2
    while(i<=7){
      B[5,i]=(CB_1*as.double(B[3,i])) + (CB_2*as.double(B[4,i]))
      i=i+1
    }
    
    
    
    
    i=2
    while(i<=7){
      B[6,i]=as.double(B[5,i])-as.double(B[1,i])
      i=i+1
    }
    
    print(B)
    print("***********************************************************************")
    
    #Checking for finish condition. Checks all the ZJ-CJ Values
    #According to the problem type "Max" or "Min" checks if all the ZJ-CJ Values bigger or smaller than 0
    #If all the ZJ-CJ values bigger or smaller it breaks the loop
    
    if(problemType == "max"){
      if((as.double(B[6,2]) >= 0) && (as.double(B[6,3]) >= 0) && (as.double(B[6,4]) >= 0) && (as.double(B[6,5]) >= 0) && (as.double(B[6,6]) >= 0) && (as.double(B[6,7]) >= 0)){
        break;
      }
    }else if(problemType == "min"){
      if((as.double(B[6,2]) <= 0) && (as.double(B[6,3]) <= 0) && (as.double(B[6,4]) <= 0) && (as.double(B[6,5]) <= 0) && (as.double(B[6,6]) <= 0) && (as.double(B[6,7]) <= 0)){
        break;
      }
    }
  }
  
}
j=1;
#For finding appended values
#Check the last form of matrix if they contains X1 OR X2 Values
D <- matrix(c("Appended X Value","Appended rows of X Values"),ncol=2)
while(j<=nrow(C)){
  
  if(C[j,1]=="X1" || C[j,1]=="X2"){
    D <- rbind(D, c(C[j,1],C[j,j]));
    
  }
  j=j+1;
}
k=1;
# If X1 or X2 is in the last table 
# Gets their values for finding final cost
while(k<=nrow(D)){
  if(D[k,1]=="X1"){
    X1_finalRowIndex=as.double(D[k,k]);
    
  } else {
    X1_finalRowIndex="NULL";
  }
  
  
  if(D[k,1]=="X2"){
    X2_finalRowIndex=as.double(D[k,k]);
    
  } else {
    X2_finalRowIndex="NULL";
  }
  
  k=k+1;
}

if(X1_finalRowIndex=="NULL"){
  ValueOfX1 = 0;
  
}else{
  ValueOfX1 = as.double(B[X1_finalRowIndex,1]); 
  
  
}
if(X2_finalRowIndex=="NULL"){
  ValueOfX2 = 0;
  
}else{
  ValueOfX2 = as.double(B[X2_finalRowIndex,1]);

}

#Finding Results
ObjectiveFunctionResult = (X1_obj* ValueOfX1) + (X2_obj*ValueOfX2)












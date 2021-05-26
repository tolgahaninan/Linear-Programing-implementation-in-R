
#EXAMPLE 1
#this is the question which we found on the internet
A<-matrix(nrow=5,ncol=5)
A[1,]<-c(9,11,14,11,7)
A[2,]<-c(6,15,13,13,10)
A[3,]<-c(12,13,6,8,8)
A[4,]<-c(11,9,10,12,9)
A[5,]<-c(7,12,14,10,14)


#EXAMPLE 2
#This is the question which its given in the class
A<-matrix(nrow=5,ncol=5)
A[1,]<-c(10,5,13,15,16)
A[2,]<-c(3,9,18,13,6)
A[3,]<-c(10,7,2,2,2)
A[4,]<-c(7,11,9,7,12)
A[5,]<-c(7,9,10,4,12)





firstTable=A;


x=1;
while(1){
  zerosCellRow <-c("n","n","n","n","n","n","n","n","n","n","n");
  zerosCellColumn <-c("n","n","n","n","n","n","n","n","n","n","n");
  intersectionLength2=1;  
  
  
  
  
  B=A;
  
  
  
  for (i in 1:5) {
    smaller=10000;
    for (j in 1:5) {
      if(as.integer(A[i,j])<smaller){                 #Rowdaki en küçük sayýyý bulup tüm Row'dan çýkarma iþlemi
        smaller=as.integer(A[i,j]);
      }
    }
    for(j in 1:5){
      A[i,j]=as.integer(A[i,j])-smaller;
    }
  }
  
  
  for (i in 1:5) {
    smaller=10000;
    for (j in 1:5) {
      if(as.integer(A[j,i])<smaller){
        smaller=as.integer(A[j,i]);                    #Columndaki en küçük sayýyý bulup tüm column'dan çýkarma iþlemi
      }
    }
    for(j in 1:5){
      A[j,i]=as.integer(A[j,i])-smaller;
    }
  }
  
  
  B=A;
  C=A;
  howManyZeros=0;
  
  for(i in 1:5){
    j=0;
    
    while(j<5){
      j=j+1;
      
      if(A[i,j]==0){
        
        row=i;
        column=j;
        zeroCounter=1;
        #Row dan yola çýkýp COLUMN çiziyor
        while(j<5){
          j=j+1;
          if(A[i,j]==0){
            zeroCounter=zeroCounter+1;
            
          }
        }
        if(zeroCounter==1){
          howManyZeros=howManyZeros+1;
          zerosCellRow[x]=row;
          zerosCellColumn[x]=column;
          x=x+1;
          for(j in 1:5){
            A[j,column]="-";
          }
          
        }
        
        
      }
    }
  }
  
  
  
  
  intersectionRow <-c("n","n","n","n","n","n","n","n","n","n","n");
  intersectionColumn <-c("n","n","n","n","n","n","n","n","n","n","n");
  intersectionLength=1;
  for(j in 1:5){
    i=0
    while(i<5){
      i=i+1;
      if(A[i,j]==0){
        row=i;
        column=j;
        zeroCounter=1;
        #Column dan yola çýkýp Row çiziyor
        while(i<5){
          i=i+1;
          if(A[i,j]==0){
            zeroCounter=zeroCounter+1;
          }
        }
        if(zeroCounter==1){
          zerosCellRow[x]=row;
          zerosCellColumn[x]=column;
          x=x+1;
          howManyZeros=howManyZeros+1;
          for(i in 1:5){
            if(A[row,i]=="-"){
              intersectionRow[intersectionLength]=row;
              intersectionColumn[intersectionLength]=i;
              intersectionLength=intersectionLength+1;
            }
            A[row,i]="-";
          }
        }
      }
      
    }
    
  }
  
  
  for(i in 1:5){
    for(j in 1:5){                              #EÐER HALA ÇÝZÝLMEMÝÞ 0 VAR ÝSE
      if(A[i,j]=="-"){
        
      }else{
        if(A[i,j]==0){
          for(i in 1:5){
            j=0;
            
            while(j<5){
              j=j+1;
              
              if(A[i,j]==0){
                row=i;
                column=j;
                zeroCounter=1;
                #Row dan yola çýkýp COLUMN çiziyor
                while(j<5){
                  j=j+1;
                  if(A[i,j]==0){
                    zeroCounter=zeroCounter+1;
                    
                  }
                }
                if(zeroCounter==1){
                  howManyZeros=howManyZeros+1;
                  zerosCellRow[x]=row;
                  zerosCellColumn[x]=column;
                  x=x+1;
                  for(j in 1:5){
                    A[j,column]="-";
                  }
                }
              }
            }
          }
          for(j in 1:5){
            i=0
            while(i<5){
              i=i+1;
              if(A[i,j]==0){
                row=i;
                column=j;
                zeroCounter=1;
                #Column dan yola çýkýp Row çiziyor
                while(i<5){
                  i=i+1;
                  if(A[i,j]==0){
                    zeroCounter=zeroCounter+1;
                  }
                }
                if(zeroCounter==1){
                  zerosCellRow[x]=row;
                  zerosCellColumn[x]=column;
                  x=x+1;
                  howManyZeros=howManyZeros+1;
                  for(i in 1:5){
                    if(A[row,i]=="-"){
                      intersectionRow[intersectionLength]=row;
                      intersectionColumn[intersectionLength]=i;
                      intersectionLength=intersectionLength+1;
                    }
                    A[row,i]="-";
                  }
                }
              }
              
            }
            
          }
        }
      }
      
    }
    
    
  }
  
  
  
  
  if(howManyZeros==5){
    break;
  }
  
  
  minimum=10000;
  for(i in 1:5){
    for(j in 1:5){
      
      if(A[i,j]=="-"){                       #çizilmeyenler arasýnda en küçük sayýyý bulma
        
      }else{
        if(as.integer(A[i,j])<minimum){
          minimum=as.integer(A[i,j]);
        }
      }
    }
  }
  
  
  
  
  for(i in 1:5){
    for(j in 1:5){
      
      if(A[i,j]=="-"){                    #üstü çizilmeyenlerden minimumu çýkarma
        
      }else{
        temprory=as.integer(A[i,j]);
        A[i,j]=as.integer(A[i,j])-minimum;
        C[i,j]=temprory-minimum;
      }
    }
  }
  
  i=1;
  while(i<intersectionLength){
    row=as.integer(intersectionRow[i]);             #intersection noktalarýna minimumu ekleme
    column=as.integer(intersectionColumn[i]);
    C[row,column]=as.integer(C[row,column])+minimum;
    i=i+1;
  }
  
  A=C;
  
  
  
  x=1;
  
  
}
totalCost=0;
i=1;
while(i<x){
  row=as.integer(zerosCellRow[i]);
  column=as.integer(zerosCellColumn[i]);       #sonuç bulma
  totalCost=as.integer(firstTable[row,column])+totalCost;
  i=i+1;
}


print("The total cost is: ");
print(totalCost)

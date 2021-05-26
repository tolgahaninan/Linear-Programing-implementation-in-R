#Defining problem
S1_D1=11; S1_D2=13; S1_D3=17; S1_D4=14;
S2_D1=16; S2_D2=18; S2_D3=14; S2_D4=10;
S3_D1=21; S3_D2=24; S3_D3=13; S3_D4=10;

Demand_D1=200;Demand_D2=225;Demand_D3=275;Demand_D4=250;
Supply_S1=250;Supply_S2=300;Supply_S3=400;

#Assigning values to the matrix
GivenTable <- matrix(c(" ","D1","D2","D3","D4","Supply",
                       "S1",S1_D1,S1_D2,S1_D3,S1_D4,Supply_S1,
                       "S2",S2_D1,S2_D2,S2_D3,S2_D4,Supply_S2,
                       "S3",S3_D1,S3_D2,S3_D3,S3_D4,Supply_S3,
                       "Demand",Demand_D1,Demand_D2,Demand_D3,Demand_D4," "
                       
)
,ncol =6,byrow = T)

#Creating new table to save allocated values
AllocatedTable <- matrix(c(" ","D1","D2","D3","D4",
                           "S1","-","-","-","-",
                           "S2","-","-","-","-",
                           "S3","-","-","-","-"
                           
                           
)
,ncol =5,byrow = T)

i=2;j=2;
a=5;b=2;
c=2;d=6;

GivenTable[a,b];
GivenTable[c,d];
GivenTable[i,j];

TheMinumumTotalTransportationCost=0;

while(1){
  #Checking if Supply is bigger than demand or demand is bigger than supply
  if(as.double(GivenTable[a,b])<as.double(GivenTable[c,d])){
    #If demand is higher, subtract supply value from demand
    GivenTable[c,d]=as.double(GivenTable[c,d])- as.double(GivenTable[a,b]);
    #Save allocated value to the allocated table
    AllocatedTable[i,j]=as.double(GivenTable[a,b]);
    #Update the transportation cost
    TheMinumumTotalTransportationCost=TheMinumumTotalTransportationCost + as.double(GivenTable[a,b])*as.double(GivenTable[i,j]);
    #In given table since the value is allocated , assign the supply value of selected line as 0
    GivenTable[a,b]=0;
    #Since the supply is bigger than demand , we are changing b and j values to find northest-westest 
    #part of table Since the demand part runned out. We can cross that line
    b=b+1;
    j=j+1;
    
    
    
    
    
  }else if (as.double(GivenTable[a,b])>as.double(GivenTable[c,d])){
    #If supply is higher, subtract demand value from supply
    GivenTable[a,b]= as.double(GivenTable[a,b])- as.double(GivenTable[c,d]);
    #Save allocated value to the allocated table
    AllocatedTable[i,j]=as.double(GivenTable[c,d]);
    #Update the transportation cost
    TheMinumumTotalTransportationCost=TheMinumumTotalTransportationCost + as.double(GivenTable[c,d])*as.double(GivenTable[i,j]);
    #In given table since the value is allocated , assign the demand value of selected line as 0
    GivenTable[c,d]=0;
    #Since the demand is bigger than supply , we are changing c and i values to find northest-westest 
    #part of table Since the supply part runned out. We can cross that line
    c=c+1;
    i=i+1;
    
    
  }else{
    #If supply and demand is equal
    AllocatedTable[i,j]=as.double(GivenTable[a,b]);
    #Update the transportation cost
    TheMinumumTotalTransportationCost=TheMinumumTotalTransportationCost + as.double(GivenTable[a,b])*as.double(GivenTable[i,j]);
    #Since demand and supply ran out at the same time , we assign both of them 0
    GivenTable[a,b]=0;
    GivenTable[c,d]=0;
    
    break;
  }
  
}
print("The Minimum Total Transportation Cost :")
print(TheMinumumTotalTransportationCost)










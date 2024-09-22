load("CA_HOMES")
load("CA_counties")
load("CA_data_new_2")


#---------------Plotting Time v Property Type by county (raw data)--------------


#below we are going to create a multi-dimensional array where an entry is true
#if and only if for a specific county, for one property type of the five and for
#that month there is data, if not then it's false

table(CA_HOMES[CA_HOMES$region=="Los Angeles County, CA",]$period_end)


array(CA_HOMES[CA_HOMES$region=="Los Angeles County, CA",]$property_type
      ,c(120,6,1))

county_mat <- array(0,c(120,6,41))
county_mat[ ,1, ] <- unique(CA_HOMES$period_end)
Los_Angeles_Homes <- CA_HOMES[CA_HOMES$region=="Los Angeles County, CA",]


for (i in 1:120){
  date_true <- Los_Angeles_Homes$period_end == county_mat[i,1, ]
  county_subdata <- Los_Angeles_Homes[Los_Angeles_Homes$period_end == county_mat[i,1,],]
  subdata_allres_true <- county_subdata$property_type == "All Residential"
  subdata_condo_true <- county_subdata$property_type == "Condo/Co-op"
  subdata_multi_true <- county_subdata$property_type == "Multi-Family (2-4 Unit)"
  subdata_single_true <- county_subdata$property_type == "Single Family Residential"
  subdata_town_true <- county_subdata$property_type == "Townhouse"
  if (length(date_true[date_true==TRUE]) == 5) {
    county_mat[i,2:6, ] <- 1   
  } else if (length(date_true[date_true==TRUE]) < 5) {
    
    if (length(subdata_allres_true[subdata_allres_true==TRUE]) == 1) {
      county_mat[i,2,] <- 1
    }
    if (length(subdata_condo_true[subdata_condo_true==TRUE]) == 1) {
      county_mat[i,3,] <- 1
    }
    if (length(subdata_multi_true[subdata_multi_true==TRUE]) == 1) {
      county_mat[i,4,] <- 1
    }
    if (length(subdata_single_true[subdata_single_true==TRUE]) == 1) {
      county_mat[i,5,] <- 1
    }
    if (length(subdata_town_true[subdata_town_true==TRUE]) == 1) {
      county_mat[i,6,] <- 1
    }
  }
}

Mariposa_Homes <- CA_HOMES[CA_HOMES$region=="Mariposa County, CA",]

for (k in 1:41) {
  county_k <- CA_HOMES[CA_HOMES$region==CA_counties[k],]
  
  #if (length((county_k$period_end == county_mat[i,1,k ])
  #[(county_k$period_end == county_mat[i,1,k ])==TRUE]) > 0) {
  for (i in 1:120){
    date_true <- county_k$period_end == county_mat[i,1,k ]
    county_subdata <- county_k[county_k$period_end == county_mat[i,1,k],]
    subdata_allres_true <- county_subdata$property_type == "All Residential"
    subdata_condo_true <- county_subdata$property_type == "Condo/Co-op"
    subdata_multi_true <- county_subdata$property_type == "Multi-Family (2-4 Unit)"
    subdata_single_true <- county_subdata$property_type == "Single Family Residential"
    subdata_town_true <- county_subdata$property_type == "Townhouse"
    if (length(date_true[date_true==TRUE]) == 5) {
      county_mat[i,2:6,k ] <- 1   
    } else if (length(date_true[date_true==TRUE]) > 0) {
      
      if (length(subdata_allres_true[subdata_allres_true==TRUE]) == 1) {
        county_mat[i,2,k] <- 1
      }
      if (length(subdata_condo_true[subdata_condo_true==TRUE]) == 1) {
        county_mat[i,3,k] <- 1
      }
      if (length(subdata_multi_true[subdata_multi_true==TRUE]) == 1) {
        county_mat[i,4,k] <- 1
      }
      if (length(subdata_single_true[subdata_single_true==TRUE]) == 1) {
        county_mat[i,5,k] <- 1
      }
      if (length(subdata_town_true[subdata_town_true==TRUE]) == 1) {
        county_mat[i,6,k] <- 1
      }
    }
    else {
      county_mat <- county_mat
    }
  } 
} 
#}



pdf("CA_property-v-time-plot_1.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 1:7){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat[i,k,n]=="1") {
        points(as.Date(county_mat[i,1,1], "%m/%d/%Y"),(5*(n-1)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-1)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*n+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-1),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}

dev.off()

pdf("CA_property-v-time-plot_2.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 8:14){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat[i,k,n]=="1") {
        points(as.Date(county_mat[i,1,1], "%m/%d/%Y"),(5*(n-8)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-8)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-7)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-8),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()

pdf("CA_property-v-time-plot_3.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 15:21){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat[i,k,n]=="1") {
        points(as.Date(county_mat[i,1,1], "%m/%d/%Y"),(5*(n-15)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-15)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-14)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-15),labels=CA_counties[n],las=3,cex.axis=0.5)
}
dev.off()

pdf("CA_property-v-time-plot_4.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")  

for (n in 22:28){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat[i,k,n]=="1") {
        points(as.Date(county_mat[i,1,1], "%m/%d/%Y"),(5*(n-22)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-22)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-21)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-22),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()

pdf("CA_property-v-time-plot_5.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")


for (n in 29:35){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat[i,k,n]=="1") {
        points(as.Date(county_mat[i,1,1], "%m/%d/%Y"),(5*(n-29)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-29)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-28)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-29),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()

pdf("CA_property-v-time-plot_6.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 36:41){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat[i,k,n]=="1") {
        points(as.Date(county_mat[i,1,1], "%m/%d/%Y"),(5*(n-36)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-36)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-35)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-36),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()


prop_type <- unique(CA_HOMES$property_type)
prop_type



#--------------Plotting Time v Property Type by county (clean data)-------------
county_mat_2 <- array(0,c(120,6,41))
county_mat_2[ ,1, ] <- unique(CA_data_new_2$period_end)

for (k in 1:41) {
  county_k_2 <- CA_data_new_2[CA_data_new_2$region==CA_counties[k],]
  for (i in 1:120){
    date_true_2 <- county_k_2$period_end == county_mat_2[i,1,k ]
    county_subdata_2 <- county_k_2[county_k_2$period_end == county_mat_2[i,1,k],]
    subdata_allres_true_2 <- county_subdata_2$property_type == "All Residential"
    subdata_condo_true_2 <- county_subdata_2$property_type == "Condo/Co-op"
    subdata_multi_true_2 <- county_subdata_2$property_type == "Multi-Family (2-4 Unit)"
    subdata_single_true_2 <- county_subdata_2$property_type == "Single Family Residential"
    subdata_town_true_2 <- county_subdata_2$property_type == "Townhouse"
    if (length(date_true_2[date_true_2==TRUE]) == 5) {
      county_mat_2[i,2:6,k ] <- 1   
    } else if (length(date_true_2[date_true_2==TRUE]) > 0) {
      
      if (length(subdata_allres_true_2[subdata_allres_true_2==TRUE]) == 1) {
        county_mat_2[i,2,k] <- 1
      }
      if (length(subdata_condo_true_2[subdata_condo_true_2==TRUE]) == 1) {
        county_mat_2[i,3,k] <- 1
      }
      if (length(subdata_multi_true_2[subdata_multi_true_2==TRUE]) == 1) {
        county_mat_2[i,4,k] <- 1
      }
      if (length(subdata_single_true_2[subdata_single_true_2==TRUE]) == 1) {
        county_mat_2[i,5,k] <- 1
      }
      if (length(subdata_town_true_2[subdata_town_true_2==TRUE]) == 1) {
        county_mat_2[i,6,k] <- 1
      }
    }
    else {
      county_mat_2 <- county_mat_2
    }
  } 
} 

pdf("CA_property-v-time-plot_1_clean.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 1:7){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat_2[i,k,n]=="1") {
        points(as.Date(county_mat_2[i,1,1], "%m/%d/%Y"),(5*(n-1)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-1)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-1)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*n+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-1),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()

pdf("CA_property-v-time-plot_2_clean.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 8:14){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat_2[i,k,n]=="1") {
        points(as.Date(county_mat_2[i,1,1], "%m/%d/%Y"),(5*(n-8)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-8)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-8)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-7)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-8),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()

pdf("CA_property-v-time-plot_3_clean.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 15:21){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat_2[i,k,n]=="1") {
        points(as.Date(county_mat_2[i,1,1], "%m/%d/%Y"),(5*(n-15)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-15)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-15)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-14)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-15),labels=CA_counties[n],las=3,cex.axis=0.5)
}
dev.off()

pdf("CA_property-v-time-plot_4_clean.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")  

for (n in 22:28){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat_2[i,k,n]=="1") {
        points(as.Date(county_mat_2[i,1,1], "%m/%d/%Y"),(5*(n-22)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-22)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-22)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-21)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-22),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()

pdf("CA_property-v-time-plot_5_clean.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")


for (n in 29:35){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat_2[i,k,n]=="1") {
        points(as.Date(county_mat_2[i,1,1], "%m/%d/%Y"),(5*(n-29)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-29)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-29)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-28)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-29),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()

pdf("CA_property-v-time-plot_6_clean.pdf", width=12.5, height=9.5)

par(mar=c(5.1, 8, 4.1, 4.1))
plot(x=c(as.Date("1/31/2012","%m/%d/%Y"),as.Date("12/31/2021","%m/%d/%Y")),
     y=c(1,35),type="n", yaxt="n", main="Property type v Date",
     xlab="Year",ylab="")

for (n in 36:41){
  for (k in 2:6) {
    for (i in 1:120){
      if (county_mat_2[i,k,n]=="1") {
        points(as.Date(county_mat_2[i,1,1], "%m/%d/%Y"),(5*(n-36)+k-1), col=c(k), pch=19)
      }
    }
  }
  axis(2, at=c(5*(n-36)+1),labels=c("All Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+2),labels=c("Condo/Co-op"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+3),labels=c("Multi-Family (2-4 Unit"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+4),labels=c("Single Family Residential"),las=2,cex.axis=0.7)
  axis(2, at=c(5*(n-36)+5),labels=c("Townhouse"),las=2,cex.axis=0.7)
  abline(h=5*(n-35)+0.5, col="black")
  # mtext(side = 4, line = 2, at = n, adj = -0.6-n*(0.5), CA_counties[n],cex=0.7)
  axis(4, at=3+5*(n-36),labels=CA_counties[n],las=3,cex.axis=0.5)
  
}
dev.off()



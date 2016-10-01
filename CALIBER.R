# 
# The script below is utilised to visualise RTI-related consultations and hospitalisations, 
# their hospital length of stay and antibiotic prescriptions.  This provided a more visual 
# representation of their pathway of care across primary and secondary care. 
#
#
# 
# 
# 
# Here, it is commented the STATA script used to generate the table "patient.csv"
# used in the R script.
#
# 
# use "NUMERATOR",clear
# 
# keep if (group=="URTI" | group=="LRTI" I group=="RTI")
# 
# sort patid
# merge m:1 patid2 using "DENOMINATOR"
# drop _merge
# 
# outsheet using "patient.csv", replace comma
#
#


uuu=read.csv("patient.csv")

# Create a matrix with the patient ID of some subject

rere=matrix(ncol=6,nrow=2)
rere[1,]=c(19438332,5198053,7244269,816441,1473457,2153173)
rere[2,]=c(17120264,41388427,14654441,11283468,10068187,17120264)

# Two loop: one for children with DS and one for matched controls
for(iii in 1:2){
	rele=rere[iii,]
	
	# Generate the file where figures will be saved
	name=paste("patients-",iii,".pdf",sep="")
	pdf(name,height=10)
	par(mfrow=c(3,2))
	for(kk in rele){
		u=as.data.frame(uuu)
		u=u[u[,"patid"]==kk,]
		sel=u[,"group"]=="LRTI" | u[,"group"]=="URTI" | u[,"group"]=="RTI"
		u=u[sel,]
		wu=rep(NA,length(u[,"group"]))
		wu[u[,"group"]=="LRTI"]=1
		wu[u[,"group"]=="URTI"]=3
		wu[u[,"group"]=="RTI"]=5
		wu[u[,"pp"]]=="POSSIBLE"]=wu[u[,"pp"]=="POSSIBLE"]+1
		u[,"eventdate"]=as.Date(u[,"eventdate"],format="%d%B%Y")
		u[,"admidate"]=as.Date(u[,"admidate"],format="%d%B%Y")
		u[,"discharged"]=as.Date(u[,"discharged"],format="%d%B%Y")
		u[,"dob"]=as.Date(u[,"dob"],format="%d%B%Y")
		begin=as.Date("1jan1997",format="%d%B%Y")
		end=as.Date("25mar2010",format="%d%B%Y")
		start=as.Date(u[1,"date_entry"],format="%d%B%Y")
		stop=as.Date(u[1,"date_exit"],format="%d%B%Y")
		start_age=(start-u[1,"dob"])/365.25
		stop_age=(stop-u[1,"dob"])/365.25
		u=u[u[,"eventdate"]<end & u[,"eventdate"]>begin]
		u[,"group"]=as.factor(as.vector(u[,"group"]))
		nn=6
		colo=rainbow(nn)
		y=(u[,"eventdate"]-u[,"dob"])/365.25
		x=u[,"eventdate"]
		wuda=rep(NA,length(u[,"dataset"]))
		wuda[u[,"dataset"]=="PRIMARY CARE"]=1
		wuda[u[,"dataset"]=="SECONDARY CARE"]=2
		plot(u[,"eventdate"],y,bg=colo[as.numeric(wu)],pch=20+wuda,
		     cex=2,las=2,xlab="Event (year)",ylab="Age (year)",main=paste("Patient",kk),
		     ylim=c(start_age,stop_age),
		     xlim=c(as.Date("01jan1993",format"%d%B%Y"),as.Date("31dec2011",format="%d%B%Y")))
		abline(v=begin,col=2,lty=2)
		abline(v=end,col=2,lty=2)
		abline(v=start,col=3,lty=3)
		abline(v=stop,col=3,lty=3)
		abline(v=start_age,col=3,lty=3)
		abline(v=stop_age,col=3,lty=3)
		points(x[u[,"antibiotic"]=="YES"],y[u[,"antibiotic"]=="YES"],col=2,pch=21,cex=3)
		yy=seq(min(y),max(y),length.out=nn)
		yyy=(stop_age-start_age)/nn*(0:(nn-1))+start_age
		if(any(u[,"dataset"]=="SECONDARY CARE")){
			uu=u[u[,"dataset"]=="SECONDARY CARE",]
			for(i in 1:nrow(uu)){
				rect(as.Date("01jan2011",format="%d%B%Y"),(uu[i,"admidate"]-u[i,"dob"])/365.25,
				     as.Date("31dec2011",format="%d%B%Y"),(uu[i,"discharged"]-u[i,"dob"])/365.25)
			}
		}
	}
	dev.off()
}

































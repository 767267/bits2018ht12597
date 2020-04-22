
source("read_diab_file.r")
source("get_filtered_data.r")

dat = get_filtered_data()




# columns 
#    1 Pregnancies, 
#    2 Glucose, 
#    3 BloodPressure, 
#    4 SkinThickness, 
#    5 Insulin,  
#    6 BMI, 
#    7 DiabetesPedigreeFunction, 
#    8 Age, 
#    9 Outcome

cluster_colors <- c("red", "orange" , "green")

#print(dat[1])

range(dat$Glucose)
range(dat$Insulin)

max_preg = max(dat$Pregnancies)
max_gluc = max(dat$Glucose)
max_blpr = max(dat$BloodPressure)
max_skth = max(dat$SkinThickness)
max_insu = max(dat$Insulin)
max_bmii = max(dat$BMI)
max_dped = max(dat$DiabetesPedigreeFunction)
max_agee = max(dat$Age)

maxs = cat(max_preg, max_gluc,max_blpr,max_skth,max_insu,max_bmii,max_dped,max_agee, sep=", ")

#subset(dat, cyl == 8 & gear == 3)$freq
c_max_preg = count(dat,dat$Pregnancies == max_preg)
c_max_gluc = count(dat,dat$Glucose == max_gluc)
c_max_blpr = count(dat,dat$BloodPressure == max_blpr)
c_max_skth = count(dat,dat$SkinThickness == max_skth)
c_max_insu = count(dat,dat$Insulin == max_insu)
c_max_bmii = count(dat,dat$BMI == max_bmii)
c_max_dped = count(dat,dat$DiabetesPedigreeFunction == max_dped)
c_max_agee = count(dat,dat$Age == max_agee)

print(c_max_preg)
print(c_max_gluc)
print(c_max_blpr)
print(c_max_skth)
print(c_max_insu)
print(c_max_bmii)
print(c_max_dped)
print(c_max_agee)


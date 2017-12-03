##################################################################################
#Loading Data Dictionary into R
##################################################################################
library(rmongodb)
myConnection1 = mongo.create();
ABCD = mongo.find.all(myConnection1, 'Assignment.datadic');
myDataDictionary = do.call(rbind.data.frame,ABCD);
View(myDataDictionary)
write.table(myDataDictionary,"C:/Users/JATESH/Desktop/Dataset/New folder/Data_Dictionary.txt", row.names=FALSE)

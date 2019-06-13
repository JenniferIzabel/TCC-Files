setwd("/home/jennifer/Documents/UTFPR/2018-1/ES36O")

#abstract

#filter by test and issue
#arqcsv2 <-arqcsv[arqcsv$unit_test > 0 & arqcsv$issues > 0,] 

#save csv
#write.csv(arqcsv2 ,file = "/home/jennifer/Documents/UTFPR/TCC/dataset2.csv")

arqcsv2 = read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/dataset2.csv") #open csv
maxPerLanguage<- 30
#filter per language
# filterC <-arqcsv2[arqcsv2$language == "C",]
# filterCplusplus <-arqcsv2[arqcsv2$language == "C++",]
# filterCsharp <-arqcsv2[arqcsv2$language == "C#",]
# filterJava <-arqcsv2[arqcsv2$language == "Java",]
# filterPython <-arqcsv2[arqcsv2$language == "Python",]
filterPHP <-arqcsv2[arqcsv2$language == "PHP",]
filterRuby <-arqcsv2[arqcsv2$language == "Ruby",]

#order by issue
# filterBiggestC <- filterC[order(-filterC$issues) , ]
# filterBiggestCplusplus <- filterCplusplus[order(-filterCplusplus$issues) , ]
# filterBiggestCsharp <- filterCsharp[order(-filterCsharp$issues) , ]
# filterBiggestJava <- filterJava[order(-filterJava$issues) , ]
# filterBiggestPython <- filterPython[order(-filterPython$issues) , ]
filterBiggestPHP <- filterPHP[order(-filterPHP$issues) , ]
filterBiggestRuby <- filterRuby[order(-filterRuby$issues) , ]

#lists of top repositories 
# CRepositories <- filterBiggestC[1:maxPerLanguage,]
# CPlusPlusRepositories <- filterBiggestCplusplus[1:maxPerLanguage,]
# CSharpRepositories <- filterBiggestCsharp[1:maxPerLanguage,]
# JavaRepositories <- filterBiggestJava[1:maxPerLanguage,]
# PythonRepositories <- filterBiggestPython[1:maxPerLanguage,]
PHPRepositories <-filterBiggestPHP[1:maxPerLanguage,]
RubyRepositories <- filterBiggestRuby[1:maxPerLanguage,]

# write.csv(CRepositories ,file = "/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/CRepositories2.csv")
# write.csv(CPlusPlusRepositories ,file = "/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/CPlusPlusRepositories2.csv")
# write.csv(CSharpRepositories ,file = "/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/CSharpRepositories2.csv")
# write.csv(JavaRepositories ,file = "/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/JavaRepositories2.csv")
# write.csv(PythonRepositories ,file = "/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/PythonRepositories2.csv")
write.csv(PHPRepositories ,file = "/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/PHPRepositories3.csv")
write.csv(RubyRepositories ,file = "/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/RubyRepositories3.csv")




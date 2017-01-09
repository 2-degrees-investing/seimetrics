# Formatting
for(i in 1:ncol(GDmaster2)) {
	if (typeof(GDmaster2[,i]) == 'character') {
    GDmaster[,i] <- str_replace_all(GDmaster2[,i], "\n", "") # linux
	} 
	if (i == 2 || i == 3) {
    GDmaster[,i] <- iconv(GDmaster2[,i], "", "ASCII", "byte") #DEFAULT
	}
}

devtools::install_github('TRIPLE-CHIST-ERA/VoIDR')
library(VoIDR)

makePackage('UniprotR','https://sparql.uniprot.org', voidEndpoint  = 'https://sparql.uniprot.org')
install.packages('UniprotR_1.0.tar.gz')

library(UniprotR)
?get_Database
get_Database()
?get_Taxon
x <- get_Taxon()
x <- get_Taxon(properties = 'commonName',limit = 10000)
y <- get_Taxon(properties = "partOfLineage",limit = 10000)
z <- get_Taxon(properties = 'depiction',limit = 10000)
w <- dplyr::inner_join(x,y)
u <- dplyr::inner_join(w,z)

?get_Interaction()
x <- get_Interaction('participant')
expandDF(x)

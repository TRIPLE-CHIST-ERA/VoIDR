devtools::install_github('TRIPLE-CHIST-ERA/VoIDR')
library(VoIDR)
makePackage('UniprotR','https://sparql.uniprot.org', voidEndpoint  = 'https://sparql.uniprot.org')


library(UniprotR)
get_Database()
?get_Taxon
x <- get_Taxon(properties = 'commonName',limit = 5000000)
y <- get_Taxon(properties = "partOfLineage",limit = 5000000)
y <- get_Taxon(properties = 'depiction',limit = 5000000)
z <- expandDF(y)
w <- inner_join(x,z)


get_Graph()
get_Interaction()
get_Interaction('participant')
?get_Participant
get_

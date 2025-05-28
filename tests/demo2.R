library(VoIDR)

f <- makePackage('UniprotR','https://sparql.uniprot.org', voidEndpoint = 'http://localhost:7200/repositories/uniprotvoid')
f
install.packages('./UniprotR_1.0.tar.gz')
library(UniprotR)
?get_Taxon
x <- get_Taxon(c('mnemonic', 'commonName', 'strain'))
str(x)
x
y <- get_Taxon(c('mnemonic','replaces', 'depiction'), only.complete.cases = TRUE, limit = NULL)
y
?get_Protein
get_Protein(c('complete', 'mnemonic', 'interaction'), only.complete.cases = TRUE, limit = NULL)

get_Interaction(only.complete.cases = TRUE, limit = NULL)


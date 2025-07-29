devtools::install_github("TRIPLE-CHIST-ERA/VoIDR")

library(VoIDR)

make_package(
  package_name = "UniprotR",
  endpoint = "https://sparql.uniprot.org",
  void_endpoint  = "https://sparql.uniprot.org"
)


install.packages("./UniprotR_1.0.tar.gz")

library(UniprotR)
?get_Database
get_Database()
?get_Taxon
x <- get_Taxon()
x <- get_Taxon(properties = "commonName", limit = 10000)
y <- get_Taxon(properties = "partOfLineage", limit = 10000)
z <- get_Taxon(properties = "depiction", limit = 10000)
w <- dplyr::inner_join(x, y)
u <- dplyr::inner_join(w, z)

?get_Interaction()
x <- get_Interaction("participant")
x
expand_df(x)


x <- get_Taxon("http://purl.uniprot.org/taxonomy/10")
x

x <- get_Taxon(properties = "commonName", limit = 10)
?get_Gene()
x <- get_Gene()
?get_Graph

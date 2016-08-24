dados <- data.frame(
  CNPJ = dfPART_LIC$CNPJCPF_FORNECEDORES,
  ID_LICITACAO = dfPART_LIC$COD_LICITACAO,
  ID_ITEM = dfPART_LIC$ID_ITEM,
  VENCEDOR = dfPART_LIC$VENCEDOR == 'S')

e <- RcextTools::rcextCriaGrafoLic(dados)

wc <- igraph::walktrap.community(e$igGrafo)

# calcula densidade interna das comunidades
den <- sapply(sort(unique(igraph::membership(wc))), function(g) {
  subg<-igraph::induced.subgraph(e$igGrafo, which(igraph::membership(wc)==g))
  igraph::ecount(subg)/igraph::ecount(e$igGrafo)
})

# selecao de empresas a partir do page rank intra-comunitario
selec_pr <- sapply(sort(unique(igraph::membership(wc))), function(g) {
  subg<-igraph::induced.subgraph(e$igGrafo, which(igraph::membership(wc)==g))
  pr <- sort(igraph::page.rank(subg)$vector, decreasing = T)

  # selecionar empresas cujo page_rank acumulado é inferior a 0.8
  selec_emp <- cumsum(pr)<.6

  # numero maximo de empresas a ser mantido: 30% do total do mercado
  max_emp <- max(3,ceiling(0.3*length(pr)))

  if (sum(selec_emp) <= max_emp) {
    return(names(pr)[selec_emp])
  } else {
    return(NULL)
  }
})

# seleção de empresas a partir da densidade intra-comunitaria
# mantem apenas as comunidades que tenham densidade superior ao 3º quartil dentre as comunidades
# com densidade não nula
comunidades_mantidas <- den>fivenum(den[den>0])[4]
vertices_mantidos <- wc$names[wc$membership %in% seq(1,max(wc$membership))[comunidades_mantidas]]


vertices_descartados <- !(wc$names[vertices_descartados] %in% intersect(vertices_mantidos, unlist(selec_pr)))

sg <- igraph::delete.vertices(
  e$igGrafo,
  wc$names[vertices_descartados])
sg <- igraph::simplify(sg)


plot(sg)

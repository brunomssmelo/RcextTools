#' Metodo S3 que plota na tela uma representacao visual do grafo do tipo `igraph` contido no objeto da classe `TipologiaRodizio`
#' @author Bruno M. S. S. Melo
#' @description Os diferentes agrupamentos representam empresas suspeitas de praticarem alguma acao colusiva num determinado mercado. As arestas apontam na direcao de um perdedor para um vencedor de licitacao. Empresas sao sempre perdedoras sao representadas por quadrados cinzas.
#' @param x objeto da classe `TipologiaRodizio`.
#' @param ...	eventuais parametros adicionais.
#' @examples
#' \dontrun{
#' casosSuspeitos <- rodizioIdentificaSituacoesSuspeitas(dados)
#' plot(casosSuspeitos)
#' }
#' @export
plot.TipologiaRodizio <- function(x, ...){

  # solving CRAN check issue
  '%>%' <- NULL
  library("visNetwork")
  library("igraph")

  dadosGrafo <- toVisNetworkData(x$grafo)
  dadosGrafo$nodes$group <- sapply(dadosGrafo$nodes$id, FUN = function(n){
    group <- x$tabela[CNPJ == n,]$MERCADO_ATUACAO[1]
  })
  dadosGrafo$nodes[!complete.cases(dadosGrafo$nodes),]$group <- 0

  visNetwork::visNetwork(nodes = dadosGrafo$nodes, edges = dadosGrafo$edges) %>%
    visNetwork::visInteraction( navigationButtons = TRUE, multiselect = TRUE ) %>%
    visNetwork::visOptions(nodesIdSelection = TRUE) %>%
    visNetwork::visEdges(arrows = 'to') %>%
    visNetwork::visGroups(groupname  = '0', color = 'grey', shape = "square")
}

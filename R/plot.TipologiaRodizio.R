#' Metodo S3 que plota na tela uma representacao visual do grafo do tipo `igraph` contido no objeto da classe `TipologiaRodizio`
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' casosSuspeitos <- rodizioIdentificaSituacoesSuspeitas(dados)
#' plot(casosSuspeitos)
#' }
#' @export
plot.TipologiaRodizio <- function(x){
  library(visNetwork)
  library(igraph)
  visIgraph(x$mercados.grafos) %>%
    visInteraction( navigationButtons = TRUE, multiselect = TRUE ) %>%
    visOptions(nodesIdSelection = TRUE)
}

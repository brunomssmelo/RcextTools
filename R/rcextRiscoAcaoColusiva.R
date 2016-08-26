#' Identifica potencias mercados de risco de praticas colusivas a partir de grafo de licitacoes
#' @param grLicitacoes objeto do tipo \code{igraph} contendo um grafo direcionado de vencedores e participantes
#'          de licitacoes;
#' @return objeto do tipo environment, contendo os seguintes objetos:
#' \itemize{
#'         \item mercados objeto do tipo \code{community} contendo todos as comunidades (mercados) obtidas a partir do grafo \code{grLicitacoes};
#'         \item grMercadosRisco grafo do tipo \item{igraph} contendo os mercados de risco extraidos do grafo \code{grLicitacoes};
#'         \item vcMercadosRisco vetor do tipo \code{numeric()} contendo os identificadores dos mercados considerados de risco;
#'         \item vcEmpresasRisco vetor do tipo \code{numeric()} contendo os identificadores dos mercados de risco a que pertencem as empresas
#'               consideradas suspeitas de praticarem acoes colusivas. As empresas sao identificadas pelo atributo \code{names}.
#'          }
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' grafoLic <- rcextAcaoColusivaLic(grLicitacoes)
#' }
#' @seealso \code{igraph}
#' @export
rcextRiscoAcaoColusiva <- function(grLicitacoes) {

  e <- new.env(parent = emptyenv())

  # identifica "comunidades" (mercados)
  wc <- igraph::walktrap.community(grLicitacoes)

  # inclui no environment "e" uma copia do grafo "grLicitacoes" a partir da qual sera construido
  # o grafo contendo unicamente as comunidades correspondentes a "mercados de risco"
  e$grMercadosRisco <- grLicitacoes

  # inicializa o vetor que ira conter as comunidades (mercados) consideradas como "de risco"
  e$vcMercadosRisco <- numeric()

  # inicializa o vetor que ira conter as empresas suspeitas de pratica colusiva
  e$vcEmpresasRisco <- numeric()

  # selecao de empresas a partir do page rank intra-comunitario
  sapply(sort(unique(igraph::membership(wc))), function(g) {

    # empresas que pertencem a comunidade g (mercado)
    empresas_comunidade_g <- which(igraph::membership(wc)==g)

    # extrai subgrafo correspondente a comunidade g
    subg<-igraph::induced.subgraph(grLicitacoes, empresas_comunidade_g)

    # calcula page rank intracomunitario
    pr <- sort(igraph::page.rank(subg)$vector, decreasing = T)

    # seleciona as empresas de maior page_rank até que o rank acumulado seja de 0.6
    selec_emp <- cumsum(pr)<.6

    # o numero de empresas acima selecionadas não devera ultrapassar 30% do total
    max_emp <- ceiling(0.3*length(pr))

    # ... nem ser inferior a 5
    min_emp <- 5

    if ((sum(selec_emp) <= max_emp) & (sum(selec_emp) >= min_emp)) {

      # insere a comunidade (mercado) na listagem de mercados de risco
      e$vcMercadosRisco <- c(e$vcMercadosRisco, g)

      # atualiza o vetor de empresas suspeitas
      vcEmpresasRisco <- rep(g, sum(selec_emp))
      names(vcEmpresasRisco) <- wc$names[empresas_comunidade_g[selec_emp]]
      e$vcEmpresasRisco <- c(e$vcEmpresasRisco, vcEmpresasRisco)
    } else {
      # retira todo o subgrafo (comunidade) do grafo principal
      e$grMercadosRisco <- igraph::delete.vertices(
        e$grMercadosRisco,
        wc$names[empresas_comunidade_g])
    }
  })

  e$mercados <- wc

  return(e)
}

#' Cria um grafo de vencedores e participantes de licitações públicas
#'
#' Utiliza-se um grafo direcionado para representar a relação entre as empresas participantes das licitações,
#' da seguinte forma:
#' \itemize{
#'         \item cada empresa é representada por um nó;
#'         \item as empresas que participaram de um mesmo certame estarão associadas por
#'          relações do tipo “perdedor-vencedor”. Tal relação é representada por uma aresta
#'          que se inicia em nó representativo da empresa participante perdedora para um nó
#'          representativo da licitante vencedora.
#' }
#'
#'
#' @param dados data.frame contendo as seguintes colunas:
#' \itemize{
#'         \item CNPJ coluna do tipo \code{character} contendo cnpj, com 14 caracteres (sem .,-, ou /),
#'          da empresa participante do certame;
#'         \item ID_ORGAO coluna do tipo \code{character} que identifica de forma única o órgão responsável
#'         pelo certame;
#'         \item ID_LICITACAO coluna do tipo \code{character} que identifica de forma única o certame;
#'         \item ID_ITEM coluna do tipo \code{character} que identifica de forma única o item do objeto a que
#'         a empresa esteja concorrendo. Caso o objeto da licitação não tenha sido dividido em itens, este campo
#'         deverá ser NA;
#'         \item ARTIGO coluna do tipo \code{character} contendo texto que descreve de forma genérica o item;
#'         \item ID_OBJETO coluna do tipo \code{character} que identifica de forma única o obeto do certame;
#'         \item DESC_OBJETO coluna do tipo \code{character} contendo texto que descreve sucintamente o objeto; e
#'         \item VENCEDOR coluna do tipo \code{character} contendo cnpj do vencedor do item do certame, com 14
#'         caracteres (sem .,-, ou /), da empresa participante do certame.
#' }
#' @return um objeto do tipo igraph contendo um grafo direcionado de vencedores e participantes de licitações.
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' grafoLic <- rcextCriaGrafoLic(dados = dfDadosLic)
#' }
#' @seealso \code{igraph}
#' @export
rcextCriaGrafoLic <- function(dados) {
  return(NULL)
}

#' Cria um grafo de vencedores e participantes de licitacoes publicas
#'
#' Utiliza-se um grafo direcionado para representar a relacao entre as empresas participantes das licitacoes,
#' da seguinte forma:
#' \itemize{
#'         \item cada empresa e representada por um no;
#'         \item as empresas que participaram de um mesmo certame estarao associadas por
#'          relacoes do tipo “perdedor-vencedor”. Tal relacao e representada por uma aresta
#'          que se inicia em no representativo da empresa participante perdedora para um no
#'          representativo da licitante vencedora.
#'         \item o desconto ofertado (diferenca entre o valor estimado e o valor homologado)
#'         podera influenciar, de forma inversamente proporcional, o peso das relacoes
#'         perdedor-vencedor. Quanto menor o desconto ofertado pelo vencedor, maior sera o peso
#'         da referida relacao.
#' }
#' @param dados data.frame contendo as seguintes colunas:
#' \itemize{
#'         \item CNPJ coluna do tipo \code{character} contendo cnpj, com 14 caracteres (sem .,-, ou /),
#'          da empresa participante do certame;
#'         \item ID_LICITACAO coluna do tipo \code{character} que identifica de forma unica o certame;
#'         \item ID_ITEM coluna do tipo \code{character} que identifica de forma unica o item do objeto a que
#'         a empresa esteja concorrendo. Caso o objeto da licitacao nao tenha sido dividido em itens, este campo
#'         \item VENCEDOR coluna do tipo \code{logical} contendo um valor booleano indicando se o licitante foi
#'         vitorioso no certame.
#'         \item VALOR_ESTIMADO coluna do tipo \code{numeric} correspondente ao valor estimado para o objeto ou
#'         serviço sendo licitado. Podera assumir o valor NA caso tal informacao nai esteja disponivel.
#'         \item VALOR_HOMOLOGADO coluna do tipo \code{numeric} correspondente ao valor homologado da proposta
#'         vencedora para o fornecimento do objeto ou serviço sendo licitado. Podera assumir o valor NA caso tal
#'         informacao nai esteja disponivel.
#' }
#' @param tipo_retorno especifica o objeto a ser retornado pela funcao. As opcoes sao as que se seguem:
#' \itemize{
#'         \item 0 retorna um objeto do tipo \code{environment}, contendo um objeto do tipo \code{igraph}
#'          (igGrafo) e um \code{data.frame} (dfGrafo) a partir do qual o mesmo foi criado. E o valor padrao;
#'         \item 1 retorna um objeto do tipo \code{igraph} contendo um grafo direcionado de vencedores e participantes
#'          de licitacoes;
#'         \item 2 retorna um objeto do tipo \code{data.frame} a partir do qual poderá ser criado um grafo
#'          por meio da funcao \code{igraph::graph.data.frame()}
#'          }
#' @param considerar_desconto parametro do tipo \code{logical} indicando se o desconto obtido (diferenca entre o valor
#' homologado e o valor estimado) devera ser levado em consideracao na atribuicao dos pesos das relacoes perdedor-vencedor.
#' Por padrao este parametro tem valor \code{TRUE}
#' @return o retorno depende do valor especificado para o parâmetro \code{tipo_retorno}.
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' grafoLic <- rcextCriaGrafoLic(dados = dfDadosLic, tipo_retorno = 0, considerar_desconto = F)
#' }
#' @seealso \code{igraph}
#' @export
rcextCriaGrafoLic <- function(dados, tipo_retorno = 0, considerar_desconto = T) {

  if ((!is.numeric(tipo_retorno)) | (!(tipo_retorno %in% 0:2))) {
    tipo_retorno <- 0
  }

  dfPERDEDOR <- subset(x = dados, VENCEDOR == F)
  dfVENCEDOR <- subset(x = dados, VENCEDOR == T)

  envGrafo <- new.env(parent = emptyenv())

  envGrafo$dfGrafo <- sqldf::sqldf(
    'SELECT DISTINCT
          dfPERDEDOR.CNPJ CNPJ_PERDEDOR,
          dfVENCEDOR.CNPJ CNPJ_VENCEDOR,
          dfVENCEDOR.ID_LICITACAO,
          dfVENCEDOR.ID_ITEM,
          dfVENCEDOR.VALOR_ESTIMADO,
          dfVENCEDOR.VALOR_HOMOLOGADO
      FROM
          dfVENCEDOR
      INNER JOIN
          dfPERDEDOR
            ON (dfVENCEDOR.ID_LICITACAO = dfPERDEDOR.ID_LICITACAO
                AND dfVENCEDOR.ID_ITEM = dfPERDEDOR.ID_ITEM
                AND dfVENCEDOR.CNPJ != dfPERDEDOR.CNPJ )'
  )

  if (considerar_desconto){
    # retira registros que nao possaum valores estimados e valores homologados
    envGrafo$dfGrafo <- envGrafo$dfGrafo[complete.cases(envGrafo$dfGrafo[, c("VALOR_ESTIMADO", "VALOR_HOMOLOGADO")]),]

    # calcula peso da relacao perdedor-vencedor
    envGrafo$dfGrafo$PESO_RELACAO <- envGrafo$dfGrafo$VALOR_HOMOLOGADO/envGrafo$dfGrafo$VALOR_ESTIMADO

    # retira registros cujos pesos calculados sejam outliers
    tukey <- fivenum(envGrafo$dfGrafo$PESO_RELACAO)
    Q3 <- tukey[4]
    Q1 <- tukey[2]
    envGrafo$dfGrafo <- envGrafo$dfGrafo[envGrafo$dfGrafo$PESO_RELACAO < (Q3 + 1.5*(Q3-Q1)),]

  } else{
    envGrafo$dfGrafo$PESO_RELACAO <- 1
  }

  envGrafo$igGrafo <- igraph::graph.data.frame(
    data.frame(
      from = envGrafo$dfGrafo[, "CNPJ_PERDEDOR"],
      to = envGrafo$dfGrafo[, "CNPJ_VENCEDOR"],
      weight = envGrafo$dfGrafo[, "PESO_RELACAO"]
    )
  )

  return(envGrafo)
}

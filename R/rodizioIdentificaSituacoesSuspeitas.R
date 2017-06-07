#' Identifica potenciais mercados de risco de praticas colusivas a partir de grafo de licitacoes
#' @param dados data.frame contendo as seguintes colunas:
#' \itemize{
#'         \item \strong{CNPJ} coluna do tipo \code{character} contendo cnpj, com 14 caracteres (sem .,-, ou /),
#'          da empresa participante do certame;
#'         \item \strong{ID_LICITACAO} coluna do tipo \code{character} que identifica de forma unica o certame;
#'         \item \strong{ID_ITEM} coluna do tipo \code{character} que identifica de forma unica o item do objeto a que
#'         a empresa esteja concorrendo. Caso o objeto da licitacao nao tenha sido dividido em itens, este campo
#'         \item \strong{VENCEDOR} coluna do tipo \code{logical} contendo um valor booleano indicando se o licitante foi
#'         vitorioso no certame.
#'         \item \strong{VALOR_ESTIMADO} coluna do tipo \code{numeric} correspondente ao valor estimado para o objeto ou
#'         serviço sendo licitado. Podera assumir o valor NA caso tal informacao nai esteja disponivel.
#'         \item \strong{VALOR_HOMOLOGADO} coluna do tipo \code{numeric} correspondente ao valor homologado da proposta
#'         vencedora para o fornecimento do objeto ou serviço sendo licitado. Podera assumir o valor NA caso tal
#'         informacao nai esteja disponivel.
#'         }
#' @param considerar_desconto parametro do tipo \code{logical} indicando se o desconto obtido (diferenca entre o valor
#' homologado e o valor estimado) devera ser levado em consideracao na atribuicao dos pesos das relacoes perdedor-vencedor.
#' Por padrao este parametro tem valor \code{TRUE}
#' @return objeto do tipo environment, contendo os seguintes objetos:
#' \itemize{
#'         \item \strong{cmMercados} objeto do tipo \code{community} contendo todos as comunidades (mercados) obtidas a partir do grafo \code{grLicitacoes};
#'         \item \strong{grMercadosRisco} grafo do tipo \code{igraph} contendo os mercados de risco extraidos do grafo \code{grLicitacoes};
#'         \item \strong{vcMercadosRisco} vetor do tipo \code{numeric()} contendo os identificadores dos mercados considerados de risco;
#'         \item \strong{vcEmpresasRisco} vetor do tipo \code{numeric()} contendo os identificadores dos mercados de risco a que pertencem as empresas
#'               consideradas suspeitas de praticarem acoes colusivas. As empresas sao identificadas pelo atributo \code{names}.
#'         \item \strong{dfResultados} objeto do tipo \code{data.frame} contendo os contratos considerados como suspeitos.
#'         }
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#'
#'  # carrega dados de licitacoes da base fornecida pelo pacote RcextTools
#'  data("part_lic")
#'
#'  dtDados <- part_lic[!is.na(part_lic$COD_LICITACAO),]
#'
#'  dtDados <- data.frame(
#'    CNPJ = dtDados$CNPJCPF_FORNECEDORES,
#'    ID_LICITACAO = dtDados$COD_LICITACAO,
#'    ID_ITEM = dtDados$ID_ITEM,
#'    VENCEDOR = ifelse(dtDados$VENCEDOR == 'S', T, F),
#'    VALOR_ESTIMADO = NA,
#'    VALOR_HOMOLOGADO = as.numeric(dtDados$VALOR_FINAL),
#'    DESC_OBJETO = dtDados$RESUMO_OBJETO,
#'    stringsAsFactors = F
#'  )
#'
#'  casosSuspeitos <- rodizioIdentificaSituacoesSuspeitas(dtDados)
#'
#'  # imprime dataframe com resultados
#'  print(casosSuspeitos)
#'
#'  # plota grafo
#'  plot(casosSuspeitos)
#' }
#' @seealso \code{igraph}
#' @importFrom stats complete.cases
#' @importFrom data.table data.table
#' @export
rodizioIdentificaSituacoesSuspeitas <- function(dados, considerar_desconto = F) {

  # para passar nos checks do CRAN:
  CNPJ = NULL
  MERCADO_ATUACAO = NULL
  VALOR_HOMOLOGADO = NULL
  VENCEDOR = NULL

  dados <- data.table(dados)

  # Geracao do grafo para ser analisado
  grafo <- rodizioCriaGrafoLic(dados, 0, considerar_desconto)

  # Identificacao de empresas e "mercados" de maior risco de acao colusiva
  e <- rodizioMetodologiaGrafoPageRank(grafo$grLicitacoes)

  # Mantem apenas as empresas vencedoras
  dtResultados <- dados[VENCEDOR == T & CNPJ %in% names(e$vcEmpresasRisco),]

  # Inclui informacao de mercado
  dtResultados$MERCADO_ATUACAO <- apply(
    X = dtResultados,
    MARGIN = 1,
    FUN = function(contratos, empresas = e$vcEmpresasRisco){
      empresas[contratos['CNPJ'] == names(empresas)]
    }
  )

  # Ordena resultados por mercado de atuação e materialidade
  dtResultados <- dtResultados[order(MERCADO_ATUACAO,-VALOR_HOMOLOGADO)]


  # Inclui coluna com valor homologado em formato texto do tipo moeda
  numericoParaTextoMoeda <- function(x){
    paste0("R$",
           formatC(as.numeric(x),
                   format="f",
                   digits=2,
                   big.mark=".",
                   decimal.mark = ",")
    )
  }

  dtResultados$TEXTO_VALOR_HOMOLOGADO <- NA_character_
  dtResultados[!is.na(VALOR_HOMOLOGADO),]$TEXTO_VALOR_HOMOLOGADO <- numericoParaTextoMoeda(dtResultados$VALOR_HOMOLOGADO)
  dtResultados <- unique(dtResultados)

  dtResultados$PROB_FAVORECIMENTO_NO_MERCADO <- 0
  for(m in names(e$lsPageRanks)){
    vecPageRank <- eval(expr = parse(text = paste0("e$lsPageRanks$`",m,"`")))
    for(pr in 1:length(vecPageRank)){
      dtResultados[MERCADO_ATUACAO == m & CNPJ == names(vecPageRank)[pr]]$PROB_FAVORECIMENTO_NO_MERCADO <- vecPageRank[pr]
    }
  }

  obj <- list(
    mercados.tabela = dtResultados,
    mercados.grafos = e$grMercadosRisco
  )

  class(obj) <- "TipologiaRodizio"

  return(obj)
}

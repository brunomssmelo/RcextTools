#' Metodo S3 que imprime na tela um data.frame que representa um objeto da classe `TipologiaRodizio`
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' casosSuspeitos <- rodizioIdentificaSituacoesSuspeitas(dados)
#' print(casosSuspeitos)
#' }
#' @export
print.TipologiaRodizio <- function (x){
  print(x$mercados.tabela)
}

Relação dos dados disponibilizados juntamente com o pacote RcextTools
---------------------------------------------------------------------

Juntamente com o pacote, são fornecidos dados de licitações ocorridas entre 2011 e 2015. Tais dados tiveram alguns campos "embaralhados" de maneira que dados que pudessem identificar os órgaos e empresas participantes não fossem revelados:

contratos Dados de contratos relativos a base de licitacoes realizadas por orgaos de um ente federativo brasileiro no periodo de 2011 a 2015 licitacoes Dados de licitacoes realizadas por orgaos de um ente federativo brasileiro no periodo de 2011 a 2015 part\_lic Dados relatvos aos participantes de licitacoes realizadas por orgaos de um ente federativo brasileiro no periodo de 2011 a 2015

O código abaixo, quando executado, exibirá uma breve descrição das bases de dados forncidas juntamente com o pacote `RcextTools`.

``` r
data(package = 'RcextTools')
```

Antes de utilizar o pacote, é preciso criar um `data.frame` de acordo com o *layout* esperado.
----------------------------------------------------------------------------------------------

``` r
# carrega o pacote RcextTools
library(RcextTools)
```

    ## Loading required package: data.table

``` r
# veja o layout esperado assim: ?rcextRiscoAcaoColusiva

# carrega dados de licitacoes da base fornecida pelo pacote RcextTools
data("part_lic")

dtDados <- part_lic[!is.na(part_lic$COD_LICITACAO),]

dtDados <- data.frame(
  CNPJ = dtDados$CNPJCPF_FORNECEDORES,
  ID_LICITACAO = dtDados$COD_LICITACAO,
  ID_ITEM = dtDados$ID_ITEM,
  VENCEDOR = ifelse(dtDados$VENCEDOR == 'S', T, F),
  VALOR_ESTIMADO = NA,
  VALOR_HOMOLOGADO = as.numeric(dtDados$VALOR_FINAL),
  DESC_OBJETO = dtDados$RESUMO_OBJETO,
  stringsAsFactors = F
)

casosSuspeitos <- rodizioIdentificaSituacoesSuspeitas(dtDados)
```

    ## Loading required package: tcltk

Exibição dos casos suspeitos
----------------------------

``` r
resultados <- casosSuspeitos$dtResultados
resultados <- unique(within(resultados, rm(list = c('VALOR_ESTIMADO', 'VENCEDOR', 'VALOR_HOMOLOGADO', 'ID_ITEM'))))
names(resultados)[names(resultados)=="TEXTO_VALOR_HOMOLOGADO"] <- "VALOR"

knitr::kable(resultados)
```

| CNPJ           |  ID\_LICITACAO| DESC\_OBJETO                             |  MERCADO\_ATUACAO| VALOR           |
|:---------------|--------------:|:-----------------------------------------|-----------------:|:----------------|
| 12003015111100 |          13030| LOCACAO DE SISTEMA PRIVADO DE TELEFONIA  |                13| R$137.799,00    |
| 12003015111100 |           9390| LOCACAO DE CENTRAL TELEFONICA CPCT       |                13| R$82.440,00     |
| 55103058201910 |           4846| INEA CONT. EMP.VIGILANCIA ELETRONICA     |                13| R$78.000,00     |
| 12003015111100 |          12178| LOCACAO DE 1 CENTRAL TELEFONICA PABX     |                13| R$74.189,00     |
| 26100001801990 |          10968| DPGE-RJ -PREST.SERV.MONIT. ELETRONICO    |                13| R$47.790,00     |
| 26100001801990 |          12571| PREST. SERV. MANUTENCAO SISTEMA INCENDIO |                13| R$30.000,00     |
| 55103058201910 |           4919| UERJ - INSTALACAO DE CAMERAS TEATRO      |                13| R$25.000,00     |
| 55103058201910 |           2963| FUNARJ-MONITORAMENTO SISTEMA DE ALARME   |                13| R$18.198,00     |
| 55103058201910 |          10663| MANUTENCAO DE REDE DE TELEFONIA E RAMAIS |                13| R$15.600,00     |
| 91606000721270 |           1942| AQUISICAO DE CENTRAIS TELEFONICAS (PABX) |                13| R$13.867,00     |
| 55103058201910 |           7146| FUNARJ-MONITORAMENTO SISTEMAS DE ALARME  |                13| R$13.500,00     |
| 55103058201910 |           3884| SERV. DE MANUTENCAO DE TELEFONIA INTERNA |                13| R$10.800,00     |
| 91606000721270 |           1942| AQUISICAO DE CENTRAIS TELEFONICAS (PABX) |                13| R$10.633,00     |
| 26100001801990 |           7345| SERVICO DE CIRCUITO FECHADO DE TV ? CFTV |                13| R$8.813,00      |
| 55103058201910 |           4427| PCERJ-MANUT CORRETIVA E PREVENTIVA CFTV  |                13| R$7.800,00      |
| 35200045701760 |            173| AQUISICAO DE CENTRAIS TELEFONICAS        |                13| R$5.691,00      |
| 91606000721270 |           1942| AQUISICAO DE CENTRAIS TELEFONICAS (PABX) |                13| R$3.990,00      |
| 82404031701440 |          10007| SEEDUC - RP AQUIS.MOBIL. ESTANTE DE ACO  |                34| R$6.112.757,54  |
| 82404031701440 |          10452| SES - AQUISICAO DE MOBILIARIO            |                34| R$3.690.000,00  |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$3.367.570,80  |
| 97206059062380 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$2.438.524,80  |
| 82404031701440 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$1.630.397,90  |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$1.356.614,57  |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$1.347.028,32  |
| 76302069101110 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$1.326.859,35  |
| 76302069101110 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$1.287.407,48  |
| 12808025701220 |          10245| AQUISICAO COBERTORES, LENCOIS E TOALHA   |                34| R$1.228.500,00  |
| 97206059062380 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$1.209.750,00  |
| 82404031701440 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$1.190.057,60  |
| 82404031701440 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$1.168.130,01  |
| 97206059062380 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$1.163.720,16  |
| 97206059062380 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$1.092.000,00  |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$1.017.455,43  |
| 97206059062380 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$975.469,60    |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$949.619,40    |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$915.000,00    |
| 12808025701220 |           9160| PMERJ - AQ. DE ARMARIO/BELICHECOLCHOES   |                34| R$889.240,00    |
| 97206059062380 |           8210| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$844.996,50    |
| 27808095501380 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$805.150,95    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$783.360,00    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$753.840,00    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$718.080,00    |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$697.495,13    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$691.020,00    |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$673.514,16    |
| 76302069101110 |           6303| AQUISICAO DE UNIFORMES SES               |                34| R$673.000,00    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$652.800,00    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$628.200,00    |
| 97206059062380 |          10423| CBMERJ - RP PARA AQ DE SACO PARA CADAVER |                34| R$622.000,00    |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$604.655,99    |
| 97206059062380 |           9937| CBMERJ - RP P AQ DE AGASALHOS GV - GMAR  |                34| R$600.000,00    |
| 12808025701220 |          11154| SES - AQ. DE MATERIAL MEDICO HOSPITALAR  |                34| R$592.470,00    |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$589.140,00    |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$542.641,23    |
| 97206059062380 |          10423| CBMERJ - RP PARA AQ DE SACO PARA CADAVER |                34| R$528.000,00    |
| 97206059062380 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$487.734,80    |
| 82404031701440 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$476.069,00    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$456.960,00    |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$449.557,80    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$439.740,00    |
| 28206031311130 |           7505| SEAP- AQ. DE BERMUDAS E CALCAS           |                34| R$435.180,80    |
| 12808025701220 |          11154| SES - AQ. DE MATERIAL MEDICO HOSPITALAR  |                34| R$429.800,00    |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$427.440,00    |
| 97206059062380 |          11702| CBMERJ - ROUPA DE CAMA                   |                34| R$414.000,00    |
| 82404031701440 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$407.022,50    |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$403.103,33    |
| 82404031701440 |           8264| COBERTOR E COLCHONETE                    |                34| R$399.500,00    |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$392.760,00    |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$356.200,00    |
| 82404031701440 |          12699| SES- AQUISICAO DE MATERIAL               |                34| R$350.400,00    |
| 82404031701440 |           8264| COBERTOR E COLCHONETE                    |                34| R$348.771,00    |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$339.135,15    |
| 76302069101110 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$331.169,52    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$326.400,00    |
| 28206031311130 |           7505| SEAP- AQ. DE BERMUDAS E CALCAS           |                34| R$326.385,60    |
| 01302088411940 |           4196| INEA CONT.EMP ESPEC.EM CONFECCAO BRINDE  |                34| R$321.300,00    |
| 76302069101110 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$321.149,18    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$314.100,00    |
| 12808025701220 |          14174| AQ. DE CAMISAS VERDES E CALCAS SOB SRP   |                34| R$310.374,00    |
| 82404031701440 |          11038| SES - AQ. DE MATERIAL PERMANENTE         |                34| R$304.000,00    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$293.760,00    |
| 82404031701440 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$291.515,15    |
| 97206059062380 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$290.434,86    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$282.690,00    |
| 12808025701220 |          14174| AQ. DE CAMISAS VERDES E CALCAS SOB SRP   |                34| R$276.920,00    |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$274.500,00    |
| 97206059062380 |          11702| CBMERJ - ROUPA DE CAMA                   |                34| R$270.000,00    |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$268.734,23    |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$261.840,00    |
| 12808025701220 |          11154| SES - AQ. DE MATERIAL MEDICO HOSPITALAR  |                34| R$255.144,00    |
| 66300072831660 |           2422| AQUISICAO DE MACACAO E GORRO AZUL (RP)   |                34| R$250.000,00    |
| 82404031701440 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$238.072,80    |
| 82404031701440 |          11748| FRALDAS GERIATRICAS E ABSORVENTES        |                34| R$223.564,80    |
| 12808025701220 |          10244| SEAP /RJ AQUISICAO DE COLCHONETES        |                34| R$222.180,00    |
| 66300072831660 |           4271| AQUISICAO DE UNIFORMES PARA CORE/PCERJ   |                34| R$211.296,00    |
| 27808095501380 |          10054| SEEDUC-PESRP 011/13-CAMISETAS ESCOLARES  |                34| R$200.865,80    |
| 97206059062380 |          11702| CBMERJ - ROUPA DE CAMA                   |                34| R$198.000,00    |
| 27808095501380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$190.366,39    |
| 82605076001220 |          14174| AQ. DE CAMISAS VERDES E CALCAS SOB SRP   |                34| R$189.916,00    |
| 82605076001220 |          14174| AQ. DE CAMISAS VERDES E CALCAS SOB SRP   |                34| R$187.512,00    |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$183.000,00    |
| 82404031701440 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$180.469,60    |
| 97206059062380 |          11702| CBMERJ - ROUPA DE CAMA                   |                34| R$175.500,00    |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$175.058,40    |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$164.600,00    |
| 12808025701220 |          10430| UERJ-AQUISICAO DE MATERIAL HOSPITALAR    |                34| R$163.200,00    |
| 12808025701220 |          14174| AQ. DE CAMISAS VERDES E CALCAS SOB SRP   |                34| R$159.795,00    |
| 01508034001180 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$146.502,75    |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$135.000,00    |
| 76302069101110 |           6303| AQUISICAO DE UNIFORMES SES               |                34| R$134.990,00    |
| 82404031701440 |           8265| FILTRO DE BARRO                          |                34| R$134.985,76    |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$134.363,14    |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$131.625,00    |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$130.920,00    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$130.560,00    |
| 97206059062380 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$127.601,34    |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$125.640,00    |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$122.250,00    |
| 97206059062380 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$120.975,00    |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$120.000,00    |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$118.150,00    |
| 97206059062380 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$116.460,00    |
| 66700006801210 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$116.250,00    |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$110.278,53    |
| 28206031311130 |           7505| SEAP- AQ. DE BERMUDAS E CALCAS           |                34| R$108.795,20    |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$108.000,00    |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$107.583,51    |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$107.452,59    |
| 76302069101110 |           9533| DEGASE- AQ. UNIFORME AGENTES SOCIOEDUCAT |                34| R$107.120,00    |
| 82404031701440 |          12079| FILTRO DE BARRO                          |                34| R$105.960,00    |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$100.769,87    |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$98.190,00     |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$97.920,00     |
| 97206059062380 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$97.050,00     |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$94.230,00     |
| 66300072831660 |           4271| AQUISICAO DE UNIFORMES PARA CORE/PCERJ   |                34| R$93.792,00     |
| 66300072831660 |           3949| RP - AQUISICAO DE UNIFORMES - DGS        |                34| R$93.014,70     |
| 27808095501380 |          14174| AQ. DE CAMISAS VERDES E CALCAS SOB SRP   |                34| R$90.751,50     |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$89.000,00     |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$88.500,00     |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$87.000,00     |
| 66300072831660 |           3949| RP - AQUISICAO DE UNIFORMES - DGS        |                34| R$84.946,35     |
| 01508034001180 |           7094| CBMERJ - AQUISICAO DE GANCHO TIPO CROQUE |                34| R$84.000,00     |
| 01508034001180 |           7021| CBMERJ-MATERIAL DE SALVAMENTO EM ALTURAS |                34| R$83.640,00     |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$83.400,00     |
| 97206059062380 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$82.900,00     |
| 97206059062380 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$81.900,00     |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$81.825,00     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$81.386,80     |
| 66300072831660 |           6890| CBMERJ-LUVAS, MASCARAS, OCULOS, M.COSTAL |                34| R$80.885,70     |
| 76302069101110 |           9533| DEGASE- AQ. UNIFORME AGENTES SOCIOEDUCAT |                34| R$80.340,00     |
| 97206059062380 |          11702| CBMERJ - ROUPA DE CAMA                   |                34| R$78.000,00     |
| 97206059062380 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$77.640,00     |
| 12808025701220 |           8341| AQUISICAO OLEADO CAMA HOSPITALAR         |                34| R$77.415,00     |
| 66300072831660 |           2467| AQUISICAO DE BARRETINAS                  |                34| R$77.000,00     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$75.000,00     |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$74.629,64     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$73.000,00     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$72.500,00     |
| 82404031701440 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$72.233,80     |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$71.240,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$71.100,00     |
| 01508034001180 |           9936| CBMERJ - AQ. GORRO, SUNGA E SHORT - GMAR |                34| R$71.092,55     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$70.023,36     |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$69.500,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$69.000,00     |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$67.178,59     |
| 27808095501380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$66.785,48     |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$65.840,00     |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$65.460,00     |
| 01508034001180 |           7000| CBMERJ-AQUISICAO DE ESPADINS             |                34| R$65.040,00     |
| 01508034001180 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$61.244,76     |
| 27808095501380 |          14174| AQ. DE CAMISAS VERDES E CALCAS SOB SRP   |                34| R$60.601,50     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$60.600,00     |
| 01508034001180 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$57.237,12     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$57.124,32     |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$55.600,00     |
| 40302050001160 |           2148| AQUISICAO DE VESTUARIO                   |                34| R$55.138,15     |
| 01302088411940 |           6589| DEGASE - AQUISICAO DE UNIFORMES          |                34| R$54.578,13     |
| 28206031311130 |           7505| SEAP- AQ. DE BERMUDAS E CALCAS           |                34| R$54.397,60     |
| 27808095501380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$54.366,85     |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$54.246,63     |
| 01302088411940 |           4457| INEA SERV.DE CONFECCAO DE UNIFORMES      |                34| R$50.543,20     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$50.137,34     |
| 76302069101110 |           9533| DEGASE- AQ. UNIFORME AGENTES SOCIOEDUCAT |                34| R$49.662,00     |
| 66300072831660 |           2422| AQUISICAO DE MACACAO E GORRO AZUL (RP)   |                34| R$49.500,00     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$49.500,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$47.400,00     |
| 12808025701220 |           9798| HUPE - COBERTOR                          |                34| R$46.225,00     |
| 01508034001180 |           9936| CBMERJ - AQ. GORRO, SUNGA E SHORT - GMAR |                34| R$43.600,20     |
| 12808025701220 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$43.440,00     |
| 01302088411940 |           5967| FUNARJ-CONFECCAO DE UNIFORMES            |                34| R$42.979,55     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$40.000,00     |
| 76302069101110 |           9533| DEGASE- AQ. UNIFORME AGENTES SOCIOEDUCAT |                34| R$38.953,38     |
| 12808025701220 |          12187| UERJ-AQUISICAO DE CAMPOS CIRURGICOS      |                34| R$37.890,00     |
| 82404031701440 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$36.155,20     |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$36.000,00     |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$35.964,00     |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$35.620,00     |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$35.280,00     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$35.011,68     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$35.000,00     |
| 82404031701440 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$34.929,60     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$34.500,00     |
| 82605076001220 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$33.584,33     |
| 76302069101110 |           8208| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$33.269,08     |
| 45107017163401 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$32.920,00     |
| 97206059062380 |           8206| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$32.730,00     |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$32.640,00     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$32.554,72     |
| 01302088411940 |           6589| DEGASE - AQUISICAO DE UNIFORMES          |                34| R$32.478,37     |
| 82404031701440 |          13866| FIA /AQUISICAO CAMISAS POLO              |                34| R$32.320,00     |
| 82404031701440 |          11190| AQUISICAO DE CAMISAS POLO                |                34| R$32.313,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$31.600,00     |
| 66300072831660 |           8211| PMERJ - AQUISICAO DE UNIFORMES           |                34| R$31.410,00     |
| 01508034001180 |           9936| CBMERJ - AQ. GORRO, SUNGA E SHORT - GMAR |                34| R$31.052,69     |
| 12808025701220 |           6101| HUPE - AQUISICAO DE TECIDO HUPE          |                34| R$30.150,00     |
| 97206059062380 |          11702| CBMERJ - ROUPA DE CAMA                   |                34| R$30.000,00     |
| 12808025701220 |          10747| UERJ - OLEADO CAMA HOSPITALAR            |                34| R$29.340,00     |
| 01302088411940 |           6589| DEGASE - AQUISICAO DE UNIFORMES          |                34| R$29.101,23     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$28.562,16     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$28.000,00     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$28.000,00     |
| 01302088411940 |           3949| RP - AQUISICAO DE UNIFORMES - DGS        |                34| R$27.870,00     |
| 12808025701220 |          10419| UERJ-AQUISICAO DE CAPOTES CIRURGICOS     |                34| R$27.840,00     |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$27.800,00     |
| 76302069101110 |           9533| DEGASE- AQ. UNIFORME AGENTES SOCIOEDUCAT |                34| R$26.780,00     |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$26.694,00     |
| 66700006801210 |          10247| AQUISICAO DE TENIS E CHINELOS            |                34| R$26.547,50     |
| 12808025701220 |          12396| UERJ - VESTUARIO HOSPITALAR, CONJ CIRURG |                34| R$25.912,00     |
| 12808025701220 |          13198| UERJ-AQUIS MAT HOSPITALAR                |                34| R$25.592,00     |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$25.294,90     |
| 12808025701220 |          12396| UERJ - VESTUARIO HOSPITALAR, CONJ CIRURG |                34| R$25.192,00     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$25.107,06     |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$25.080,00     |
| 12808025701220 |          10473| HUPE - CONJUNTOS CIRURGICOS              |                34| R$24.448,00     |
| 82404031701440 |          13866| FIA /AQUISICAO CAMISAS POLO              |                34| R$24.240,00     |
| 66300072831660 |           4271| AQUISICAO DE UNIFORMES PARA CORE/PCERJ   |                34| R$24.000,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$23.700,00     |
| 82404031701440 |          11748| FRALDAS GERIATRICAS E ABSORVENTES        |                34| R$23.616,00     |
| 12808025701220 |          10244| SEAP /RJ AQUISICAO DE COLCHONETES        |                34| R$22.500,00     |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$22.118,40     |
| 12808025701220 |          10429| UERJ-AQUISICAO DE TOLHA E TRAVESSEIRO    |                34| R$21.225,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$21.000,00     |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$20.850,00     |
| 12808025701220 |           6101| HUPE - AQUISICAO DE TECIDO HUPE          |                34| R$20.100,00     |
| 76302069101110 |           9533| DEGASE- AQ. UNIFORME AGENTES SOCIOEDUCAT |                34| R$19.476,19     |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$19.440,00     |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$18.680,40     |
| 40302050001160 |           2148| AQUISICAO DE VESTUARIO                   |                34| R$18.379,06     |
| 40302050001160 |           2148| AQUISICAO DE VESTUARIO                   |                34| R$18.264,53     |
| 40302050001160 |           2148| AQUISICAO DE VESTUARIO                   |                34| R$18.072,35     |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$18.000,00     |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$18.000,00     |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$17.880,00     |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$17.820,00     |
| 01302088411940 |           6589| DEGASE - AQUISICAO DE UNIFORMES          |                34| R$17.809,50     |
| 01508034001180 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$17.632,80     |
| 28206031311130 |           7505| SEAP- AQ. DE BERMUDAS E CALCAS           |                34| R$17.587,20     |
| 12808025701220 |          10429| UERJ-AQUISICAO DE TOLHA E TRAVESSEIRO    |                34| R$17.360,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$17.250,00     |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$16.614,00     |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$16.416,00     |
| 76302069101110 |           8912| SES - AQUISICAO DE UNIFORMES             |                34| R$16.277,36     |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$16.265,28     |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$16.160,00     |
| 82404031701440 |          11190| AQUISICAO DE CAMISAS POLO                |                34| R$16.152,10     |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$15.985,00     |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$15.895,06     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$15.800,00     |
| 12808025701220 |          10747| UERJ - OLEADO CAMA HOSPITALAR            |                34| R$15.738,00     |
| 12808025701220 |           7722| HUPE - AQUIS CAPOTES ETC.                |                34| R$15.625,00     |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$15.470,00     |
| 12808025701220 |           7722| HUPE - AQUIS CAPOTES ETC.                |                34| R$15.290,00     |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$15.200,00     |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$14.760,00     |
| 12808025701220 |          10430| UERJ-AQUISICAO DE MATERIAL HOSPITALAR    |                34| R$14.500,00     |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$14.400,00     |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$14.400,00     |
| 12808025701220 |          13943| UERJ-AQUISICAO DE CAPA BIOMBO            |                34| R$14.340,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$14.220,00     |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$14.100,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$14.000,00     |
| 82404031701440 |          11748| FRALDAS GERIATRICAS E ABSORVENTES        |                34| R$14.000,00     |
| 12808025701220 |           7722| HUPE - AQUIS CAPOTES ETC.                |                34| R$13.980,00     |
| 12808025701220 |          10419| UERJ-AQUISICAO DE CAPOTES CIRURGICOS     |                34| R$13.910,00     |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$13.599,00     |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$13.391,00     |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$12.889,50     |
| 12808025701220 |            145| AQUISICAO DE COLCHOES E CAPAS.           |                34| R$12.804,01     |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$12.510,00     |
| 97206059062380 |           6804| R/P PARA AQ. DE UNIFORMES 3OG, 3OF E 4A  |                34| R$12.435,00     |
| 12808025701220 |           7722| HUPE - AQUIS CAPOTES ETC.                |                34| R$12.232,00     |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$11.959,76     |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$11.700,00     |
| 12808025701220 |          12396| UERJ - VESTUARIO HOSPITALAR, CONJ CIRURG |                34| R$11.596,00     |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$11.580,00     |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$11.458,80     |
| 12808025701220 |           6101| HUPE - AQUISICAO DE TECIDO HUPE          |                34| R$10.990,00     |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$10.950,00     |
| 12808025701220 |          12807| UERJ-AQUIS LENCOL HOSPITALAR             |                34| R$10.890,00     |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$10.800,00     |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$10.740,00     |
| 76302069101110 |           9533| DEGASE- AQ. UNIFORME AGENTES SOCIOEDUCAT |                34| R$10.712,00     |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$10.584,39     |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$10.509,60     |
| 66300072831660 |           4271| AQUISICAO DE UNIFORMES PARA CORE/PCERJ   |                34| R$10.500,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$10.500,00     |
| 12808025701220 |          10430| UERJ-AQUISICAO DE MATERIAL HOSPITALAR    |                34| R$10.500,00     |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$10.350,00     |
| 12808025701220 |           6101| HUPE - AQUISICAO DE TECIDO HUPE          |                34| R$10.000,00     |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$9.825,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$9.760,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$9.720,00      |
| 82404031701440 |          11190| AQUISICAO DE CAMISAS POLO                |                34| R$9.690,06      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$9.535,00      |
| 12808025701220 |           9449| SESEG SRP ITENS COZINHA, CAMA, MESA CICC |                34| R$9.248,00      |
| 12808025701220 |           7722| HUPE - AQUIS CAPOTES ETC.                |                34| R$9.174,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$9.000,00      |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$9.000,00      |
| 12808025701220 |          14624| UERJ-AQUIS CAMPO CIRURGICO               |                34| R$9.000,00      |
| 12808025701220 |           7747| HUPE - CAMISOLAS, LENCOL, ETC.           |                34| R$8.875,00      |
| 12808025701220 |            145| AQUISICAO DE COLCHOES E CAPAS.           |                34| R$8.530,02      |
| 12808025701220 |           7747| HUPE - CAMISOLAS, LENCOL, ETC.           |                34| R$8.520,00      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$8.249,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$8.192,44      |
| 12808025701220 |          13943| UERJ-AQUISICAO DE CAPA BIOMBO            |                34| R$8.032,50      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$7.920,00      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$7.640,00      |
| 12808025701220 |          10473| HUPE - CONJUNTOS CIRURGICOS              |                34| R$7.640,00      |
| 12808025701220 |          10419| UERJ-AQUISICAO DE CAPOTES CIRURGICOS     |                34| R$7.500,00      |
| 12808025701220 |          10419| UERJ-AQUISICAO DE CAPOTES CIRURGICOS     |                34| R$7.497,50      |
| 12808025701220 |          10430| UERJ-AQUISICAO DE MATERIAL HOSPITALAR    |                34| R$7.445,00      |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$7.347,60      |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$7.300,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$7.263,00      |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$7.200,00      |
| 01508034001180 |           7021| CBMERJ-MATERIAL DE SALVAMENTO EM ALTURAS |                34| R$7.150,00      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$7.100,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$7.020,00      |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$7.000,00      |
| 12808025701220 |          10419| UERJ-AQUISICAO DE CAPOTES CIRURGICOS     |                34| R$6.950,00      |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$6.900,00      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$6.900,00      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$6.850,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$6.741,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$6.720,00      |
| 01302088411940 |           6589| DEGASE - AQUISICAO DE UNIFORMES          |                34| R$6.668,99      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$6.643,00      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$6.561,71      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$6.501,58      |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$6.400,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$6.240,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$6.189,18      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$6.120,00      |
| 12808025701220 |          10473| HUPE - CONJUNTOS CIRURGICOS              |                34| R$6.112,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$6.009,78      |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$6.000,00      |
| 50102045971910 |          14175| AQ. DE TENIS CHINELOS E MEIAS SOB SRP    |                34| R$6.000,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$5.979,88      |
| 12808025701220 |          10430| UERJ-AQUISICAO DE MATERIAL HOSPITALAR    |                34| R$5.967,00      |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$5.714,80      |
| 01508034001180 |           6467| AQUISICAO DE LUVAS E BOTAS DE NEOPRENE   |                34| R$5.700,00      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$5.603,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$5.600,00      |
| 12808025701220 |           7722| HUPE - AQUIS CAPOTES ETC.                |                34| R$5.594,00      |
| 12808025701220 |           6101| HUPE - AQUISICAO DE TECIDO HUPE          |                34| R$5.425,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$5.400,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$5.391,00      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$5.233,50      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$5.220,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$5.040,00      |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$5.040,00      |
| 12808025701220 |           9449| SESEG SRP ITENS COZINHA, CAMA, MESA CICC |                34| R$5.000,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$5.000,00      |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$5.000,00      |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$4.865,00      |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$4.800,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$4.726,80      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$4.694,21      |
| 82404031701440 |          11748| FRALDAS GERIATRICAS E ABSORVENTES        |                34| R$4.665,60      |
| 12808025701220 |          10419| UERJ-AQUISICAO DE CAPOTES CIRURGICOS     |                34| R$4.639,50      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$4.620,00      |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$4.600,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$4.556,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$4.500,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$4.484,91      |
| 12808025701220 |          10430| UERJ-AQUISICAO DE MATERIAL HOSPITALAR    |                34| R$4.450,00      |
| 01302088411940 |           6589| DEGASE - AQUISICAO DE UNIFORMES          |                34| R$4.440,50      |
| 01508034001180 |           7021| CBMERJ-MATERIAL DE SALVAMENTO EM ALTURAS |                34| R$4.400,00      |
| 28206031311130 |           7505| SEAP- AQ. DE BERMUDAS E CALCAS           |                34| R$4.396,80      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$4.318,80      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$4.250,00      |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$4.218,07      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$4.200,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$4.200,00      |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$4.194,38      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$4.180,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$4.131,00      |
| 12808025701220 |           1667| FIA-AQUIS DE COLCHOES, CAMA TRAVESEIRO   |                34| R$4.060,80      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$4.001,46      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$4.000,00      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$3.990,00      |
| 12808025701220 |           9798| HUPE - COBERTOR                          |                34| R$3.975,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$3.974,50      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$3.939,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$3.886,92      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$3.840,00      |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$3.840,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$3.827,12      |
| 01302088411940 |           6589| DEGASE - AQUISICAO DE UNIFORMES          |                34| R$3.775,54      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$3.674,24      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$3.673,80      |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$3.660,00      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$3.625,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$3.600,00      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$3.600,00      |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$3.600,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$3.543,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$3.542,40      |
| 12808025701220 |          10473| HUPE - CONJUNTOS CIRURGICOS              |                34| R$3.505,00      |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$3.475,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$3.423,00      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$3.400,00      |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$3.347,24      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$3.307,12      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$3.298,50      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$3.285,60      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$3.259,04      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$3.247,80      |
| 82404031701440 |          11190| AQUISICAO DE CAMISAS POLO                |                34| R$3.225,10      |
| 12808025701220 |           7722| HUPE - AQUIS CAPOTES ETC.                |                34| R$3.224,00      |
| 12808025701220 |          12139| CEASA-RJ AQUISICAO DE MOBILIARIO         |                34| R$3.189,60      |
| 12808025701220 |           8024| AQUISICAO DE MAT. DE CONSUMO PARA A DAS  |                34| R$3.180,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$3.150,00      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$3.135,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$3.123,00      |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$3.121,35      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$3.121,24      |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$3.120,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$3.079,64      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$3.075,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$3.049,74      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$3.001,20      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$3.000,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$3.000,00      |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$3.000,00      |
| 12808025701220 |          11869| UERJ - CAMPO CIRURGICO                   |                34| R$3.000,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.930,14      |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$2.920,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$2.900,40      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$2.878,50      |
| 01508034001180 |           6467| AQUISICAO DE LUVAS E BOTAS DE NEOPRENE   |                34| R$2.850,00      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$2.842,50      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$2.720,00      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$2.701,00      |
| 12808025701220 |           8024| AQUISICAO DE MAT. DE CONSUMO PARA A DAS  |                34| R$2.700,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.690,95      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$2.690,59      |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$2.650,00      |
| 12808025701220 |           9449| SESEG SRP ITENS COZINHA, CAMA, MESA CICC |                34| R$2.574,50      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$2.560,00      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$2.500,00      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$2.479,79      |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$2.459,40      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$2.440,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.400,92      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$2.400,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.391,95      |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$2.370,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$2.358,90      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.332,15      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.272,36      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.187,64      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$2.187,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$2.179,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$2.163,60      |
| 12808025701220 |           7747| HUPE - CAMISOLAS, LENCOL, ETC.           |                34| R$2.130,00      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$2.125,00      |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$2.100,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.092,96      |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$2.085,00      |
| 12808025701220 |           3005| MOBILIARIO PARA A SESEG                  |                34| R$2.069,92      |
| 12808025701220 |           1690| AQUISICAO DE JALECOS E BONES             |                34| R$2.049,60      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$2.040,00      |
| 01508034001180 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$2.025,65      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$2.006,25      |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$2.000,00      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$2.000,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$1.983,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$1.980,00      |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$1.980,00      |
| 12808025701220 |           1690| AQUISICAO DE JALECOS E BONES             |                34| R$1.959,75      |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$1.953,00      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$1.925,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$1.920,00      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$1.912,50      |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$1.855,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$1.846,80      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$1.800,00      |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$1.800,00      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$1.800,00      |
| 12808025701220 |           7747| HUPE - CAMISOLAS, LENCOL, ETC.           |                34| R$1.775,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$1.771,20      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$1.760,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$1.740,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$1.720,05      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$1.704,27      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$1.680,00      |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$1.680,00      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$1.620,00      |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$1.599,16      |
| 12808025701220 |          10421| UERJ-AQUISICAO DE CAMISOLAS, ETC...      |                34| R$1.550,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$1.518,60      |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$1.518,00      |
| 12808025701220 |           6101| HUPE - AQUISICAO DE TECIDO HUPE          |                34| R$1.491,00      |
| 01508034001180 |           7119| CBMERJ - AQ UNIFORME - AG COMUNITARIO    |                34| R$1.460,67      |
| 66300072831660 |           2367| PMERJ - AQUISICAO DE PECAS DE FARDAMENTO |                34| R$1.446,99      |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$1.356,34      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$1.345,47      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$1.345,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$1.301,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$1.260,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$1.252,80      |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$1.250,00      |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$1.232,94      |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$1.219,00      |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$1.206,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$1.195,98      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$1.179,19      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$1.177,20      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$1.166,08      |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$1.104,00      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$1.080,00      |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$1.024,80      |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$1.020,00      |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$1.001,63      |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$960,00        |
| 01508034001180 |           6467| AQUISICAO DE LUVAS E BOTAS DE NEOPRENE   |                34| R$950,00        |
| 97206059062380 |          10404| CBMERJ - AQ DE UNIFORMES PARA CADETES    |                34| R$900,00        |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$876,00        |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$867,08        |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$851,02        |
| 12808025701220 |           1690| AQUISICAO DE JALECOS E BONES             |                34| R$850,00        |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$838,25        |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$822,50        |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$816,40        |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$804,90        |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$800,00        |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$780,16        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$777,87        |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$744,25        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$742,33        |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$729,60        |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$708,00        |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$702,64        |
| 40302050001160 |           9454| DEGASE - VESTUARIO ADOLESCENTES          |                34| R$700,00        |
| 01508034001180 |           7307| CBMERJ - RP TENIS PRETO DE ED. FISICA    |                34| R$695,00        |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$690,00        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$682,12        |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$677,20        |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$670,60        |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$658,80        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$623,88        |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$600,00        |
| 01302088411940 |          13288| CBMERJ - UNIFORMES PARA CADETES          |                34| R$600,00        |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$571,48        |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$571,08        |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$563,66        |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$562,80        |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$560,00        |
| 12808025701220 |           1690| AQUISICAO DE JALECOS E BONES             |                34| R$550,00        |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$525,00        |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$511,00        |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$500,00        |
| 12808025701220 |          11134| PCERJ - AQUISICAO ROUPAS HOSPITALARES    |                34| R$500,00        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$491,60        |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$489,84        |
| 12808025701220 |           8723| UERJ - MATERIAL HOSPITALAR               |                34| R$484,00        |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$480,00        |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$478,37        |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$466,60        |
| 01302088411940 |           3959| AQUISICAO DE UNIFORMES PARA CADETES      |                34| R$456,00        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$442,24        |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$434,00        |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$408,20        |
| 01508034001180 |           7581| CBMERJ - AQ. MATERIAL DE SERRALHERIA     |                34| R$403,64        |
| 12808025701220 |          10717| PCERJ-AQ ROUPAS DE CAMA E BANHO          |                34| R$400,00        |
| 12808025701220 |           1690| AQUISICAO DE JALECOS E BONES             |                34| R$399,90        |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$399,30        |
| 12808025701220 |           1667| FIA-AQUIS DE COLCHOES, CAMA TRAVESEIRO   |                34| R$398,20        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$396,83        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$392,88        |
| 01302088411940 |           7392| CBMERJ - UNIFORMES PARA OS CADETES       |                34| R$390,00        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$379,06        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$369,19        |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$341,60        |
| 12808025701220 |           1485| UNIFORMES E BOTAS DE SEGURANCA           |                34| R$341,18        |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$334,00        |
| 12808025701220 |          11565| PCERJ-AQ DE GUARDA PO                    |                34| R$330,00        |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$326,56        |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$317,26        |
| 01302088411940 |           4349| AQUISICAO DE UNIFORMES                   |                34| R$244,92        |
| 01508034001180 |           7119| CBMERJ - AQ UNIFORME - AG COMUNITARIO    |                34| R$202,60        |
| 12808025701220 |           9764| CBMERJ - AQ. DE VESTUARIO DE USO MEDICO  |                34| R$189,45        |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$158,14        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$121,42        |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$95,75         |
| 01508034001180 |           7103| AQ. DE UNIFORMES - DEFESA CIVIL          |                34| R$77,09         |
| 01302088411940 |           5879| FTMRJ - AQUISICAO DE UNIFORMES           |                34| R$50,34         |
| 45704061501700 |          11028| PATIOS E REBOQUES REGIAO METROPOLITANA   |                40| R$93.199.991,04 |
| 45704061501700 |          11029| PATIOS REBOQUES REGIAO NORTE SUL SERRANA |                40| R$19.008.892,08 |
| 11603029401680 |           2181| LOCACAO DE VEICULOS COM MOTORISTAS       |                40| R$850.549,38    |
| 11603029401680 |           9103| SERVICOS MENSAIS DE CONDUCAO DE VEICULOS |                40| R$671.280,00    |
| 82405020091260 |          13130| SEAP - PREST. SERVICO LOCACAO DE MOTO    |                40| R$474.999,60    |
| 15909080011500 |           8865| CONTR.SERV.CONDUTORES VEICULOS           |                40| R$440.999,90    |
| 15909080011500 |          14114| PREST. SERVICOS DE COPEIRAGEM E RECEPCAO |                40| R$332.292,00    |
| 15909080011500 |          11641| SEEDUC-PREST.SERV.DE RECEPCAO            |                40| R$322.617,60    |
| 45704061501700 |          12045| SERVICO DE COLETA DE LIXO EXTRAORDINARIO |                40| R$294.343,20    |
| 45704061501700 |          12632| LOCACAO DE REBOQUE COM MOTORISTA         |                40| R$230.000,00    |
| 82405020091260 |           7105| SSCS - PRESTACAO DE SERVICOS DE MOTOBOY  |                40| R$217.900,00    |
| 15909080011500 |           7189| UERJ - ENTREGA DE DOCUMENTOS             |                40| R$208.000,00    |
| 11603029401680 |           2881| LOCACAO DE VEICULOS C/ MOTORISTA         |                40| R$201.250,00    |
| 11603029401680 |           2881| LOCACAO DE VEICULOS C/ MOTORISTA         |                40| R$169.420,00    |
| 48903028611290 |          12308| SERV. DE LOC. DE VEICULOS COM MOTORISTAS |                40| R$153.000,00    |
| 11603029401680 |           4277| PREGAO ELETRONICO DE LOCACAO DE VEICULOS |                40| R$150.100,00    |
| 11603029401680 |           2181| LOCACAO DE VEICULOS COM MOTORISTAS       |                40| R$131.214,74    |
| 15909080011500 |           4939| UERJ - TERCEIRIZACAO DE MAO-DE-OBRA UERJ |                40| R$93.646,08     |
| 11603029401680 |           5022| SERVICO DE TRANSPORTE DE PROVAS DO VEST. |                40| R$76.290,00     |
| 11603029401680 |           5783| CONT EMP ESP EM ENTREGA RAPIDA - MOTOBOY |                40| R$59.496,00     |
| 11603029401680 |           4277| PREGAO ELETRONICO DE LOCACAO DE VEICULOS |                40| R$58.000,00     |
| 11603029401680 |           2948| TRANSPORTE DE PROVAS - VESTIBULAR        |                40| R$51.900,00     |
| 11603029401680 |           7904| UERJ - LOCACAO VEICULO 1.6 SEDAN         |                40| R$47.500,00     |
| 11603029401680 |           2834| PRESTACAO DE SERVICO DE LOCACAO          |                40| R$41.779,92     |
| 15909080011500 |           4939| UERJ - TERCEIRIZACAO DE MAO-DE-OBRA UERJ |                40| R$41.753,92     |
| 15909080011500 |          11053| SERVICO DE MOTOBOY                       |                40| R$38.395,50     |
| 15909080011500 |           4777| CONTRATACAO DE SERVICO DE MOTOBOY        |                40| R$24.989,99     |
| 11603029401680 |           1929| UERJ - LOCACAO DE VEICULO FENG           |                40| R$11.960,00     |
| 11603029401680 |           1929| UERJ - LOCACAO DE VEICULO FENG           |                40| R$5.292,00      |
| 60100023901290 |          10752| DIGITALIZACAO DOC. HISTORICOS TEXTUAIS   |                50| R$1.140.000,00  |
| 29806005221780 |           3296| ORGANIZACAO E PRESERVACAO DE ACERVO.     |                50| R$799.500,00    |
| 29806005221780 |           4887| CONTRATACAO DE SERVICOS DE DIGITALIZACAO |                50| R$780.000,00    |
| 88300079401760 |           6727| SEC - CAT., HIG., DIAGNOSTICO DOS MUSEUS |                50| R$772.000,00    |
| 14504005811300 |           6922| CATALOGACAO DE ACERVO                    |                50| R$747.000,00    |
| 14504005811300 |           4728| CATALOGACAO E INDEXACAO DE ACERVO        |                50| R$706.634,08    |
| 29806005221780 |            529| SERVICO DE TRATAMENTO DE ACERVO          |                50| R$438.000,00    |
| 29806005221780 |           2729| HIGIENIZACAO DIGITALIZACAO DISCOS/ FITAS |                50| R$419.000,00    |
| 29806005221780 |           8286| DIGITALIZACAO DE DOCUMENTOS HISTORICOS   |                50| R$405.000,00    |
| 88300079401760 |           4728| CATALOGACAO E INDEXACAO DE ACERVO        |                50| R$391.000,00    |
| 29806005221780 |           8286| DIGITALIZACAO DE DOCUMENTOS HISTORICOS   |                50| R$364.000,00    |
| 44600060601880 |           4728| CATALOGACAO E INDEXACAO DE ACERVO        |                50| R$354.000,00    |
| 14504005811300 |           8162| FMIS - REVISAO DE CATALOGACAO            |                50| R$300.800,00    |
| 88300079401760 |           9435| FTMRJ - SERV. DE CATALOGACAO DE DOCS.    |                50| R$291.688,52    |
| 29806005221780 |           4887| CONTRATACAO DE SERVICOS DE DIGITALIZACAO |                50| R$269.000,00    |
| 14504005811300 |           5673| GESTAO E PADRONIZACAO DOS ITENS (CATMAS) |                50| R$258.000,00    |
| 14504005811300 |           6579| CATALOGACAO E INDEXACAO DE PARTITURAS    |                50| R$248.400,00    |
| 60100023901290 |           2754| HIGIENIZACAO DIGITALIZACAO PARTITURAS    |                50| R$228.000,00    |
| 60100023901290 |           8037| FMIS - CATALOGACAO E INDEXACAO DE ACERVO |                50| R$195.000,00    |
| 88300079401760 |            556| CATALOGACAO E INDEXACAO DE ACERVOS       |                50| R$190.200,00    |
| 44600060601880 |          10370| CATALOGACAO INDEXACAO REVISAO PARTITURAS |                50| R$174.988,00    |
| 63405006101720 |           6462| CONTRATACAO DE ARQUIVISTAS               |                50| R$167.481,88    |
| 44600060601880 |          11613| CATALOGACAO E INDEXACAO DE 20700 ITENS.  |                50| R$154.000,00    |
| 14504005811300 |           5673| GESTAO E PADRONIZACAO DOS ITENS (CATMAS) |                50| R$150.000,00    |
| 60100023901290 |           9550| PRODUCAO DE BANCO DE IMAGENS             |                50| R$141.124,00    |
| 88300079401760 |           2679| CATALOGACAO INDEXACAO 4500(DISCOS VINIL) |                50| R$139.000,00    |
| 60100023901290 |           6720| DIGITALIZACAO DE PARTITURAS              |                50| R$134.700,00    |
| 60100023901290 |           8286| DIGITALIZACAO DE DOCUMENTOS HISTORICOS   |                50| R$112.900,00    |
| 63405006101720 |           8555| EMPRESA DE ARQUIVOLOGIA                  |                50| R$97.820,00     |
| 88300079401760 |           8037| FMIS - CATALOGACAO E INDEXACAO DE ACERVO |                50| R$69.900,00     |
| 88300079401760 |           1253| MANUTENCAO DE BANCO DE DADOS             |                50| R$61.989,00     |
| 60100023901290 |           8037| FMIS - CATALOGACAO E INDEXACAO DE ACERVO |                50| R$55.000,00     |
| 14504005811300 |           5989| CATALOGACAO INDEXACAO BIBIOTECA          |                50| R$49.000,00     |
| 14504005811300 |           5673| GESTAO E PADRONIZACAO DOS ITENS (CATMAS) |                50| R$37.900,00     |
| 14504005811300 |           5673| GESTAO E PADRONIZACAO DOS ITENS (CATMAS) |                50| R$27.200,00     |
| 14504005811300 |           5673| GESTAO E PADRONIZACAO DOS ITENS (CATMAS) |                50| R$12.900,00     |
| 19004033701920 |           9689| SEEDUC-RP AQUIS.MOBILIARIO (CONJ. ALUNO) |                55| R$37.988.000,00 |
| 26608073054340 |          10008| SEEDUC-RP AQUIS.MOBIL.(CONJ. REFEITORIO) |                55| R$10.540.393,50 |
| 26608073054340 |           9692| SEEDUC-RP AQUIS.MOBIL. (CONJ.PROFESSOR)  |                55| R$8.007.000,00  |
| 19004033701920 |           9694| SEEDUC-RP AQ MOBILIARIO (CJ SECRETARIA)  |                55| R$3.275.184,10  |
| 19004033701920 |           9696| SEEDUC-RP AQ.MOBILIARIO (ARQUIVO EM ACO) |                55| R$3.152.193,33  |
| 58606042601310 |           9160| PMERJ - AQ. DE ARMARIO/BELICHECOLCHOES   |                55| R$1.487.700,00  |
| 43408017001530 |           1997| MATERIAL FOTOGRAFICO E FILMAGEM          |                55| R$876.000,00    |
| 66301089601710 |          11566| AQUISICAO DE ARMARIOS E GAVETERIO        |                55| R$649.200,00    |
| 24001016601560 |          11037| SES - AQUISICAO DE MOBILIARIO            |                55| R$634.182,40    |
| 66301089601710 |          11566| AQUISICAO DE ARMARIOS E GAVETERIO        |                55| R$584.000,00    |
| 66301089601710 |          11566| AQUISICAO DE ARMARIOS E GAVETERIO        |                55| R$582.000,00    |
| 11707007221670 |           7153| MANUTENCAO DE SISTEMAS DE REFRIGERACAO   |                55| R$564.999,50    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$527.190,44    |
| 16707032211700 |           7214| AQUISICAO DE MATERIAL DE SAUDE           |                55| R$485.000,00    |
| 66301089601710 |           4814| PCERJ - AQUISICAO DE ARMARIOS            |                55| R$432.067,67    |
| 24001016601560 |          11037| SES - AQUISICAO DE MOBILIARIO            |                55| R$422.079,00    |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$414.990,00    |
| 66301089601710 |          11566| AQUISICAO DE ARMARIOS E GAVETERIO        |                55| R$408.720,00    |
| 11707007221670 |           6983| PREST SEV MANT PREVENTIVA E CORRETIVA AR |                55| R$407.371,80    |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$400.167,62    |
| 40600060201860 |          11982| CBMERJ - RP AQ. ARMARIO BELICHE COLCHAO  |                55| R$389.000,00    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$364.356,96    |
| 16707032211700 |           7214| AQUISICAO DE MATERIAL DE SAUDE           |                55| R$360.000,00    |
| 66301089601710 |          11566| AQUISICAO DE ARMARIOS E GAVETERIO        |                55| R$359.360,00    |
| 02006042701140 |           7580| CBMERJ - AQ. DE ARMARIO E BELICHE        |                55| R$330.950,00    |
| 66301089601710 |          12699| SES- AQUISICAO DE MATERIAL               |                55| R$256.800,00    |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$229.726,00    |
| 02006042701140 |           8219| SES - AQUISICAO DE MOBILIARIOS           |                55| R$220.000,00    |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$210.373,89    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$210.116,26    |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$204.997,14    |
| 45308083001170 |          12699| SES- AQUISICAO DE MATERIAL               |                55| R$200.000,00    |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$190.992,00    |
| 66301089601710 |           4814| PCERJ - AQUISICAO DE ARMARIOS            |                55| R$189.092,51    |
| 02006042701140 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$186.750,00    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$186.326,08    |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$184.050,00    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$182.664,29    |
| 58606042601310 |          11879| SEPLAG-AQUIS. MOBILIA. ARMARIO GAVETEIRO |                55| R$182.350,45    |
| 58606042601310 |          11879| SEPLAG-AQUIS. MOBILIA. ARMARIO GAVETEIRO |                55| R$172.528,06    |
| 58606042601310 |          11879| SEPLAG-AQUIS. MOBILIA. ARMARIO GAVETEIRO |                55| R$172.321,65    |
| 58606042601310 |          12975| SES - AQUISICAO DE MATERIAL              |                55| R$169.990,00    |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$162.422,37    |
| 40600060201860 |           8219| SES - AQUISICAO DE MOBILIARIOS           |                55| R$160.000,00    |
| 02006042701140 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$158.562,58    |
| 40600060201860 |           8219| SES - AQUISICAO DE MOBILIARIOS           |                55| R$155.000,00    |
| 58606042601310 |           9446| AQ. DE MOBILIARIO                        |                55| R$154.000,00    |
| 14208011011210 |           4292| MOBILIARIO TECNICO CICC                  |                55| R$147.499,80    |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$143.520,00    |
| 66301089601710 |          12699| SES- AQUISICAO DE MATERIAL               |                55| R$141.600,00    |
| 40600060201860 |           8219| SES - AQUISICAO DE MOBILIARIOS           |                55| R$140.500,00    |
| 24001016601560 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$138.040,00    |
| 58606042601310 |          11879| SEPLAG-AQUIS. MOBILIA. ARMARIO GAVETEIRO |                55| R$137.223,66    |
| 58606042601310 |          11879| SEPLAG-AQUIS. MOBILIA. ARMARIO GAVETEIRO |                55| R$135.979,65    |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$133.928,66    |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$133.232,00    |
| 02006042701140 |          11699| AQUISICAO DE GAVETEIROS DE ACO           |                55| R$130.997,82    |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$130.900,00    |
| 58606042601310 |           9446| AQ. DE MOBILIARIO                        |                55| R$127.500,00    |
| 96904000411110 |           8219| SES - AQUISICAO DE MOBILIARIOS           |                55| R$127.350,00    |
| 58606042601310 |          11879| SEPLAG-AQUIS. MOBILIA. ARMARIO GAVETEIRO |                55| R$126.493,66    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$123.102,97    |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$121.904,00    |
| 02006042701140 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$121.892,53    |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$120.750,00    |
| 02006042701140 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$118.000,00    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$113.675,38    |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$112.931,25    |
| 02006042701140 |           4893| AQUISICAO DE ESTANTES DE ACO             |                55| R$112.300,00    |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$111.033,00    |
| 02006042701140 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$110.994,00    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$105.879,46    |
| 14208011011210 |           9807| AQUISICAO DE MOBILIARIO P/ ECO TEC.      |                55| R$100.784,75    |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$99.593,40     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$99.157,01     |
| 45308083001170 |          11716| SEA - AQUISICAO DE MOBILIARIO ECOTEC     |                55| R$98.892,00     |
| 02006042701140 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$98.070,00     |
| 16707032211700 |          13910| UERJ-AQUIS MOBILIARIO ARQUIVO ETC        |                55| R$98.059,00     |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$97.920,00     |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$96.800,00     |
| 14208011011210 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$96.382,00     |
| 58606042601310 |           9446| AQ. DE MOBILIARIO                        |                55| R$90.000,00     |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$87.287,89     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$85.581,72     |
| 40600060201860 |           8219| SES - AQUISICAO DE MOBILIARIOS           |                55| R$83.500,00     |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$81.000,00     |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$80.515,42     |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$76.680,00     |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$76.000,00     |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$73.500,00     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$72.000,00     |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$69.781,23     |
| 58606042601310 |           9446| AQ. DE MOBILIARIO                        |                55| R$69.000,00     |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$68.750,00     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$68.376,32     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$65.999,50     |
| 66301089601710 |          11178| DPGE-RJ R.P. AQUI.MOBILIARIO D.DE CAXIAS |                55| R$61.246,00     |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$60.010,44     |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$59.500,00     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$58.900,00     |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$58.533,78     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$58.308,32     |
| 40600060201860 |          11787| AQUISICAO DE ESTANTES DE ACO             |                55| R$58.200,00     |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$57.000,00     |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$56.630,00     |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$55.488,00     |
| 24001016601560 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$55.335,00     |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$55.000,00     |
| 11707007221670 |           7147| SEASDH-SERV.MANUT.AR CONDICIONADO        |                55| R$54.999,98     |
| 11707007221670 |           6062| MANUTENCAO DE AR CONIDICONADO CHILLER.   |                55| R$53.900,00     |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$53.530,01     |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$53.475,00     |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$52.700,80     |
| 45308083001170 |          10576| AQUISICAO DE MOBILIARIO PARA O AUDITORIO |                55| R$51.920,00     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$49.485,50     |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$49.362,00     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$48.837,51     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$48.450,00     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$46.729,53     |
| 45308083001170 |           1665| SEOBRAS - AQUIS C/MONTAGEM DE MOBILIARIO |                55| R$46.496,58     |
| 16707032211700 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$45.996,00     |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$45.833,56     |
| 45308083001170 |           1665| SEOBRAS - AQUIS C/MONTAGEM DE MOBILIARIO |                55| R$45.471,18     |
| 45308083001170 |           9446| AQ. DE MOBILIARIO                        |                55| R$44.325,00     |
| 66301089601710 |           4814| PCERJ - AQUISICAO DE ARMARIOS            |                55| R$44.110,60     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$43.636,32     |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$43.561,00     |
| 02006042701140 |           7301| CBMERJ - AQUISICAO DE ESTANTES METALICAS |                55| R$43.560,00     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$42.340,00     |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$41.892,03     |
| 14208011011210 |          11888| PCERJ- AQUISICAO DE CADEIRAS             |                55| R$41.500,00     |
| 16707032211700 |          13910| UERJ-AQUIS MOBILIARIO ARQUIVO ETC        |                55| R$41.328,00     |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$41.169,06     |
| 14208011011210 |           1035| AQUISICAO DE CADEIRAS.                   |                55| R$41.100,00     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$41.100,00     |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$40.467,00     |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$39.615,93     |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$39.600,00     |
| 45308083001170 |          11716| SEA - AQUISICAO DE MOBILIARIO ECOTEC     |                55| R$39.556,80     |
| 02006042701140 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$38.308,17     |
| 21009090011800 |          11698| AQUISICAO DE MOBILIARIO SEDE DETRAN/RJ   |                55| R$37.944,00     |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$36.520,00     |
| 02006042701140 |           6348| SEPLAG - AQUISICAO DE ESTANTES DE ACO    |                55| R$36.498,00     |
| 16707032211700 |           9427| UERJ - AQUIS CADEIRAS UNIV., AR COND, ET |                55| R$34.650,00     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$34.176,66     |
| 77609080401760 |           1035| AQUISICAO DE CADEIRAS.                   |                55| R$33.634,00     |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$32.632,50     |
| 14208011011210 |           9807| AQUISICAO DE MOBILIARIO P/ ECO TEC.      |                55| R$32.393,12     |
| 40600060201860 |           6530| AQUISICAO DE ARMARIO TIPO VESTUARIO      |                55| R$32.100,00     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$31.500,00     |
| 02006042701140 |          11038| SES - AQ. DE MATERIAL PERMANENTE         |                55| R$31.500,00     |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$31.498,40     |
| 11707007221670 |           7197| SEASDH - SERV INST.AP. AR CONDICIONADO   |                55| R$30.993,99     |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$30.800,00     |
| 56003086011990 |          11178| DPGE-RJ R.P. AQUI.MOBILIARIO D.DE CAXIAS |                55| R$30.749,00     |
| 40600060201860 |           6374| DEGASE -AQUISICAO DE MOBILIARIO EM GERAL |                55| R$30.573,37     |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$30.500,00     |
| 43408017001530 |           7099| AQUISICOES DE 04 ESTACOES E 08 MONITORES |                55| R$30.296,00     |
| 45308083001170 |          11891| SEASDH - MATERIAL PERMANENTE SUPDH       |                55| R$30.062,38     |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$29.700,00     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$28.980,00     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$28.482,71     |
| 58602037001290 |           8279| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$28.200,00     |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$28.022,27     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$27.908,72     |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$27.338,23     |
| 58602037001290 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$26.989,20     |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$26.650,00     |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$26.500,00     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$26.104,05     |
| 77609080401760 |           1517| AQUISICAO COM INSTALACAO DE ARMARIOS     |                55| R$25.799,72     |
| 02006042701140 |           7491| AQUISICAO DE ARQUIVOS DE ACO             |                55| R$25.600,00     |
| 14208011011210 |          11888| PCERJ- AQUISICAO DE CADEIRAS             |                55| R$25.600,00     |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$25.171,75     |
| 58602037001290 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$24.685,96     |
| 24001016601560 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$24.570,00     |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$24.374,35     |
| 40600060201860 |           8219| SES - AQUISICAO DE MOBILIARIOS           |                55| R$24.300,00     |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$23.940,00     |
| 66503076101200 |          11910| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$23.931,60     |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$23.820,00     |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$23.795,64     |
| 56003086011990 |          11178| DPGE-RJ R.P. AQUI.MOBILIARIO D.DE CAXIAS |                55| R$23.688,00     |
| 77609080401760 |           5880| UERJ-AQUIS DE MESAS E AR CONDICIONADO    |                55| R$23.506,23     |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$23.195,83     |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$23.100,00     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$22.784,00     |
| 16707032211700 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$22.617,00     |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$22.434,66     |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$22.385,00     |
| 16707032211700 |          13910| UERJ-AQUIS MOBILIARIO ARQUIVO ETC        |                55| R$22.180,00     |
| 02006042701140 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$21.870,00     |
| 14208011011210 |           9807| AQUISICAO DE MOBILIARIO P/ ECO TEC.      |                55| R$21.833,67     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$21.804,51     |
| 56003086011990 |          13674| DPGE - R.P AQ DE APARELHOS TELEFONICOS   |                55| R$21.800,00     |
| 45308083001170 |           1665| SEOBRAS - AQUIS C/MONTAGEM DE MOBILIARIO |                55| R$21.512,65     |
| 21009090011800 |           8475| AQUISICAO DE MOBILIARIOS EM GERAL        |                55| R$21.483,00     |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$21.197,00     |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$20.953,83     |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$20.740,00     |
| 58602037001290 |           8279| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$20.400,00     |
| 56003086011990 |          11178| DPGE-RJ R.P. AQUI.MOBILIARIO D.DE CAXIAS |                55| R$20.207,00     |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$20.085,00     |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$20.000,00     |
| 45308083001170 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$19.999,70     |
| 45308083001170 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$19.962,70     |
| 56003086011990 |          11178| DPGE-RJ R.P. AQUI.MOBILIARIO D.DE CAXIAS |                55| R$19.892,43     |
| 43005036401540 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$19.500,00     |
| 16707032211700 |          12683| FUNARJ - AQUISICAO MOBILIARIO EMVL       |                55| R$19.351,50     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$19.318,00     |
| 16707032211700 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$18.990,00     |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$18.445,00     |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$18.352,91     |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$18.252,00     |
| 45308083001170 |            619| SEPLAG - AQUISICAO DE ARQUIVOS DE ACO    |                55| R$18.000,00     |
| 02006042701140 |           2059| UERJ-AQUIS ROUPEIROS E ARMARIOS P HUPE   |                55| R$17.700,00     |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$17.637,92     |
| 11707007221670 |            937| AQUISICAO DE AR CONDICIONADO             |                55| R$17.500,00     |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$17.299,86     |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$16.996,81     |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$16.988,13     |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$16.910,15     |
| 02006042701140 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$16.800,00     |
| 16707032211700 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$16.640,00     |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$16.620,00     |
| 11707007221670 |            627| AQUISICAO DE MATERIAL: AR CONDICIONADO   |                55| R$16.616,00     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$16.481,08     |
| 40600060201860 |          12072| PCERJ - AQ ARMARIOS DE ACO               |                55| R$16.400,00     |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$16.380,00     |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$16.226,40     |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$16.100,00     |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$16.044,60     |
| 96904000411110 |          12250| SESEG SRP AQ MAT ESCRITORIO PAPORESPONSA |                55| R$15.885,00     |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$15.859,89     |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$15.619,58     |
| 11707007221670 |           1998| UERJ\_AQUIS\_MOBILIARIO, AR COND, ETC.   |                55| R$15.600,00     |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$15.598,00     |
| 43408017001530 |           1786| MESAS PARA O QUIOSQUE                    |                55| R$15.499,99     |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$15.496,10     |
| 96904000411110 |           8204| AQUISICAO DE 12 MESAS TIPO PENINSULAR    |                55| R$15.468,00     |
| 02006042701140 |           7756| ARMARIOS VETRO-LATERAL                   |                55| R$15.295,00     |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$15.225,00     |
| 66503076101200 |            769| AQUIS. C/MONTAGEM MOBIL. P/DEL.LEGAIS    |                55| R$15.169,43     |
| 58606042601310 |           9446| AQ. DE MOBILIARIO                        |                55| R$15.000,00     |
| 96904000411110 |           2651| AQUISICAO DE ESTANTES DE ACO             |                55| R$14.997,60     |
| 77609080401760 |           2585| PCERJ-AQ LONGARINAS                      |                55| R$14.899,50     |
| 56003086011990 |          11178| DPGE-RJ R.P. AQUI.MOBILIARIO D.DE CAXIAS |                55| R$14.891,49     |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$14.859,38     |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$14.699,28     |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$14.501,88     |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$14.449,20     |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$14.424,00     |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$14.325,08     |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$14.322,66     |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$14.190,00     |
| 02006042701140 |          11017| AQUISICAO DE ARMARIOS DE ACO             |                55| R$14.000,00     |
| 16707032211700 |          11061| SEFAZ PE008/14 CAMAS E COLCHOES          |                55| R$13.975,00     |
| 56003086011990 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$13.512,12     |
| 11707007221670 |            937| AQUISICAO DE AR CONDICIONADO             |                55| R$13.500,00     |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$13.500,00     |
| 16707032211700 |          10142| AQUISICAO DE ITENS COMPLEMENTARES.       |                55| R$13.255,82     |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$13.200,00     |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$13.198,05     |
| 77609080401760 |           5880| UERJ-AQUIS DE MESAS E AR CONDICIONADO    |                55| R$13.052,46     |
| 21009090011800 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$13.050,00     |
| 43408017001530 |           7388| SEASDH - AQ. AR CONDICIONADO TIPO SPLIT  |                55| R$13.000,05     |
| 21009090011800 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$12.572,00     |
| 43005036401540 |          10517| CBMERJ - CADEIRAS PARA O GBMUS           |                55| R$12.500,00     |
| 43408017001530 |           1997| MATERIAL FOTOGRAFICO E FILMAGEM          |                55| R$12.465,00     |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$12.437,50     |
| 21009090011800 |           7358| PGE - CONFECCAO DE MOBILIARIO            |                55| R$12.300,00     |
| 77609080401760 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$12.172,00     |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$12.133,20     |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$12.117,00     |
| 16707032211700 |          11891| SEASDH - MATERIAL PERMANENTE SUPDH       |                55| R$12.034,75     |
| 16707032211700 |          14057| UENF - MOBILIARIO - REST. UNIVERSITARIO  |                55| R$12.000,00     |
| 45308083001170 |          11891| SEASDH - MATERIAL PERMANENTE SUPDH       |                55| R$11.928,88     |
| 56003086011990 |          11178| DPGE-RJ R.P. AQUI.MOBILIARIO D.DE CAXIAS |                55| R$11.826,00     |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$11.780,00     |
| 45308083001170 |           1665| SEOBRAS - AQUIS C/MONTAGEM DE MOBILIARIO |                55| R$11.733,09     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$11.704,88     |
| 40600060201860 |           6374| DEGASE -AQUISICAO DE MOBILIARIO EM GERAL |                55| R$11.450,64     |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$11.434,54     |
| 56003086011990 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$11.389,31     |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$11.361,28     |
| 96904000411110 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$11.305,00     |
| 66503076101200 |          11910| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$11.053,00     |
| 16707032211700 |           9973| AQUISICAO MOBILIARIO E ELETRODOMESTICO   |                55| R$11.040,00     |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$10.857,40     |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$10.735,00     |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$10.340,40     |
| 96904000411110 |          10334| ARMARIOS VETRO LATERAIS                  |                55| R$10.300,00     |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$10.242,90     |
| 42400015001830 |           7084| AQUISICAO DE MOBILIARIO                  |                55| R$10.120,00     |
| 58602037001290 |           8647| SEPLAG - AQUISICAO DE ARMARIOS           |                55| R$10.080,00     |
| 43408017001530 |           4267| AQ. EQ. INFORMATICA E ELETRO-ELETRONICOS |                55| R$10.000,00     |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$10.000,00     |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$9.959,40      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$9.753,50      |
| 43005036401540 |           7948| AQUISICAO DE MOBILIARIO PARA DRV         |                55| R$9.690,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$9.672,24      |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$9.670,41      |
| 45308083001170 |           9446| AQ. DE MOBILIARIO                        |                55| R$9.662,50      |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$9.630,00      |
| 16707032211700 |          12683| FUNARJ - AQUISICAO MOBILIARIO EMVL       |                55| R$9.603,46      |
| 45308083001170 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$9.285,91      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$9.114,00      |
| 96904000411110 |          12250| SESEG SRP AQ MAT ESCRITORIO PAPORESPONSA |                55| R$9.073,00      |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$9.020,72      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$8.996,00      |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$8.930,00      |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$8.899,92      |
| 96904000411110 |           3698| MOBILIARIO POLO NOVA IGUACU              |                55| R$8.809,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$8.800,00      |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$8.580,00      |
| 56003086011990 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$8.580,00      |
| 96904000411110 |           7397| AQUIS.DE MESA MODULAR E POLTRONAS FIXAS  |                55| R$8.424,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$8.320,00      |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$8.145,04      |
| 24001016601560 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$8.137,15      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$7.950,00      |
| 96904000411110 |          12250| SESEG SRP AQ MAT ESCRITORIO PAPORESPONSA |                55| R$7.914,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$7.899,50      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$7.896,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$7.872,00      |
| 11707007221670 |            559| EQUIPAMENTOS ELETRICOS (NOBREAK E GPS)   |                55| R$7.810,00      |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$7.808,46      |
| 66301089601710 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$7.800,92      |
| 16707032211700 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$7.800,00      |
| 45308083001170 |          11716| SEA - AQUISICAO DE MOBILIARIO ECOTEC     |                55| R$7.783,68      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$7.731,44      |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$7.714,95      |
| 56003086011990 |          14238| DETRAN/RJ AQUISICAO TELEFONES ANALOGICO  |                55| R$7.700,00      |
| 16707032211700 |          11061| SEFAZ PE008/14 CAMAS E COLCHOES          |                55| R$7.600,00      |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$7.540,00      |
| 16707032211700 |           9973| AQUISICAO MOBILIARIO E ELETRODOMESTICO   |                55| R$7.519,84      |
| 40600060201860 |           5846| AQUISICAO DE MOBILIARIOS DE ESCRITORIO.  |                55| R$7.500,00      |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$7.500,00      |
| 42400015001830 |           7084| AQUISICAO DE MOBILIARIO                  |                55| R$7.452,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$7.397,36      |
| 58602037001290 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$7.272,99      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$7.200,00      |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$7.162,40      |
| 77609080401760 |           1035| AQUISICAO DE CADEIRAS.                   |                55| R$7.132,50      |
| 11707007221670 |            628| AQUISICAO DE EQUIPAMENTOS ELETRICOS      |                55| R$7.094,46      |
| 40600060201860 |           8025| SEPLAG - AQUISICAO DE ARMATIRO DE ACO    |                55| R$7.040,00      |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$7.000,00      |
| 56003086011990 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$6.948,96      |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$6.930,00      |
| 02006042701140 |           1067| AQUISICAO DE ESTANTES                    |                55| R$6.914,00      |
| 16707032211700 |          12683| FUNARJ - AQUISICAO MOBILIARIO EMVL       |                55| R$6.880,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$6.846,45      |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$6.840,00      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$6.840,00      |
| 77609080401760 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$6.750,00      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$6.750,00      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$6.725,04      |
| 14208011011210 |           9807| AQUISICAO DE MOBILIARIO P/ ECO TEC.      |                55| R$6.715,17      |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$6.600,00      |
| 16707032211700 |           9973| AQUISICAO MOBILIARIO E ELETRODOMESTICO   |                55| R$6.595,20      |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$6.566,00      |
| 42400015001830 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$6.513,20      |
| 16707032211700 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$6.500,00      |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$6.444,00      |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$6.441,01      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$6.400,00      |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$6.332,20      |
| 16707032211700 |           9980| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$6.300,00      |
| 16707032211700 |           9427| UERJ - AQUIS CADEIRAS UNIV., AR COND, ET |                55| R$6.294,00      |
| 96904000411110 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$6.168,00      |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$6.095,00      |
| 45308083001170 |          11733| SEAP - AQ. DE MOBILIARIOS                |                55| R$6.069,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$6.005,40      |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$6.000,00      |
| 11707007221670 |            937| AQUISICAO DE AR CONDICIONADO             |                55| R$5.980,00      |
| 96904000411110 |           3771| MESA CAMPING E CADEIRA LEI SECA          |                55| R$5.950,29      |
| 96904000411110 |           3217| COMPRA DE MOBILIARIO                     |                55| R$5.940,00      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$5.916,00      |
| 02006042701140 |           2208| AQUISICAO E MONTAGEM DE ESTANTE DE ACO   |                55| R$5.875,00      |
| 02006042701140 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$5.872,00      |
| 21009090011800 |           7358| PGE - CONFECCAO DE MOBILIARIO            |                55| R$5.850,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$5.831,00      |
| 16707032211700 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$5.810,00      |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$5.775,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$5.764,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$5.727,00      |
| 43408017001530 |           4267| AQ. EQ. INFORMATICA E ELETRO-ELETRONICOS |                55| R$5.700,00      |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$5.674,87      |
| 16707032211700 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$5.646,63      |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$5.597,00      |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$5.596,50      |
| 45308083001170 |          11733| SEAP - AQ. DE MOBILIARIOS                |                55| R$5.542,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$5.500,00      |
| 96904000411110 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$5.472,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$5.434,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$5.379,27      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$5.376,00      |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$5.365,52      |
| 16707032211700 |          11891| SEASDH - MATERIAL PERMANENTE SUPDH       |                55| R$5.348,34      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$5.346,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$5.340,00      |
| 43408017001530 |           5529| INEA AQUIS.MESAS E CADEIRAS              |                55| R$5.338,64      |
| 45308083001170 |            159| AQUISICAO DE ARMARIO DE ACO              |                55| R$5.280,00      |
| 45308083001170 |           9455| CBMERJ - AQ. MOBILIARIO - SUOP           |                55| R$5.280,00      |
| 77609080401760 |           7725| DPGE-RJ - AQUIS.DE CADEIRAS BIBLIOTECA   |                55| R$5.247,00      |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$5.220,00      |
| 42400015001830 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$5.200,00      |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$5.200,00      |
| 96904000411110 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$5.135,00      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$5.120,00      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$5.085,20      |
| 66301089601710 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$5.040,00      |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$5.037,15      |
| 77609080401760 |           1035| AQUISICAO DE CADEIRAS.                   |                55| R$5.010,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$5.000,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$4.980,00      |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$4.959,00      |
| 02006042701140 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$4.935,35      |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$4.935,00      |
| 43005036401540 |           7948| AQUISICAO DE MOBILIARIO PARA DRV         |                55| R$4.930,00      |
| 16707032211700 |          11001| UERJ - AQUIS DE MOBILIARIO ESCOLAR       |                55| R$4.926,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$4.900,00      |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$4.900,00      |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$4.797,58      |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$4.795,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$4.793,25      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$4.644,00      |
| 45308083001170 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$4.639,53      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$4.632,18      |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$4.620,14      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$4.590,00      |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$4.536,00      |
| 16707032211700 |          14041| DETRAN/RJ AQUISICAO DE MOBILIARIO        |                55| R$4.536,00      |
| 96904000411110 |           5089| JUCERJA AQUISICAO MAT. EXPED. E PERMAN.  |                55| R$4.522,00      |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$4.521,48      |
| 40600060201860 |           7606| UERJ - MOBILIARIO P SOC/UPE              |                55| R$4.520,00      |
| 45308083001170 |           6406| AQUISICAO DE 30 CADEIRAS TIPO SECRETARIA |                55| R$4.500,00      |
| 43005036401540 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$4.500,00      |
| 45308083001170 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$4.440,00      |
| 77609080401760 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$4.428,69      |
| 45308083001170 |           9455| CBMERJ - AQ. MOBILIARIO - SUOP           |                55| R$4.416,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$4.410,00      |
| 56003086011990 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$4.350,60      |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$4.350,00      |
| 45308083001170 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$4.337,72      |
| 45308083001170 |           1665| SEOBRAS - AQUIS C/MONTAGEM DE MOBILIARIO |                55| R$4.328,93      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$4.326,90      |
| 77609080401760 |           5620| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$4.294,00      |
| 42400015001830 |           7084| AQUISICAO DE MOBILIARIO                  |                55| R$4.205,00      |
| 45308083001170 |           1665| SEOBRAS - AQUIS C/MONTAGEM DE MOBILIARIO |                55| R$4.188,97      |
| 16707032211700 |          14041| DETRAN/RJ AQUISICAO DE MOBILIARIO        |                55| R$4.140,00      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$4.120,00      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$4.080,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$4.050,00      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$4.048,20      |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$4.000,00      |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$4.000,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$4.000,00      |
| 21009090011800 |           7358| PGE - CONFECCAO DE MOBILIARIO            |                55| R$4.000,00      |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$3.964,64      |
| 43005036401540 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$3.960,00      |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$3.960,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$3.944,97      |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$3.930,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$3.920,00      |
| 24001016601560 |           5476| REGISTRO DE PRECOS MOBILIARIO 07 RISPS   |                55| R$3.917,76      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$3.910,44      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$3.904,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$3.881,62      |
| 16707032211700 |          12683| FUNARJ - AQUISICAO MOBILIARIO EMVL       |                55| R$3.880,76      |
| 96904000411110 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$3.825,00      |
| 96904000411110 |           5089| JUCERJA AQUISICAO MAT. EXPED. E PERMAN.  |                55| R$3.771,00      |
| 45308083001170 |          11716| SEA - AQUISICAO DE MOBILIARIO ECOTEC     |                55| R$3.767,52      |
| 21009090011800 |          11733| SEAP - AQ. DE MOBILIARIOS                |                55| R$3.760,00      |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$3.759,36      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$3.747,00      |
| 56003086011990 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$3.726,00      |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$3.705,00      |
| 40600060201860 |          11733| SEAP - AQ. DE MOBILIARIOS                |                55| R$3.700,00      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$3.696,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$3.690,72      |
| 45308083001170 |          11733| SEAP - AQ. DE MOBILIARIOS                |                55| R$3.688,00      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$3.680,00      |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$3.675,00      |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$3.665,72      |
| 96904000411110 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$3.664,00      |
| 11707007221670 |           3314| AQUISICAO DE MOBILIARIO                  |                55| R$3.630,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$3.600,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$3.600,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$3.600,00      |
| 45308083001170 |           9455| CBMERJ - AQ. MOBILIARIO - SUOP           |                55| R$3.600,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$3.600,00      |
| 96904000411110 |           5510| AQUSICAO DE MOBILIARIO                   |                55| R$3.599,00      |
| 96904000411110 |           5976| LOTERJ-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$3.564,00      |
| 45308083001170 |           3231| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$3.551,51      |
| 96904000411110 |          12250| SESEG SRP AQ MAT ESCRITORIO PAPORESPONSA |                55| R$3.550,00      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$3.550,00      |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$3.540,00      |
| 14208011011210 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$3.504,00      |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$3.502,84      |
| 11707007221670 |           2045| FUNARJ - AQUISICAO DE MOBILIARIO E AFINS |                55| R$3.499,86      |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$3.469,99      |
| 11707007221670 |           3314| AQUISICAO DE MOBILIARIO                  |                55| R$3.458,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$3.454,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$3.450,00      |
| 77609080401760 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$3.420,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$3.400,00      |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$3.400,00      |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$3.390,00      |
| 96904000411110 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$3.384,00      |
| 16707032211700 |           9973| AQUISICAO MOBILIARIO E ELETRODOMESTICO   |                55| R$3.382,40      |
| 96904000411110 |           5976| LOTERJ-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$3.370,00      |
| 16707032211700 |          11001| UERJ - AQUIS DE MOBILIARIO ESCOLAR       |                55| R$3.360,00      |
| 42400015001830 |          14041| DETRAN/RJ AQUISICAO DE MOBILIARIO        |                55| R$3.360,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$3.343,00      |
| 58602037001290 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$3.331,99      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$3.315,00      |
| 43408017001530 |           1997| MATERIAL FOTOGRAFICO E FILMAGEM          |                55| R$3.296,00      |
| 96904000411110 |           5976| LOTERJ-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$3.294,00      |
| 56003086011990 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$3.286,50      |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$3.262,45      |
| 45308083001170 |          10576| AQUISICAO DE MOBILIARIO PARA O AUDITORIO |                55| R$3.252,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$3.250,00      |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$3.250,00      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$3.240,00      |
| 45308083001170 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$3.240,00      |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$3.236,00      |
| 96904000411110 |          11517| CEASA-RJ AQUISICAO DE MAT. E MOBILIARIO  |                55| R$3.230,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$3.221,45      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$3.221,43      |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$3.216,00      |
| 43005036401540 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$3.204,00      |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$3.200,00      |
| 21009090011800 |           7606| UERJ - MOBILIARIO P SOC/UPE              |                55| R$3.200,00      |
| 40600060201860 |          11733| SEAP - AQ. DE MOBILIARIOS                |                55| R$3.200,00      |
| 96904000411110 |          14580| CEASA-RJ AQUISICAO DE MOBILIARIO         |                55| R$3.200,00      |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$3.199,80      |
| 96904000411110 |          12891| CEASA-RJ AQUISICAO DE MOBILIARIOS        |                55| R$3.159,00      |
| 43408017001530 |           4267| AQ. EQ. INFORMATICA E ELETRO-ELETRONICOS |                55| R$3.150,00      |
| 42400015001830 |          14041| DETRAN/RJ AQUISICAO DE MOBILIARIO        |                55| R$3.120,00      |
| 42400015001830 |           6767| AQUISICAO DE MOBILIARIO DIVISAO MEDICA   |                55| R$3.116,75      |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$3.106,00      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$3.098,43      |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$3.084,54      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$3.049,56      |
| 43408017001530 |           5529| INEA AQUIS.MESAS E CADEIRAS              |                55| R$3.047,37      |
| 77609080401760 |           5743| AQUISICAO DE MATERIAL PERMANENTE         |                55| R$3.037,50      |
| 56003086011990 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$3.020,00      |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$3.000,00      |
| 77609080401760 |            837| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$3.000,00      |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$3.000,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$3.000,00      |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$2.984,40      |
| 77609080401760 |           4911| AQUISICAO DE MOBILIARIO                  |                55| R$2.960,00      |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$2.883,78      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.880,00      |
| 43005036401540 |           8878| AQUISICAO MOBILIARIOS DIVERSOS           |                55| R$2.847,00      |
| 42400015001830 |          10205| AQUISICAO DE MOBILIARIO                  |                55| R$2.831,00      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$2.807,00      |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$2.800,00      |
| 40600060201860 |          12535| CEASA-RJ AQUISICAO DE MOBILIARIO         |                55| R$2.792,00      |
| 96904000411110 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$2.780,00      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$2.775,00      |
| 56003086011990 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$2.773,05      |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$2.760,00      |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$2.753,03      |
| 77609080401760 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$2.750,00      |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$2.725,68      |
| 40600060201860 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$2.708,00      |
| 21009090011800 |           7358| PGE - CONFECCAO DE MOBILIARIO            |                55| R$2.700,00      |
| 96904000411110 |           9980| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$2.695,00      |
| 96904000411110 |           3771| MESA CAMPING E CADEIRA LEI SECA          |                55| R$2.689,80      |
| 21009090011800 |          11733| SEAP - AQ. DE MOBILIARIOS                |                55| R$2.688,00      |
| 56003086011990 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$2.686,02      |
| 96904000411110 |           2059| UERJ-AQUIS ROUPEIROS E ARMARIOS P HUPE   |                55| R$2.680,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.650,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$2.649,90      |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$2.643,00      |
| 56003086011990 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$2.620,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$2.619,90      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$2.606,02      |
| 77609080401760 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$2.599,80      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$2.590,08      |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$2.590,00      |
| 77609080401760 |           1035| AQUISICAO DE CADEIRAS.                   |                55| R$2.582,30      |
| 77609080401760 |           7606| UERJ - MOBILIARIO P SOC/UPE              |                55| R$2.581,38      |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$2.537,58      |
| 11707007221670 |            628| AQUISICAO DE EQUIPAMENTOS ELETRICOS      |                55| R$2.517,75      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.500,00      |
| 77609080401760 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$2.499,98      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$2.464,00      |
| 40600060201860 |           5792| PCERJ-AQ ESTANTES E ARMARIOS DE ACO      |                55| R$2.463,85      |
| 77609080401760 |           1756| AQUISICAO DE MESA,ARMARIO E CADEIRA.     |                55| R$2.459,68      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$2.400,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.400,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.400,00      |
| 21009090011800 |           7358| PGE - CONFECCAO DE MOBILIARIO            |                55| R$2.400,00      |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$2.400,00      |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$2.400,00      |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$2.400,00      |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$2.400,00      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$2.400,00      |
| 43005036401540 |          12891| CEASA-RJ AQUISICAO DE MOBILIARIOS        |                55| R$2.399,70      |
| 21009090011800 |           8475| AQUISICAO DE MOBILIARIOS EM GERAL        |                55| R$2.385,00      |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$2.379,84      |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$2.371,43      |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$2.366,00      |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$2.360,00      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$2.340,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$2.333,30      |
| 40600060201860 |          13091| AQUISICAO DE MOBILIARIO                  |                55| R$2.320,00      |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$2.308,40      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.300,00      |
| 21009090011800 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$2.300,00      |
| 96904000411110 |           5743| AQUISICAO DE MATERIAL PERMANENTE         |                55| R$2.295,00      |
| 42400015001830 |           6767| AQUISICAO DE MOBILIARIO DIVISAO MEDICA   |                55| R$2.286,00      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$2.280,09      |
| 43005036401540 |           7948| AQUISICAO DE MOBILIARIO PARA DRV         |                55| R$2.280,00      |
| 96904000411110 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$2.280,00      |
| 56003086011990 |          13421| AQUISICAO DE ELETRODOMESTICOS            |                55| R$2.280,00      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$2.270,66      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$2.255,00      |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$2.250,00      |
| 77609080401760 |           1756| AQUISICAO DE MESA,ARMARIO E CADEIRA.     |                55| R$2.225,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$2.223,00      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$2.214,87      |
| 02006042701140 |           6318| FORNECIMENTO DE MOBILIARIOS COM MONTAGEM |                55| R$2.207,47      |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$2.200,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$2.200,00      |
| 96904000411110 |          11517| CEASA-RJ AQUISICAO DE MAT. E MOBILIARIO  |                55| R$2.192,00      |
| 96904000411110 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$2.180,00      |
| 56003086011990 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$2.178,00      |
| 45308083001170 |           3231| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$2.175,12      |
| 96904000411110 |            273| AQ . DE MOBILIARIO PARA CGU/SSGE/ASSEJUR |                55| R$2.174,85      |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$2.173,30      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.166,00      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$2.164,39      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.160,00      |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$2.159,00      |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$2.158,54      |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$2.151,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$2.148,00      |
| 77609080401760 |           5620| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$2.147,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$2.133,00      |
| 96904000411110 |           1892| AQUISICAO DE CADEIRAS ERGONOMICAS        |                55| R$2.100,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.100,00      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$2.093,70      |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$2.073,65      |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$2.070,00      |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$2.060,00      |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$2.055,61      |
| 66301089601710 |           4763| PCERJ - AQUISICAO BIOMBOS DIVISORIAS     |                55| R$2.036,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$2.035,00      |
| 21009090011800 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$2.023,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.000,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.000,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$2.000,00      |
| 21009090011800 |           7358| PGE - CONFECCAO DE MOBILIARIO            |                55| R$2.000,00      |
| 96904000411110 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$2.000,00      |
| 56003086011990 |          12586| AQUISICAO DE ESTOPA E PALHA DE ACO       |                55| R$2.000,00      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$1.998,00      |
| 77609080401760 |           5880| UERJ-AQUIS DE MESAS E AR CONDICIONADO    |                55| R$1.987,00      |
| 77609080401760 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$1.980,00      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$1.969,68      |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$1.963,00      |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$1.960,00      |
| 77609080401760 |           3698| MOBILIARIO POLO NOVA IGUACU              |                55| R$1.960,00      |
| 96904000411110 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$1.956,00      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$1.955,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.950,00      |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$1.950,00      |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$1.942,31      |
| 96904000411110 |           9443| CEASA-RJ AQUISICAO DE MOBILIARIO         |                55| R$1.940,00      |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$1.925,00      |
| 96904000411110 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$1.925,00      |
| 45308083001170 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$1.916,37      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$1.915,98      |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$1.915,20      |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$1.905,66      |
| 11707007221670 |            613| REFRIGERADOR FREEZER HORIZONTAL          |                55| R$1.900,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$1.896,00      |
| 96904000411110 |           3217| COMPRA DE MOBILIARIO                     |                55| R$1.890,00      |
| 96904000411110 |           2045| FUNARJ - AQUISICAO DE MOBILIARIO E AFINS |                55| R$1.887,00      |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$1.883,00      |
| 43408017001530 |           4267| AQ. EQ. INFORMATICA E ELETRO-ELETRONICOS |                55| R$1.840,00      |
| 45308083001170 |           1895| AQUISICAO DE MOBILIARIO                  |                55| R$1.831,40      |
| 96904000411110 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$1.830,00      |
| 45308083001170 |           8697| PCERJ-AQ. MOBILIARIO(MESAS,ARMARIOS,ETC) |                55| R$1.829,46      |
| 16707032211700 |          14041| DETRAN/RJ AQUISICAO DE MOBILIARIO        |                55| R$1.824,00      |
| 77609080401760 |           3698| MOBILIARIO POLO NOVA IGUACU              |                55| R$1.820,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.800,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.800,00      |
| 45308083001170 |           9455| CBMERJ - AQ. MOBILIARIO - SUOP           |                55| R$1.800,00      |
| 42400015001830 |          10205| AQUISICAO DE MOBILIARIO                  |                55| R$1.795,50      |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$1.794,86      |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.779,00      |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$1.768,00      |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$1.760,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.750,00      |
| 42400015001830 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$1.740,00      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$1.740,00      |
| 96904000411110 |           5948| AQUISICAO FRIGOBAR E APARELHOS DIVERSOS  |                55| R$1.722,00      |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$1.720,71      |
| 21009090011800 |           8475| AQUISICAO DE MOBILIARIOS EM GERAL        |                55| R$1.700,50      |
| 96904000411110 |           5976| LOTERJ-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$1.700,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.699,80      |
| 96904000411110 |           5846| AQUISICAO DE MOBILIARIOS DE ESCRITORIO.  |                55| R$1.694,00      |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$1.685,10      |
| 66301089601710 |           4814| PCERJ - AQUISICAO DE ARMARIOS            |                55| R$1.681,44      |
| 77609080401760 |           3907| AQUISICAO DE MOBILIARIO - MIRACEMA       |                55| R$1.680,00      |
| 56003086011990 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$1.677,01      |
| 16707032211700 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$1.666,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.662,40      |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$1.639,98      |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$1.634,27      |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.632,00      |
| 16707032211700 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$1.620,00      |
| 43005036401540 |           7948| AQUISICAO DE MOBILIARIO PARA DRV         |                55| R$1.617,00      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$1.600,00      |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$1.600,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.600,00      |
| 21009090011800 |           7358| PGE - CONFECCAO DE MOBILIARIO            |                55| R$1.600,00      |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$1.600,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.599,65      |
| 77609080401760 |           1035| AQUISICAO DE CADEIRAS.                   |                55| R$1.596,00      |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$1.590,00      |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$1.584,00      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$1.577,28      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.570,00      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$1.560,00      |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$1.560,00      |
| 96904000411110 |           5743| AQUISICAO DE MATERIAL PERMANENTE         |                55| R$1.546,50      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$1.542,84      |
| 16707032211700 |          14028| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$1.540,00      |
| 45308083001170 |           3231| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.533,28      |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$1.525,00      |
| 42400015001830 |          10205| AQUISICAO DE MOBILIARIO                  |                55| R$1.512,00      |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$1.512,00      |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$1.503,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.500,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.500,00      |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$1.500,00      |
| 66301089601710 |          10411| AQUISICAO DE MOBILIARIO PARA DELEGACIAS  |                55| R$1.500,00      |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$1.500,00      |
| 16707032211700 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$1.500,00      |
| 96904000411110 |            273| AQ . DE MOBILIARIO PARA CGU/SSGE/ASSEJUR |                55| R$1.499,90      |
| 21009090011800 |           8475| AQUISICAO DE MOBILIARIOS EM GERAL        |                55| R$1.499,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.496,40      |
| 40600060201860 |           5846| AQUISICAO DE MOBILIARIOS DE ESCRITORIO.  |                55| R$1.495,00      |
| 96904000411110 |           8559| UERJ - MATERIAL PERMANENTE               |                55| R$1.494,00      |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$1.480,38      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$1.478,79      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.470,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$1.467,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$1.452,00      |
| 77609080401760 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$1.450,00      |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.440,00      |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$1.440,00      |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$1.437,07      |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$1.437,00      |
| 21009090011800 |           8475| AQUISICAO DE MOBILIARIOS EM GERAL        |                55| R$1.432,50      |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$1.425,23      |
| 43005036401540 |           8878| AQUISICAO MOBILIARIOS DIVERSOS           |                55| R$1.423,00      |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$1.420,49      |
| 56003086011990 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$1.408,79      |
| 77609080401760 |            837| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$1.400,00      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$1.400,00      |
| 96904000411110 |            273| AQ . DE MOBILIARIO PARA CGU/SSGE/ASSEJUR |                55| R$1.399,92      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$1.378,07      |
| 96904000411110 |           3217| COMPRA DE MOBILIARIO                     |                55| R$1.375,80      |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$1.373,00      |
| 42400015001830 |           6767| AQUISICAO DE MOBILIARIO DIVISAO MEDICA   |                55| R$1.359,83      |
| 77609080401760 |            837| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$1.350,00      |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.350,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.350,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.350,00      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$1.346,33      |
| 96904000411110 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$1.339,56      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$1.336,21      |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$1.336,00      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$1.335,00      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$1.330,00      |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$1.329,78      |
| 77609080401760 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$1.325,00      |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$1.320,00      |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$1.314,00      |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$1.301,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.300,00      |
| 66503076101200 |          10004| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$1.300,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.296,70      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$1.281,00      |
| 42400015001830 |           7084| AQUISICAO DE MOBILIARIO                  |                55| R$1.280,00      |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.272,00      |
| 96904000411110 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$1.266,56      |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$1.265,82      |
| 40600060201860 |          13091| AQUISICAO DE MOBILIARIO                  |                55| R$1.260,00      |
| 45308083001170 |           1665| SEOBRAS - AQUIS C/MONTAGEM DE MOBILIARIO |                55| R$1.255,70      |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$1.252,41      |
| 96904000411110 |           3931| JUCERJA-AQUISICAO MATERIAL PERMANENTE    |                55| R$1.252,00      |
| 77609080401760 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$1.248,60      |
| 45308083001170 |           3231| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.248,02      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$1.236,73      |
| 96904000411110 |           5846| AQUISICAO DE MOBILIARIOS DE ESCRITORIO.  |                55| R$1.224,00      |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$1.222,00      |
| 43408017001530 |           9599| FIA-AQUISICAO TV, CAMERA, FAX E TELEFONE |                55| R$1.220,00      |
| 96904000411110 |           5846| AQUISICAO DE MOBILIARIOS DE ESCRITORIO.  |                55| R$1.218,00      |
| 43408017001530 |           8021| AQUISICAO DE ELETROELETRONICOS.          |                55| R$1.205,34      |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$1.205,15      |
| 77609080401760 |            837| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$1.200,00      |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$1.200,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.200,00      |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$1.200,00      |
| 43005036401540 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$1.199,00      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$1.193,28      |
| 58602037001290 |           1171| PCERJ - AQUISICAO DE MOBILIARIO          |                55| R$1.188,80      |
| 43408017001530 |           8021| AQUISICAO DE ELETROELETRONICOS.          |                55| R$1.187,48      |
| 56003086011990 |          11698| AQUISICAO DE MOBILIARIO SEDE DETRAN/RJ   |                55| R$1.185,00      |
| 16707032211700 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$1.180,00      |
| 96904000411110 |           3217| COMPRA DE MOBILIARIO                     |                55| R$1.178,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$1.175,55      |
| 96904000411110 |           7899| SEA - AQUISICAO DE MOBILIARIOS           |                55| R$1.172,00      |
| 77609080401760 |           5620| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$1.170,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.166,65      |
| 40600060201860 |          13091| AQUISICAO DE MOBILIARIO                  |                55| R$1.163,88      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$1.158,83      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$1.158,81      |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$1.150,00      |
| 43005036401540 |           7948| AQUISICAO DE MOBILIARIO PARA DRV         |                55| R$1.140,00      |
| 96904000411110 |           5089| JUCERJA AQUISICAO MAT. EXPED. E PERMAN.  |                55| R$1.138,00      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$1.137,60      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$1.129,98      |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$1.128,00      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$1.122,84      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$1.121,00      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$1.120,00      |
| 96904000411110 |            273| AQ . DE MOBILIARIO PARA CGU/SSGE/ASSEJUR |                55| R$1.119,96      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.114,00      |
| 96904000411110 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$1.110,32      |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$1.110,00      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$1.108,80      |
| 56003086011990 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$1.108,40      |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$1.100,00      |
| 42400015001830 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$1.100,00      |
| 77609080401760 |           4911| AQUISICAO DE MOBILIARIO                  |                55| R$1.089,00      |
| 45308083001170 |           1325| AQUISICAO DE MOBILIARIO                  |                55| R$1.088,00      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.082,00      |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$1.081,41      |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$1.079,30      |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$1.074,74      |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$1.073,31      |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$1.062,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.050,00      |
| 96904000411110 |           7899| SEA - AQUISICAO DE MOBILIARIOS           |                55| R$1.050,00      |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.049,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.040,00      |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$1.025,36      |
| 96904000411110 |           5976| LOTERJ-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$1.020,00      |
| 96904000411110 |           3217| COMPRA DE MOBILIARIO                     |                55| R$1.018,00      |
| 14208011011210 |           4684| MOBILIARIO DPGE SAO GONCALO              |                55| R$1.014,53      |
| 16707032211700 |          12683| FUNARJ - AQUISICAO MOBILIARIO EMVL       |                55| R$1.008,80      |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$1.000,00      |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.000,00      |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$1.000,00      |
| 21009090011800 |           7455| SEASDH - AQUISICAO MATERIAL PERMANENTE   |                55| R$1.000,00      |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$990,72        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$981,86        |
| 16707032211700 |          14028| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$980,00        |
| 56003086011990 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$977,00        |
| 96904000411110 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$960,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$960,00        |
| 45308083001170 |           8560| DPGE-RJ-AQUI.DE MOBILIARIO (CONVENIO)    |                55| R$960,00        |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$958,41        |
| 96904000411110 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$957,00        |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$950,00        |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$945,90        |
| 77609080401760 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$929,99        |
| 56003086011990 |          10710| MATERIAL DE LIMPEZA                      |                55| R$916,00        |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$914,29        |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$910,00        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$906,66        |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$900,00        |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$900,00        |
| 96904000411110 |           9443| CEASA-RJ AQUISICAO DE MOBILIARIO         |                55| R$900,00        |
| 56003086011990 |          13421| AQUISICAO DE ELETRODOMESTICOS            |                55| R$895,33        |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$875,48        |
| 16707032211700 |          12683| FUNARJ - AQUISICAO MOBILIARIO EMVL       |                55| R$873,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$870,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$870,00        |
| 96904000411110 |           5089| JUCERJA AQUISICAO MAT. EXPED. E PERMAN.  |                55| R$869,00        |
| 43408017001530 |           8021| AQUISICAO DE ELETROELETRONICOS.          |                55| R$859,98        |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$858,00        |
| 43005036401540 |           7948| AQUISICAO DE MOBILIARIO PARA DRV         |                55| R$850,00        |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$848,00        |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$840,00        |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$840,00        |
| 56003086011990 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$840,00        |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$837,15        |
| 77609080401760 |           7606| UERJ - MOBILIARIO P SOC/UPE              |                55| R$835,00        |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$829,64        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$829,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$820,75        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$819,00        |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$818,00        |
| 16707032211700 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$814,00        |
| 58602037001290 |            714| AQUISICAO E MONTAGEM DE MOBILIARIOS DIVE |                55| R$800,00        |
| 77609080401760 |            837| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$800,00        |
| 77609080401760 |           3907| AQUISICAO DE MOBILIARIO - MIRACEMA       |                55| R$800,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$800,00        |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$800,00        |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$798,88        |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$786,00        |
| 40600060201860 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$786,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$785,30        |
| 96904000411110 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$780,00        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$779,00        |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$777,35        |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$771,00        |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$769,00        |
| 40600060201860 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$765,00        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$759,00        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$754,88        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$752,30        |
| 11707007221670 |           1998| UERJ\_AQUIS\_MOBILIARIO, AR COND, ETC.   |                55| R$750,00        |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$750,00        |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$750,00        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$750,00        |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$748,40        |
| 77609080401760 |           5880| UERJ-AQUIS DE MESAS E AR CONDICIONADO    |                55| R$739,63        |
| 77609080401760 |           4911| AQUISICAO DE MOBILIARIO                  |                55| R$738,00        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$730,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$729,90        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$726,65        |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$715,59        |
| 96904000411110 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$713,00        |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$711,30        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$709,00        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$705,69        |
| 77609080401760 |           4911| AQUISICAO DE MOBILIARIO                  |                55| R$705,00        |
| 56003086011990 |          11698| AQUISICAO DE MOBILIARIO SEDE DETRAN/RJ   |                55| R$705,00        |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$704,00        |
| 77609080401760 |            926| DETRAN/RJ - AQUISICAO DE MOBILIARIO.     |                55| R$700,00        |
| 77609080401760 |           3654| AQUISICAO DE MOBILIARIO DE ESCRITORIO    |                55| R$700,00        |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$700,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$700,00        |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$700,00        |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$700,00        |
| 96904000411110 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$699,96        |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$699,96        |
| 56003086011990 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$697,90        |
| 58602037001290 |            765| SEPLAG - AQUISICAO DE MOBILIARIO         |                55| R$695,00        |
| 96904000411110 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$695,00        |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$680,00        |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$680,00        |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$673,52        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$669,80        |
| 77609080401760 |           3907| AQUISICAO DE MOBILIARIO - MIRACEMA       |                55| R$668,00        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$665,00        |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$664,62        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$656,65        |
| 14208011011210 |           9807| AQUISICAO DE MOBILIARIO P/ ECO TEC.      |                55| R$656,51        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$646,85        |
| 42400015001830 |           6767| AQUISICAO DE MOBILIARIO DIVISAO MEDICA   |                55| R$640,00        |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$640,00        |
| 96904000411110 |          12891| CEASA-RJ AQUISICAO DE MOBILIARIOS        |                55| R$640,00        |
| 11707007221670 |           1998| UERJ\_AQUIS\_MOBILIARIO, AR COND, ETC.   |                55| R$630,00        |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$630,00        |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$630,00        |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$630,00        |
| 96904000411110 |          14580| CEASA-RJ AQUISICAO DE MOBILIARIO         |                55| R$630,00        |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$629,00        |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$628,00        |
| 77609080401760 |           7606| UERJ - MOBILIARIO P SOC/UPE              |                55| R$620,01        |
| 77609080401760 |           5880| UERJ-AQUIS DE MESAS E AR CONDICIONADO    |                55| R$615,69        |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$612,66        |
| 96904000411110 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$612,00        |
| 96904000411110 |           7899| SEA - AQUISICAO DE MOBILIARIOS           |                55| R$606,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$600,00        |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$600,00        |
| 16707032211700 |           8504| UENF - MOBILIARIO (REST. UNIV.)          |                55| R$600,00        |
| 40600060201860 |          12535| CEASA-RJ AQUISICAO DE MOBILIARIO         |                55| R$600,00        |
| 56003086011990 |          14364| AQ. DE MOBILIARIO PARA RESENDE           |                55| R$596,46        |
| 96904000411110 |           2045| FUNARJ - AQUISICAO DE MOBILIARIO E AFINS |                55| R$591,00        |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$588,00        |
| 77609080401760 |           6156| SEASDH-AQUISICAO DE MATERIAL PERMANENTE  |                55| R$582,39        |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$580,53        |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$580,00        |
| 45308083001170 |           8237| SEC - AQUISICAO DE MOBILIARIO            |                55| R$579,43        |
| 42400015001830 |           6767| AQUISICAO DE MOBILIARIO DIVISAO MEDICA   |                55| R$575,91        |
| 96904000411110 |           3217| COMPRA DE MOBILIARIO                     |                55| R$568,00        |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$567,00        |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$564,00        |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$560,00        |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$560,00        |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$552,00        |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$541,00        |
| 43408017001530 |           3998| AQUISICAO DE ESTABILIZADORES E NOBREAK   |                55| R$532,00        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$528,92        |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$528,04        |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$526,47        |
| 96904000411110 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$524,93        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$522,00        |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$514,56        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$513,92        |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$513,00        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$510,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$507,00        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$506,92        |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$504,00        |
| 77609080401760 |           7934| AQUISICAO DE MOBILIAS                    |                55| R$501,38        |
| 77609080401760 |           4911| AQUISICAO DE MOBILIARIO                  |                55| R$500,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$500,00        |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$500,00        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$500,00        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$494,00        |
| 45308083001170 |           3231| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$492,08        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$489,52        |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$486,48        |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$483,84        |
| 77609080401760 |           5065| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$480,00        |
| 77609080401760 |           7606| UERJ - MOBILIARIO P SOC/UPE              |                55| R$479,78        |
| 40600060201860 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$468,00        |
| 24001016601560 |          14306| UERJ-AQUISICAO DE MOBILIARIO             |                55| R$466,93        |
| 66301089601710 |          12997| UERJ - AQUIS MOBILIARIO DE ESCRITORIO    |                55| R$459,00        |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$458,60        |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$458,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$450,00        |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$449,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$443,30        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$440,95        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$425,00        |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$423,00        |
| 11707007221670 |            628| AQUISICAO DE EQUIPAMENTOS ELETRICOS      |                55| R$422,79        |
| 77609080401760 |           3907| AQUISICAO DE MOBILIARIO - MIRACEMA       |                55| R$420,00        |
| 96904000411110 |          11358| UERJ-AQUISICAO DE MOBILIARIOS            |                55| R$415,00        |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$412,45        |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$408,40        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$403,30        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$400,00        |
| 56003086011990 |          13421| AQUISICAO DE ELETRODOMESTICOS            |                55| R$400,00        |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$399,95        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$399,00        |
| 02006042701140 |            940| REGISTRO DE PRECO PARA ROUPEIRO DE ACO   |                55| R$390,00        |
| 21009090011800 |          13420| AQUISICAO DE MOBILIARIOS                 |                55| R$390,00        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$388,98        |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$384,00        |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$384,00        |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$381,40        |
| 42400015001830 |           7084| AQUISICAO DE MOBILIARIO                  |                55| R$381,00        |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$381,00        |
| 43408017001530 |           5743| AQUISICAO DE MATERIAL PERMANENTE         |                55| R$376,26        |
| 77609080401760 |            926| DETRAN/RJ - AQUISICAO DE MOBILIARIO.     |                55| R$375,00        |
| 96904000411110 |          14580| CEASA-RJ AQUISICAO DE MOBILIARIO         |                55| R$364,99        |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$362,16        |
| 45308083001170 |           2377| SEAPEC MOVEIS DE ESCRITORIO              |                55| R$360,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$360,00        |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$358,08        |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$358,00        |
| 40600060201860 |          13091| AQUISICAO DE MOBILIARIO                  |                55| R$357,90        |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$355,84        |
| 43408017001530 |           7261| AQUISICAO DE EQUIPAMENTOS DE INFORMATICA |                55| R$355,00        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$350,00        |
| 56003086011990 |          13421| AQUISICAO DE ELETRODOMESTICOS            |                55| R$350,00        |
| 77609080401760 |            926| DETRAN/RJ - AQUISICAO DE MOBILIARIO.     |                55| R$344,90        |
| 43005036401540 |          12891| CEASA-RJ AQUISICAO DE MOBILIARIOS        |                55| R$342,89        |
| 77609080401760 |           3056| UERJ-AQUISICAO DE MOBILIARIO E BEBEDOURO |                55| R$339,00        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$337,14        |
| 96904000411110 |          12686| IVB - AQUISICAO DE MOBILIARIOS DIVERSOS. |                55| R$336,42        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$333,85        |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$330,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$329,65        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$328,57        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$323,90        |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$320,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$320,00        |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$319,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$312,00        |
| 77609080401760 |           4911| AQUISICAO DE MOBILIARIO                  |                55| R$304,00        |
| 77609080401760 |            837| AQUISICAO DE MOBILIARIO EM GERAL         |                55| R$300,00        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$300,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$300,00        |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$300,00        |
| 40600060201860 |           8302| AQUISICAO DE MOBILIARIO                  |                55| R$300,00        |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$300,00        |
| 43408017001530 |          10936| UERJ - MOBILIARIO - MESAS, CADEIRAS, ETC |                55| R$300,00        |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$300,00        |
| 45308083001170 |          12544| SEASDH - MATERIAL PERMANENTE SUPIM       |                55| R$298,10        |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$293,50        |
| 45308083001170 |          12041| UERJ - MOBILIARIO P HUPE.                |                55| R$290,00        |
| 43408017001530 |           6502| EQUIPAMENTOS DE MULTIMIDIA               |                55| R$280,00        |
| 43408017001530 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$280,00        |
| 21009090011800 |           7276| AQUISICAO DE MOBILIARIOS                 |                55| R$272,00        |
| 56003086011990 |          13421| AQUISICAO DE ELETRODOMESTICOS            |                55| R$270,00        |
| 77609080401760 |           5620| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$260,51        |
| 43408017001530 |           4267| AQ. EQ. INFORMATICA E ELETRO-ELETRONICOS |                55| R$260,00        |
| 42400015001830 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$256,80        |
| 77609080401760 |           2747| INEA AQUIS. MOBILIARIO PARA UNID.DO INEA |                55| R$251,93        |
| 77609080401760 |            926| DETRAN/RJ - AQUISICAO DE MOBILIARIO.     |                55| R$240,00        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$238,80        |
| 16707032211700 |           7350| PCERJ - AQ CADEIRAS E LONGARINAS         |                55| R$237,50        |
| 77609080401760 |           7760| AQUISICAO DE MOBILIARIO - CITTA AMERICA  |                55| R$237,00        |
| 77609080401760 |            926| DETRAN/RJ - AQUISICAO DE MOBILIARIO.     |                55| R$230,00        |
| 11707007221670 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$229,00        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$222,84        |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$222,00        |
| 96904000411110 |            273| AQ . DE MOBILIARIO PARA CGU/SSGE/ASSEJUR |                55| R$219,99        |
| 96904000411110 |           5846| AQUISICAO DE MOBILIARIOS DE ESCRITORIO.  |                55| R$213,50        |
| 77609080401760 |           6372| SEASDH - AQ.MAT.PERMANENTE (MESAS, ETC.) |                55| R$213,50        |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$208,90        |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$200,00        |
| 43408017001530 |           8021| AQUISICAO DE ELETROELETRONICOS.          |                55| R$199,98        |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$191,97        |
| 77609080401760 |           5620| SEASDH - AQUISICAO DE MOBILIARIO         |                55| R$191,64        |
| 77609080401760 |           3335| AQUISICAO DE MOBILIARIO                  |                55| R$190,00        |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$180,00        |
| 42400015001830 |           9213| AQUISICAO DE MOBILIARIO                  |                55| R$180,00        |
| 40600060201860 |          13095| AQUISICAO MOBILIARIO MARICA              |                55| R$172,00        |
| 42400015001830 |           6767| AQUISICAO DE MOBILIARIO DIVISAO MEDICA   |                55| R$170,00        |
| 21009090011800 |           6652| MOBILIARIO E ART. DE ESCRITORIO          |                55| R$160,00        |
| 77609080401760 |            926| DETRAN/RJ - AQUISICAO DE MOBILIARIO.     |                55| R$150,00        |
| 56003086011990 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$150,00        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$148,57        |
| 77609080401760 |           7875| AQUISICAO DE MOBILIARIO ESCOLA PUBLICA   |                55| R$142,17        |
| 77609080401760 |            970| AQUISICAO DE MOBILIARIO.                 |                55| R$141,00        |
| 21009090011800 |           8449| COMPRA DE MOBILIARIO DE ESCRITORIO       |                55| R$137,58        |
| 77609080401760 |           3005| MOBILIARIO PARA A SESEG                  |                55| R$109,52        |
| 42400015001830 |          10767| AQUISICAO DE MOBILIARIOS                 |                55| R$100,00        |
| 21009090011800 |          10731| IVB - AQUISICAO DE MOBILIARIOS.          |                55| R$81,76         |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$58,00         |
| 43408017001530 |           4008| SEASDH - AQ. MATERIAL PERMANENTE         |                55| R$45,00         |
| 56003086011990 |          10710| MATERIAL DE LIMPEZA                      |                55| R$38,68         |
| 51509082901750 |           4776| RP. PREST.SERV.PREPARO E DISTR.REFEICOES |                79| R$23.699.862,00 |
| 65208000401300 |           7703| PRESTACAO DE SERVICO DE ALIMENTACAO-SES  |                79| R$11.745.595,08 |
| 36704017301500 |           7703| PRESTACAO DE SERVICO DE ALIMENTACAO-SES  |                79| R$10.883.029,80 |
| 51509082901750 |          12519| DEGASE - FORNECIMENTO ALIMENTACAO        |                79| R$9.100.000,00  |
| 51509082901750 |           8744| RESTAURANTE POPULAR MADUREIRA            |                79| R$5.499.345,60  |
| 02407092871630 |          13893| PREST SERV ALIM REST CIDADAO BONSUCESSO  |                79| R$4.812.080,00  |
| 51509082901750 |          11835| SEASDH - ALIMENTACAO ACR                 |                79| R$4.370.000,00  |
| 02407092871630 |           7430| SEASDH- PREP.REFEICOES REST. BONSUCESSO  |                79| R$4.323.637,50  |
| 46106017401610 |           6649| SEASDH-PREPARO REFEICOES REST. D.CAXIAS  |                79| R$4.308.937,50  |
| 16804037871030 |           7432| SEASDH-REFEICOES REST.CAMPO GRANDE       |                79| R$4.306.089,37  |
| 23901017001560 |            651| PCERJ-COMIDA DE PRESOS - ABRANGENCIA IV  |                79| R$4.279.000,00  |
| 23901017001560 |           2687| PCERJ-AQ REFEICAO PRESOS-ABRANGENCIA V   |                79| R$3.886.596,00  |
| 51509082901750 |          13092| SEASDH - ALIMENTACAO REST POPULAR VR     |                79| R$3.721.984,00  |
| 23901017001560 |           1680| RESTAURANTE POP NITEROI                  |                79| R$3.502.200,00  |
| 23901017001560 |           2686| PCERJ-AQ REFEICAO PRESOS-ABRANGENCIA IV  |                79| R$3.389.457,60  |
| 23901017001560 |          13396| PREST. SERV. ALIM. REST. CIDADAO CAMPOS  |                79| R$3.373.350,00  |
| 23901017001560 |          14136| PREST SERV ALIM REST CIDADAO DE IRAJA    |                79| R$3.284.685,00  |
| 23901017001560 |            652| PCERJ-COMIDA DE PRESOS - ABRANGENCIA V   |                79| R$3.260.000,00  |
| 23901017001560 |           5239| SEASDH-PREPARO,FORN.ALIMENT.REST.IRAJA   |                79| R$2.902.500,00  |
| 51509082901750 |            649| PCERJ-COMIDA DE PRESOS - ABRANGENCIA III |                79| R$2.335.000,00  |
| 23901017001560 |            653| PCERJ-COMIDA DE PRESOS-ABRANGENCIA VI    |                79| R$1.864.300,00  |
| 23901017001560 |           2688| PCERJ-AQ REFEICAO PRESOS-ABRANGENCIA VI  |                79| R$1.779.624,00  |
| 51509082901750 |           2685| PCERJ-AQ REFEICAO PRESOS-ABRANGENCIA III |                79| R$1.610.136,00  |
| 36704017301500 |           5677| AQUISICAO REFEICAO PRONTA TRANSPORTADA   |                79| R$1.475.760,00  |
| 51509082901750 |           8744| RESTAURANTE POPULAR MADUREIRA            |                79| R$575.656,20    |
| 02407092871630 |           7430| SEASDH- PREP.REFEICOES REST. BONSUCESSO  |                79| R$430.710,00    |
| 23901017001560 |           1680| RESTAURANTE POP NITEROI                  |                79| R$353.600,00    |
| 02407092871630 |          13893| PREST SERV ALIM REST CIDADAO BONSUCESSO  |                79| R$307.569,60    |
| 23901017001560 |          13396| PREST. SERV. ALIM. REST. CIDADAO CAMPOS  |                79| R$306.504,00    |
| 16804037871030 |           7432| SEASDH-REFEICOES REST.CAMPO GRANDE       |                79| R$294.372,35    |
| 46106017401610 |           6649| SEASDH-PREPARO REFEICOES REST. D.CAXIAS  |                79| R$287.875,00    |
| 51509082901750 |          13092| SEASDH - ALIMENTACAO REST POPULAR VR     |                79| R$277.760,00    |
| 23901017001560 |          14136| PREST SERV ALIM REST CIDADAO DE IRAJA    |                79| R$239.598,00    |
| 23901017001560 |           5239| SEASDH-PREPARO,FORN.ALIMENT.REST.IRAJA   |                79| R$214.914,00    |
| 79705093331210 |           8359| PCERJ - OUTORGA LOCAL REFEICOES CIDPOL   |                79| R$20,00         |
| 04207049801440 |          12914| POSTOS SERVICOS DE IDENTIFICACAO CIVIL   |                81| R$18.637.299,96 |
| 86202000601930 |           8091| SERV. INSTAL. PLACAS LACRES TARJETAS     |                81| R$12.499.999,92 |
| 86202000601930 |          12232| INSTALACAO DE PLACAS, TARJETAS E LACRES  |                81| R$9.699.999,96  |
| 86202000601930 |          12748| PCERJ - TRANSPORTE VOLUMES DOCUMENTOS    |                81| R$2.841.000,00  |
| 04207049801440 |            608| SEPLAG - SERVICO IDENTIFICACAO BIOMETRIC |                81| R$2.731.264,60  |
| 86202000601930 |          10899| PRESTACAO SERVICO DE ENTREGA DE MALOTE   |                81| R$2.663.905,92  |
| 86202000601930 |           2883| SERVICO DE PREPARO TECNICO DOCUMENTOS    |                81| R$2.507.590,00  |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$1.722.500,00  |
| 86202000601930 |          10080| SEPLAG - SERVICO DE CONDUCAO DE VEICULOS |                81| R$1.603.346,22  |
| 85801000731420 |           8320| SEOBRAS - SERVICOS APOIO ADMINIST.TEC.   |                81| R$1.346.980,00  |
| 98802050511670 |          11406| CBMERJ - P.S. BUROCRATICOS DO FUNESBOM   |                81| R$1.294.484,28  |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$1.103.500,00  |
| 98802050511670 |          12908| MANUTENCAO PREDIAL REGIAO 1              |                81| R$1.079.175,84  |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$1.031.625,00  |
| 86202000601930 |          10080| SEPLAG - SERVICO DE CONDUCAO DE VEICULOS |                81| R$1.007.790,60  |
| 98802050511670 |           9018| ITERJ - SERVICOS DE TOPOGRAFIA, CADASTRO |                81| R$968.000,00    |
| 13503090201920 |           9624| SERVICO DE GUARDA E GESTAO ARQUIVISTICA  |                81| R$960.107,88    |
| 97900061111910 |          12484| SEASDH - LIMPEZA E CONSERVACAO FIA       |                81| R$880.000,00    |
| 86202000601930 |          10080| SEPLAG - SERVICO DE CONDUCAO DE VEICULOS |                81| R$858.503,58    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$834.250,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$823.000,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$810.835,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$801.625,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$798.000,00    |
| 98802050511670 |           7126| SERVICO DE CADASTRAMENTO                 |                81| R$797.900,00    |
| 98802050511670 |          14405| CEASA-RJ CONTRAT. DE SERV. TERCEIRIZADO  |                81| R$699.737,88    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$610.165,00    |
| 98802050511670 |          11394| SERVICOS DE MANUTENCAO PREDIAL           |                81| R$519.334,32    |
| 86202000601930 |          10080| SEPLAG - SERVICO DE CONDUCAO DE VEICULOS |                81| R$450.241,33    |
| 86202000601930 |           5117| PRESTACAO DE SERVICOS DE PROTOCOLO       |                81| R$396.645,12    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$326.100,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$300.400,00    |
| 98802050511670 |          10872| ITERJ - PE 002/2014 - TOPOGRAFIA         |                81| R$282.933,41    |
| 98802050511670 |           8394| SERVICO DE RECEPCIONISTA BILINGUE CICC   |                81| R$275.305,68    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$250.400,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$217.267,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$217.033,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$208.900,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$207.700,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$203.225,00    |
| 98802050511670 |          11272| AGERIO - MANUTENCAO AR CONDICIONADO      |                81| R$198.984,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$192.400,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$188.975,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$187.150,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$181.650,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$177.200,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$174.025,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$173.600,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$172.567,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$154.400,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$150.450,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$144.300,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$138.525,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$135.050,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$133.100,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$132.766,50    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$131.733,50    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$129.033,50    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$116.400,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$116.116,50    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$115.316,50    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$113.550,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$113.300,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$111.200,00    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$109.833,50    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$107.266,50    |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$96.050,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$93.716,50     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$92.450,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$92.216,50     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$91.783,50     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$76.300,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$72.600,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$72.252,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$68.716,50     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$63.283,50     |
| 98802050511670 |          13002| INSTALACAO DE SISTEMA DE SEGURANCA CFTV  |                81| R$48.400,00     |
| 98802050511670 |           8315| CODIN-MANUTENCAO PREVENTIVA E CORRETIVA  |                81| R$46.128,00     |
| 86202000601930 |           7973| SERV. DE TRANSPORTE DE PROVAS E FISCAIS  |                81| R$44.899,98     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$36.000,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$31.494,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$30.261,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$29.038,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$25.396,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$24.776,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$24.270,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$21.055,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$18.410,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$17.965,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$17.600,00     |
| 98802050511670 |          13214| REGISTRO DE PRECOS - ESTUDOS E PESQUISAS |                81| R$14.630,00     |
| 41101081601160 |           8909| SERVICOS SEGURANCA E VIGILANCIA ARMADA   |                96| R$10.015.000,00 |
| 41101081601160 |           8909| SERVICOS SEGURANCA E VIGILANCIA ARMADA   |                96| R$9.621.999,99  |
| 14700029001350 |          12642| INEA SERV DE VIG.PATRIMONIAL ARMADA      |                96| R$8.905.793,88  |
| 41101081601160 |           9013| PCERJ-PS SEGURANCA ARMADA LOTE I         |                96| R$8.370.000,00  |
| 14700029001350 |          12453| UENF - VIGILANCIA ARMADA E DESARMADA     |                96| R$7.320.000,00  |
| 14700029001350 |           9612| SEC - SERV.VIGILANCIA ARMADA E DESARMADA |                96| R$5.708.164,15  |
| 14700029001350 |           5121| SEASDH-SERVICO DE VIGILANCIA DESARMADA   |                96| R$5.633.000,00  |
| 34802092701250 |           7069| PGE-RJ- SERVICO DE VIGILANCIA DESARMADA  |                96| R$5.273.998,92  |
| 67004057711700 |          10253| PE 29/2013 PRESTACAO SERV. VIGILANCIA    |                96| R$4.601.175,36  |
| 41101081601160 |           9391| DPGE-RJ PREST.SERV.VIGILANCIA DESARMADA  |                96| R$3.997.955,52  |
| 14700029001350 |           9628| FUNARJ-SERVICO DE VIGILANCIA E SEGURANCA |                96| R$3.024.895,80  |
| 13606080901470 |           9647| VIGILANCIA E SEGURANCA ARMADA            |                96| R$2.922.440,16  |
| 13606080901470 |           9000| SERVICO DE VIGILANCIA E SEGURANCA        |                96| R$2.229.000,00  |
| 42105075011100 |           5944| SERV. DE SEGURANCA E VIGILANCIA ARMADA   |                96| R$2.155.000,00  |
| 49403005311630 |           6695| VIGILANCIA E SEGURANCA PATRIMONIAL       |                96| R$2.063.976,00  |
| 01601052501270 |           2490| PRESTACAO SV.DE VIG.ARMADA E DESARMADA   |                96| R$2.007.021,12  |
| 13606080901470 |           8498| PRESTACAO DOS SERVICOS DE VIGILANCIA     |                96| R$1.788.000,00  |
| 41908001601920 |           6069| PCERJ-PS VIGILANCIA PATRIMONIAL ARMADA   |                96| R$1.502.499,99  |
| 67004057711700 |           7420| CONTRATATACAO VIGILANCIA DESARMADA       |                96| R$1.319.000,00  |
| 13606080901470 |           6743| VIGILANCIA DESARMADA                     |                96| R$1.272.000,00  |
| 41101081601160 |          10025| VIGILANCIA ARMADA                        |                96| R$1.123.200,00  |
| 14700029001350 |           7761| CODERTE - SERVICO SEGURANCA E VIGILANCIA |                96| R$1.013.397,84  |
| 01903003401810 |           6134| PRESTACAO SERVICOS DE VIGILANCIA         |                96| R$1.007.998,92  |
| 01903003401810 |           1345| PREST. SERV. ESPECIALIZADO EM VIGILANCIA |                96| R$878.497,70    |
| 01601052501270 |           7311| SEPLAG - VIG. ELETRONICA E PATRIMONIAL   |                96| R$845.143,80    |
| 41101081601160 |           2940| CONTRATACAO DE SERVICOS DE VIGILANCIA    |                96| R$839.808,00    |
| 34802092701250 |           9081| SERVICO DE VIGILANCIA DESARMADA          |                96| R$837.990,00    |
| 49403005311630 |           5642| DETRO - SERVICOS DE VIGILANCIA           |                96| R$764.000,00    |
| 41908001601920 |           6071| PREST DE SERVICO DE VIGILANCIA DESARMADA |                96| R$633.998,99    |
| 01601052501270 |           8451| SERVICOS DE VIGILANCIA                   |                96| R$565.536,00    |
| 14700029001350 |           5559| FUNARJ-SERV.VIGILANCIA E SEGURANCA       |                96| R$473.250,00    |
| 01601052501270 |           2958| SEPLAG - SERVICO VIGILANCIA E SEGURANCA  |                96| R$433.235,52    |
| 01601052501270 |            764| SEPLAG - SERVICO DE VIGILANCIA           |                96| R$406.719,84    |
| 42105075011100 |           8290| CONTRATACAO SEGURANCA ARMADA -SEDRAP     |                96| R$398.879,25    |
| 41908001601920 |           2218| FIPERJ - CONTRATACAO SERV. VIGILANCIA    |                96| R$262.499,00    |
| 01903003401810 |           2575| SERVICO DE VIGILANCIA DESARMADA          |                96| R$260.784,12    |
| 96106007401950 |          11841| VIGILANCIA E SEGURANCA DESARMADA         |                96| R$238.950,00    |
| 34802092701250 |          14660| DRM - SERVICO DE VIGILANCIA              |                96| R$220.015,92    |
| 13606080901470 |           7995| UERJ - LOCACAO VEICULOS P VESTIBULAR     |                96| R$206.500,00    |
| 41908001601920 |           4148| VIGILANCIA PATRIM. E SEGURANCA DESARMADA |                96| R$202.999,68    |
| 41908001601920 |           7047| PREGAO DE SERVICOS DE VIGILANCIA         |                96| R$155.560,56    |
| 96106007401950 |           9539| CONTRATACAO DE SERVICO DE SEGURANCA      |                96| R$135.000,00    |
| 01601052501270 |           1483| SEPLAG - SERVICOS DE VIGILANCIA          |                96| R$115.650,72    |
| 41908001601920 |           7543| PREST DE SERVICO DE VIGILANCIA DESARMADA |                96| R$115.300,00    |
| 34802092701250 |            452| SERVICOS DE VIG. SEG. ARMADA             |                96| R$108.003,00    |
| 01903003401810 |           1084| AGENERSA SERVICO DE VIGILANCIA           |                96| R$77.500,00     |
| 42105075011100 |           8290| CONTRATACAO SEGURANCA ARMADA -SEDRAP     |                96| R$72.072,96     |
| 67004057711700 |           6265| LOTERJ - SERV. VIGILANCIA E SEGURANCA    |                96| R$68.133,47     |
| 67004057711700 |           6265| LOTERJ - SERV. VIGILANCIA E SEGURANCA    |                96| R$62.947,56     |
| 01903003401810 |            929| FIA - VIGILANCIA DESARMADA               |                96| R$54.189,95     |
| 34802092701250 |            452| SERVICOS DE VIG. SEG. ARMADA             |                96| R$52.782,03     |
| 13606080901470 |           3277| LOCACAO DE VEICULOS VESTIBULAR 2012      |                96| R$3.150,00      |
| 13606080901470 |           7995| UERJ - LOCACAO VEICULOS P VESTIBULAR     |                96| R$2.700,00      |
| 23103009501290 |          10116| DETRO - LOCACAO DE VEICULOS C/MOTORISTA  |               109| R$6.584.922,28  |
| 69009079191730 |           7893| IPEM - LOCACAO DE VEICULOS DO TIPO SEDAN |               109| R$1.838.592,00  |
| 94105021811920 |           8562| SERVICO DE LOCACAO DE VEICULOS           |               109| R$1.789.920,00  |
| 67106033311950 |          14634| SEGOV - LOCACAO DE VEICULOS              |               109| R$1.727.856,00  |
| 19109028401870 |           8469| SESEG - LOCACAO DE VEICULOS BLINDADOS    |               109| R$1.498.560,00  |
| 51300095501480 |           3841| INEA LOC.DE PICK-UPS 4X4 SEM MOTORISTA   |               109| R$1.338.999,96  |
| 50401070011130 |           5539| SEGOV - LOC. VEICULOS OBF                |               109| R$1.299.960,00  |
| 69009079191730 |           1674| LOCACAO DE 02 VEICULOS BLINDADOS         |               109| R$1.260.000,00  |
| 51300095501480 |           5575| INEA LOCACAO DE 25 PICK-UPS              |               109| R$1.150.000,00  |
| 23103009501290 |          10116| DETRO - LOCACAO DE VEICULOS C/MOTORISTA  |               109| R$1.139.695,86  |
| 30105026001500 |           4200| PE01/2012 SEGOV LOC VEICULOS LEI SECA    |               109| R$1.116.900,00  |
| 94105021811920 |           5497| SEEDUC - SERV.FRETAMENTO DE CAMINHAO BAU |               109| R$1.033.775,16  |
| 50302004201970 |          13303| DEGASE - LOCACAO VEICULOS COM MOTORISTA  |               109| R$855.000,00    |
| 30105026001500 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$852.661,67    |
| 32806081801390 |          11149| PE 02/2014 VEICULOS                      |               109| R$850.500,00    |
| 50401070011130 |           5539| SEGOV - LOC. VEICULOS OBF                |               109| R$786.240,00    |
| 29809014001500 |           8406| PREST DE SERVICO DE LOCACAO DE VEICULOS  |               109| R$707.266,20    |
| 29903060201600 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$629.314,00    |
| 29809014001500 |           5286| PREST. SERVICO DE LOCACAO DE VEICULOS    |               109| R$617.382,72    |
| 30105026001500 |           4200| PE01/2012 SEGOV LOC VEICULOS LEI SECA    |               109| R$571.968,00    |
| 94105021811920 |          13977| DEGASE - LOCACAO DE VEICULOS             |               109| R$558.900,00    |
| 30105026001500 |            945| LOCACAO DE VEICULOS                      |               109| R$555.555,00    |
| 77507077901330 |           1937| RP PARA A LOC. VEIC. AUT. (REP. /SEDAN)  |               109| R$535.342,50    |
| 62801003711270 |           8924| SERVICO DE LOCACAO DE VEICULOS TIPO VAN  |               109| R$501.000,00    |
| 54106081001240 |            945| LOCACAO DE VEICULOS                      |               109| R$460.248,00    |
| 29809014001500 |           5092| PREST SERV TRANSPORTE                    |               109| R$459.950,52    |
| 29903060201600 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$456.597,00    |
| 30105026001500 |           4200| PE01/2012 SEGOV LOC VEICULOS LEI SECA    |               109| R$455.544,00    |
| 23103009501290 |          10116| DETRO - LOCACAO DE VEICULOS C/MOTORISTA  |               109| R$422.107,99    |
| 55601092201180 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$418.266,00    |
| 50302004201970 |            945| LOCACAO DE VEICULOS                      |               109| R$414.310,95    |
| 30105026001500 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$402.675,00    |
| 29903060201600 |          12541| SEPLAG - SERVICO DE LOCACAO DE VEICULOS  |               109| R$399.040,00    |
| 69009079191730 |           1373| LOCACAO DE VEICULOS BILNDADOS NIVEL IIIA |               109| R$396.000,00    |
| 62801003711270 |           6410| INEA LOC.VEIC.UTILITARIOS -PROJ.OLIMPICO |               109| R$369.750,00    |
| 30105026001500 |            945| LOCACAO DE VEICULOS                      |               109| R$367.380,00    |
| 29809014001500 |           1902| LOCACAO DE VEICULOS BLINDADOS NIVEL IIIA |               109| R$343.598,00    |
| 30105026001500 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$319.930,00    |
| 29809014001500 |           8131| LOCACAO DE VEICULOS                      |               109| R$308.112,00    |
| 62801003711270 |            945| LOCACAO DE VEICULOS                      |               109| R$300.884,00    |
| 07906020601020 |          13509| UERJ - LOCACAO DE VEICULOS C/ MOTORISTAS |               109| R$297.500,00    |
| 50801082001270 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$278.695,00    |
| 62801003711270 |            945| LOCACAO DE VEICULOS                      |               109| R$253.008,00    |
| 94105021811920 |          12932| SEGOV - LOCACAO DE VEICULOS              |               109| R$250.872,00    |
| 50401070011130 |           4941| SERVICOS DE LOCACAO DE VEICULOS.         |               109| R$244.348,19    |
| 94105021811920 |          12932| SEGOV - LOCACAO DE VEICULOS              |               109| R$224.640,00    |
| 67106033311950 |           8638| LOCACAO DE VAN ADAPTADAS                 |               109| R$215.976,00    |
| 63308095301800 |           4856| SERVICOS DE LOCACAO DE VEICULOS          |               109| R$212.100,11    |
| 07906020601020 |          12444| UERJ - LOCACAO VEICULOS C MOTORISTAS     |               109| R$210.647,77    |
| 07906020601020 |          12308| SERV. DE LOC. DE VEICULOS COM MOTORISTAS |               109| R$202.999,95    |
| 67106033311950 |          11836| SEFAZ PE014 LOCACAO DE CAMINHAO BAU      |               109| R$175.680,00    |
| 07906020601020 |          11119| UERJ - LOCACAO DE VEICULO                |               109| R$170.327,12    |
| 19109028401870 |           7822| LOCACAO DE VEICULOS BLINDADOS            |               109| R$168.000,00    |
| 94105021811920 |          13054| SEGOV - LOCACAO DE VAN ADAPTADA          |               109| R$167.880,00    |
| 50401070011130 |           3985| CONTRATACAO DE EMPRESA PARA TRANSPORTE   |               109| R$153.870,00    |
| 07906020601020 |           9620| UERJ - LOCACAO DE VEICULOS               |               109| R$150.000,00    |
| 30105026001500 |           1414| LOCACAO DE VEICULOS DE REPRESENTACAO     |               109| R$148.800,00    |
| 67106033311950 |          10120| PE 011/2013 - LOCACAO DE VEICULO BLINDAD |               109| R$147.960,00    |
| 50401070011130 |           4663| UERJ - LOCACAO DE VEICULO VESTIBULAR     |               109| R$145.000,00    |
| 62801003711270 |            716| SEGOV - LOCACAO DE VEICULOS              |               109| R$138.000,00    |
| 07906020601020 |          14610| UERJ - LOCACAO VEICULOS C/ MOTORISTAS    |               109| R$135.000,00    |
| 51300095501480 |           2364| LOCACAO DE VEICULO PICK-UP               |               109| R$131.800,00    |
| 62801003711270 |           2557| LOCACAO DE VEICULOS                      |               109| R$120.600,00    |
| 51300095501480 |           4277| PREGAO ELETRONICO DE LOCACAO DE VEICULOS |               109| R$120.000,00    |
| 50401070011130 |           3277| LOCACAO DE VEICULOS VESTIBULAR 2012      |               109| R$105.998,00    |
| 50401070011130 |           5646| LOCACAO DE VEICULOS TRANSPORTE DE PASSAG |               109| R$104.160,00    |
| 94105021811920 |           5543| SEGOV LOC. VEICULO VAN ADAPTADO OBF      |               109| R$99.999,00     |
| 50401070011130 |           2881| LOCACAO DE VEICULOS C/ MOTORISTA         |               109| R$99.589,00     |
| 30105026001500 |            945| LOCACAO DE VEICULOS                      |               109| R$96.356,00     |
| 29809014001500 |           8103| UERJ - LOCACAO VEICULOS P FAC ENGENHARIA |               109| R$87.017,50     |
| 50401070011130 |           4390| LOCACAO DE VEICULOS PARA ATENDER PROJETO |               109| R$87.000,00     |
| 51300095501480 |           1146| LOCACAO DE VEICULOS                      |               109| R$85.555,99     |
| 58305050801520 |           4390| LOCACAO DE VEICULOS PARA ATENDER PROJETO |               109| R$85.200,00     |
| 23103009501290 |          10116| DETRO - LOCACAO DE VEICULOS C/MOTORISTA  |               109| R$84.419,58     |
| 29903060201600 |          12541| SEPLAG - SERVICO DE LOCACAO DE VEICULOS  |               109| R$81.180,00     |
| 07906020601020 |           9438| UERJ - LOCACAO DE VEICULO                |               109| R$80.500,00     |
| 50401070011130 |           6871| UERJ - LOCACAO DE VEICULO                |               109| R$79.054,40     |
| 77507077901330 |           8368| SERV. TRANSPORTE DE CARGAS DISTRIBUICAO  |               109| R$78.900,00     |
| 19109028401870 |           8631| SEAPEC - LOCACAO VEICULO UTILITARIO      |               109| R$76.800,00     |
| 77507077901330 |           8368| SERV. TRANSPORTE DE CARGAS DISTRIBUICAO  |               109| R$75.997,00     |
| 94105021811920 |          12932| SEGOV - LOCACAO DE VEICULOS              |               109| R$67.800,00     |
| 29809014001500 |           5494| CONT EMP ESP EM ENTREGA RAPIDA - MOTOBOY |               109| R$67.560,00     |
| 50401070011130 |           6758| LOCACAO DE VEICULO UTILITARIO            |               109| R$65.400,00     |
| 50401070011130 |           6833| CECIERJ - SERV. DE TRANSPORTE DE PROVAS  |               109| R$64.760,00     |
| 62801003711270 |           6775| LOCACAO DE VEICULOS AUTOMOTORES TIPO VAN |               109| R$62.503,00     |
| 50302004201970 |           7061| LOCACAO DE VEICULOS COM MOTORISTAS       |               109| R$61.483,95     |
| 23103009501290 |           9833| LOCACAO DE VEICULO C/ MOTORISTA          |               109| R$58.200,00     |
| 51300095501480 |           4277| PREGAO ELETRONICO DE LOCACAO DE VEICULOS |               109| R$55.560,00     |
| 50302004201970 |           7061| LOCACAO DE VEICULOS COM MOTORISTAS       |               109| R$52.025,11     |
| 29809014001500 |           8103| UERJ - LOCACAO VEICULOS P FAC ENGENHARIA |               109| R$49.976,58     |
| 32806081801390 |           4020| SEC- PREST. SERV. MOTOBOY                |               109| R$49.498,00     |
| 67106033311950 |           8704| LOCACAO DE VEICULO                       |               109| R$48.177,84     |
| 07906020601020 |           6885| SERV. LOCACAO DE VEICULOS C/ MOTORISTA   |               109| R$45.000,00     |
| 50401070011130 |           5646| LOCACAO DE VEICULOS TRANSPORTE DE PASSAG |               109| R$41.040,00     |
| 69009079191730 |           2027| UERJ - LOCACAO DE VEICULOS - MAUA        |               109| R$40.500,00     |
| 07906020601020 |          10364| UERJ - LOCACAO VEICULO CEPUERJ           |               109| R$40.000,00     |
| 29809014001500 |           5759| PCERJ- PS ADEQUACAO ACUSTICA             |               109| R$35.900,00     |
| 50401070011130 |           2881| LOCACAO DE VEICULOS C/ MOTORISTA         |               109| R$35.099,96     |
| 29809014001500 |           8103| UERJ - LOCACAO VEICULOS P FAC ENGENHARIA |               109| R$34.348,50     |
| 29809014001500 |           4908| SERVICOS DE LOCACAO CARRO ELETRICO       |               109| R$31.800,00     |
| 94105021811920 |           5567| UERJ-LOCACAO DE VEICULO                  |               109| R$30.997,92     |
| 32806081801390 |           4438| JUCERJA-SERVICO DE TRANSPORTE DE MALOTES |               109| R$29.929,00     |
| 69009079191730 |           5786| LOCACAO DE VEICULO PARA O CEPE           |               109| R$28.092,00     |
| 50401070011130 |           5646| LOCACAO DE VEICULOS TRANSPORTE DE PASSAG |               109| R$27.900,00     |
| 23103009501290 |           2713| SERVICOS DE MOTOBOY                      |               109| R$25.200,00     |
| 07906020601020 |          10364| UERJ - LOCACAO VEICULO CEPUERJ           |               109| R$24.000,00     |
| 50401070011130 |           5638| UERJ - LOCACAO VEICULO 1.6, SEDAN, ETC.  |               109| R$22.989,00     |
| 58305050801520 |           3527| SERVICO DE 07 MOTOBOY                    |               109| R$18.000,00     |
| 69009079191730 |           1605| UERJ - LOCACAO DE VEICULOS PARA LCR      |               109| R$16.950,00     |
| 67106033311950 |           8578| UERJ - LOCACAO VEICULO P SALGUEIRO       |               109| R$14.402,20     |
| 50401070011130 |           3239| LOCACAO DE VEICULOS UTILITARIOS          |               109| R$11.905,20     |
| 77507077901330 |           8368| SERV. TRANSPORTE DE CARGAS DISTRIBUICAO  |               109| R$9.449,00      |
| 50401070011130 |           6871| UERJ - LOCACAO DE VEICULO                |               109| R$5.912,61      |
| 07906020601020 |          11119| UERJ - LOCACAO DE VEICULO                |               109| R$5.628,01      |
| 50401070011130 |           4663| UERJ - LOCACAO DE VEICULO VESTIBULAR     |               109| R$5.440,00      |
| 07906020601020 |          12444| UERJ - LOCACAO VEICULOS C MOTORISTAS     |               109| R$4.352,23      |
| 07906020601020 |           9620| UERJ - LOCACAO DE VEICULOS               |               109| R$4.000,00      |
| 07906020601020 |          13509| UERJ - LOCACAO DE VEICULOS C/ MOTORISTAS |               109| R$2.500,00      |
| 33502068811180 |          14242| PGE - PRESTACAO DE SERVICOS DE LIMPEZA   |               156| R$2.745.799,74  |
| 33502068811180 |          13790| INEA SERV LIMPEZA UNID DESCENTALIZADAS   |               156| R$1.791.495,48  |
| 33502068811180 |          14098| LIMPEZA PREDIAL                          |               156| R$202.248,00    |
| 33502068811180 |          14242| PGE - PRESTACAO DE SERVICOS DE LIMPEZA   |               156| R$142.849,92    |

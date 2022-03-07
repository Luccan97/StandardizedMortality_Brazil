

### O que é?
O dashboard oferece uma vizualização interativa da distribuição espacial nas Unidades Federativas do Brasil das taxas de mortalidade padronizadas por sexo e feixa etária
de acordo com as causas básicas agrupadas nos capítulos do CID-10.

### Porque é importante padronizar as taxas de mortalidade?
As taxas de mortalidade são indicadores muito utilizados para diagnosticar o contexto de saúde de determinados lugares e períodos. Entender a causa básica de óbito das populações ajuda a compreender o seu contexto e características específicas em relação ao modo de vida e das interações com  determinantes de saúde e os serviços de saúde.

A taxa de mortalidade bruta é calculada pela razão entre número de óbitos em um espaço e tempo sobre a população em risco nesse mesmo espaço e tempo. Pode-se dizer que a taxa mortalidade é a incidência de óbitos.

O problema de calcular a taxa bruta é que a comparação entre populações com perfil etário diferente se torna impossível. Por exemplo, a taxa de mortalidade por doenças cardiovasculares será naturalmente maior em territórios com população envelhecida, assim como em geral a taxa de mortalidade por violência será naturalmente maior em territórios com população mais jovem. 

### Como realizar a padronização por faixa etária e sexo?
Existem muitas maneiras de se padronizar uma taxa, a mais comum é  ajustar as populações que serão comparadas por uma mesma população padrão. 
A população padrão utilizada na construção desse dashboard pode ser encontrada [nesse link.](https://www.opendata.nhs.scot/pt_PT/dataset/standard-populations/resource/2f493d21-fd39-48f9-ad6a-9b2c95b32e30?view_id=ce366795-6a5f-483e-8f42-a9dafe239582)

### Dados de Mortalidade do Sistema de Informação de Mortalidade (SIM)
Os dados de mortalidade foram baixados no IDE RStudio por meio do pacote microdatasus, o github com maiores informações pode ser encontrado [aqui](https://github.com/rfsaldanha/microdatasus).

Foram utilizadas as variáveis: Município de residência do óbito, idade em anos, ano do óbito e causa básica.
As causas básicas foram reclassificadas e agrupadas por capítulos da CID-10.

### As bases populacionais anuais das Unidades Federativas
Para construção dos denominadores das taxas de mortalidade utilizou-se as projeções ano a ano por nível de agregação de municípios disponibilizados pelo [laboratório de Estimativas e Projeções Populacionais do Programa de Pós-Graduação em Demografia e Departamento de Demografia e Ciências Atuarias da UFRN](https://demografiaufrn.net/projecao-populacional/).

### Processo de construção do dash:
Nesse repositório estão disponibilizados os scripts 
* 00_prepareData.R que faz a leitura e limpeza das bases utilizadas
* App.R arquivo que constrói a interface de usuário e o servidor do dashboard Shiny.
# Referências

SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.

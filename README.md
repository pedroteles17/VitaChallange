# Vita Challange (2022)

Repositório com os dados e códigos para o Shiny Apps. Além disso, tembém apresenta como os ativos do portfólio de ações locais foram determinados.

Os interessados em verem e testarem o produto final (o aplicativo), podem acessá-lo através do [https://pedroteles17.shinyapps.io/DesafioVita/](link).

Nesse momento, apresentamos cada pasta dentro do nosso repositório.

## backtest

Realiza o backtest dos portfólios de fatores e faz pequenos ajustes nos dados dos fundos de investimento e da taxa livre de risco e IBX (benchmark). Esse código irá gerar três arquivos essenciais para o bom funcionamento do shiny: all_hyper_ports.rds, fundos.rds, index_rf.rds. O primeiro desses refere-se aos dados dos portfólios de fatores e para os outros os nomes são auto-explicativos.

## shiny

Material para reproduzir o shin app.

## definicao_port

Sabendo que os backtests performaram bem, precisamos definir quais ativos comprar para o período seguinte.

BÔNUS: A estratégia original não incorpora fatores ESG. Entretanto, é possível descomentar parte do código e verificar qual seria a composição do portfólio no caso em que também incorporassemos critérios ESG para seleção dos ativos.

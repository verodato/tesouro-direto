# tesouro_direto

**Brazilian sovereign bonds: rates and prices **

Last updated: Jan 2022

These are R scripts that read and parse data on the web pages of the National Treasury and output the tables and visualizations shown in verodato.com/tesouro-direto/.

The time series of bonds rates and taxes, updated daily at the end of the trading day by the National Treasury, is found in Windows Excel files. tesouro_direto.R reads these files and constructs bond rates time series. The excel files can be found in the xls directory. 

Bonds prices are usually updated three times during the trading day. update.R reads this data and incorporates the newest information into the times series tesouro_direto.R assembled. 

See https://www.tesourodireto.com.br/titulos/precos-e-taxas.htm




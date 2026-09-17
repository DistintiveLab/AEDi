# Correcao pontual do citec4 2024 no DW: o append original gravou so 1.128
# municipios (sem 0-fill). O gravar_serie_dw(append) ja faz delete+insert por
# refdate, basta reexecutar o script canônico corrigido (agora com 0-fill).
# Rodar uma unica vez a partir da raiz do AEDi.

source("coleta/citec4_patentes_remoto.R")

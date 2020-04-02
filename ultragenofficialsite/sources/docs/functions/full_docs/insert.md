Diversos valores podem ser passados e cada um será passado para o template de uma forma diferente.

Se o valor for um *alias* de um gen existente no template, o gen inteiro será passado para o template e o alias também será passado como parâmetro. Do contrário apenas o valor será passado como parâmetro.

Os valores podem ser acessados pelo template inserido através do prefixo @param[N]. Os gens passados podem ser acessados por aliases prefixados como paramN, ou seja, os nomes de alias originais são alterados. Em ambos os casos N é a posição do parâmetro iniciando em 0.

### insert x include

Por que você deveria usar insert ou include? Há duas questões a se pensar sobre isso.

A primeira é isolamento de escopo. Insert é executado num escopo completamente isolado, mesmo que o gen dado tenha sido definido como global. Você pode ter certeza de que nada que aconteça em insert será refletido em quem o chamou. Include é mais pensado para interações com o seu chamador. Prover funções, variáveis, valores, ou mesmo gens inteiros. Vale lembrar que templates incluídos tem acesso de modificação ao *gen set* do chamador.

A segunda é semântica e portabilidade. Pela sua característica isolada você pode programar templates que funcionem como componentes. E pode transportar esses componentes entre projetos de forma segura. Pense em includes como uma alternativa para incluir templates ou módulos com várias funções ou recursos.

Em outras palavras, a escolha por usar insert ou include é melhor decidida por você e seu projeto.

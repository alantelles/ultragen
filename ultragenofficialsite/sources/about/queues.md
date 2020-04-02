`DOCUMENTAÇÃO INCOMPLETA`

`RECURSO AINDA NÃO-IMPLEMENTADO`

# Queues

## Conceito

A execução multithread é implementada através das **queues**. Queues são **observers** que controlam conjuntos de **threads**. Uma "vaga" para execução de uma thread é chamada **slot**. Cada queue tem sua própria fila onde guarda a lista de tarefas a fazer e seus slots. Sempre que o procedimento `queue` é executado, a queue declarada adiciona à sua fila a tarefa a ser feita e verifica se há slots disponíveis. Se houver, a tarefa é executada e ao término a fila é avaliada novamente.

## Sintaxe

Basta criar uma `queue` e definir seu alias e um limite de threads simultâneas através da declaração `createQueue:'myQueue',3`, para uma fila chamada *myQueue* com limite de 3 threads, por exemplo.

Para enfileirar uma tarefa que seja um procedimento faça `queue:'myQueue','taskName','param1','paramN'`.

Se você precisa enfileirar uma função faça `queue:'myQueue',aVariable,'taskName','param1','paramN'`. Isto fará com que o retorno da função seja associado à variável *aVariable*. Não é possível fazer uma associação comum da linguagem porque ao enfileirarmos uma tarefa não temos a certeza de quando ela será executada. Naturalmente, deve-se ter bom senso sobre a utilização dessa variável. Se você tentar utilizar esta variável antes do fim da execução dessa tarefa

Se houver mais declarações como essa do que o limite da thread as declarações ficam guardadas na fila para irem sendo executadas conforme *slots* de execução forem liberados.

#### NOTA:

- Queues são executadas de forma totalmente assíncrona, de forma que se você precisar do retorno da tarefa executada, observe se o valor que ela deve apresentar já está disponível.
- Apenas funções e procedimentos podem ser declarados como `'taskName'`. Isto quer dizer que você não deve colocar procedimentos neste parâmetro. Se você utilizar uma chamada de função neste campo, o retorno dela será usado como taskName.

## Exemplos

São utilizações válidas de threads.

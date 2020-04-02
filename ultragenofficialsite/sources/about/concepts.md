# Conceitos

UltraGen é uma **linguagem de script/template engine** voltada para três atividades primariamente:

- Processamento e persistência de textos utilizando dados variáveis sobre templates
- Criação da camada de interfaces com outras tecnologias e visualização de sistemas web.
- Servir de forma simples páginas web ou arquivos.

Nada impede que seja utilizada para outros fins ou que sejam criados novas extensões - tanto *core*, quanto módulos - pelo desenvolvedor para execução de outras tarefas.

## Arquivos gen

Arquivos gen são arquivos de texto plano utilizados como fontes de dados para processamento dos templates. Você encontra tudo que precisa saber sobre eles [nesta seção](). Por hora, saiba que eles são dicionários de dados organizados por chave-valor.

#### NOTA:

- Embora sejam parecidos, não devem ser confundidos com arquivos **.ini**. Arquivos ini e gen não são compatíveis e um arquivo ini não tem garantia de ser bem *parseado* pelo UltraGen.

## Template Engine

UltraGen surgiu como uma nova versão de uma aplicação chamada MetaGen que era estritamente uma template engine. Você pode ler melhor sobre isso na seção sobre [arquivos gen](link). Esta funcionalidade ainda é o core da linguagem. Basicamente UltraGen é capaz de substituir *tokens* marcados em um texto plano template por expressões oriundas de um arquivo de texto plano contendo dados formatados chamados **gen**. Simplificando, em uma utilização básica, UltraGen processa um arquivo fonte de dados sobre um template substituindo no template as áreas demarcadas, chamadas *tokens* por expressões declaradas neste mesmo token.

Um código-fonte UltraGen, por legado desta funcionalidade, é chamado *template*. A título de curiosidade, se você vir o código-fonte do projeto UltraGen, verá que a classe que abriga o código-fonte se chama **TTemplate**.

Já existem funções built-in diretas para a persistência destes templates processados, bem como para manipulação e persistência das estruturas gen, além de declarações para não processar um template em tempo de execução baseado em valores gen.

## Interfaces e visualização Web

Bem, já que temos uma template engine em mãos, por que não potencializá-la? Esta foi a ideia quando foi decidido estender suas funcionalidades para que UltraGen também funcionasse para a construção de páginas web. Assim foi implementado o modo **live** que em vez de persistir apenas exibe o template processado. Logo, se a saída for direcionada à resposta de uma requisição http, temos a exibição de uma página web.

Para ajudar nesta tarefa, um dos recursos implementados foi a herança de templates. Você pode estender um template base exibindo uma seção dentro dele como costuma fazer com as template engines de frameworks conhecidos. Além disso, foi implementado um servidor web multithread embutido e um mínimo framework para o funcionamento de páginas sem sair do ambiente UltraGen.

## A interface com outras tecnologias

Executar processos externos é extremamente fácil em UltraGen. Isto é proposital. É feito desta forma para que você possa facilmente conectar sua aplicação UltraGen com respostas de outras tecnologias. É super simples executar processos externos tanto em série, como em multithread e ainda obter seu retorno para uso posterior.

Obter respostas JSON de APIs REST também é simples através do procedimento **request**. O JSON retornado é convertido para um gen e você pode navegar por ele normalmente.

## "Inquebrável"

UltraGen por padrão busca não "quebrar" quando algo sai errado. Se você buscar uma variável que não existe, ou chamar uma função que não existe, ou com o número errado de argumentos, seu script será executado normalmente e no lugar do dado desejado nada será exibido, como um espaço em branco. Você terá uma string vazia como resposta. Naturalmente, falta algo na sua página, mas o seu site continua de pé e visualizado na sua maior parte. Em desenvolvimento você pode ligar as ferramentas de notificação ou alguma ocasião que você julgue ideal. de erros para uma construção mais segura da sua aplicação.

#### NOTA:

- Ao longo da documentação nos referiremos ao retorno de string vazias como "o silêncio como resposta".

## Aliases

Alias são amplamente utilizados em UltraGen. Devido a sua natureza de apenas associar valores atômicos a variáveis, todas as outras estruturas internas são referenciadas através de um alias. Você pode pensar em um alias como um nome de variável para acessar um tipo interno da linguagem. A vantagem é que como alias são strings, eles dispõem de todas as funções de strings para sua definição.

## Outros conceitos

UltraGen possui mais alguns conceitos para o seu uso, mas estes merecem artigos mais específicos sobre o assunto. São estes:

- Tipos de dados em UltraGen: *strings* e *listables*.
- Metaprogramação
- Funções e procedimentos
- Tempos de execução UltraGen

#### NOTA:

- Ao longo da documentação sempre que quisermos explicar algo sobre a forma como algum assunto é implementado pela linguagem termos um bloco sob o título **INSIDE**.

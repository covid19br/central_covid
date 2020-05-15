# Meta repo para dados e analises

Repo centralizado para organizar os repos de análise e de dados numa estrutura padronizada.

```bash
git clone git@github.com:covid19br/central_covid.git
cd central_covid
git submodule update --init
```


# Estrutura

O repositório central covid funciona como um agregador de todos os reposítórios relacionados às análises e divulgação do Observatório covid19. O uso de um meta-repo e git submodules permite conotrolar o acesso a dados sigilosos, mas disponibilizar todos os códigos e dados processados públicamente.

![Estrutura do repo central_covid](central_covid.png)

## Dados

Os repositorios sigilogoso devem ficar dentro da pasta dados, e pode ser ignorados pelos usuários sem acesso aos dados sigilosos. 

Para acrescentar um conjunto de dados novo, basta adicionar o repositorio novo na pasta de dados:

```
cd dados
git submodule add URL_do_repo_novo
```

## Fazendo mudanças nos sub-respostitórios e no meta-repostório

Todo o trabalho dentro das pastas site e nowcasting deve ser incluida nesses repos normalmente, com ```git add``` e ```git commit``` utilizados dentro das pastas dos sub-repositórios. Após mudanças nesses sub-repositórios, é necessário atualizar no meta-repostorio central_covid qual é o commit mais recente dos sub-repositorios. Isso é feito com um ```git add``` e ```git commit``` a partir do meta-repositório central.

## Trazendo mudanças recentes do servidor

É possível atualizar simultaneamente todos os sub-repositórios com:

```bash
git pull
git pull --recurse-submodules
```
# Meta repo para dados e analises

Repo centralizado para organizar as pastes de dados privados.
Deve ser clonado dentro do repo publico nowcast.

```bash
git clone git@github.com:covid19br/central_covid.git
cd central_covid
git submodule update --init
```
Depois, para atualizar:

```bash
git pull
git pull --recurse-submodules
```

# Estrutura

O repositório central covid funciona como um agregador de todos os reposítórios relacionados às análises e divulgação do Observatório covid19. O uso de um meta-repo e git submodules permite conotrolar o acesso a dados sigilosos, mas disponibilizar todos os códigos e dados processados públicamente.

![Estrutura do repo central_covid](central_covid.png)

## Dados

Os repositorios sigilogoso 

# Meta repo para dados 

Repo centralizado para organizar as pastes de dados privados.
Deve ser clonado dentro do repo publico nowcast.

```bash
git clone git@github.com:covid19br/central_covid_data.git
git submodule update --init
```
Depois, para atualizar:

```bash
git pull
git pull --recurse-submodules
```
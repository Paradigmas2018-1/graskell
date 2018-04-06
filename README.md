# GrasKell

## How to execute

1. Ensure you have Docker CE installed [See](https://docs.docker.com/install/)

2. Ensure you have Docker Compose installed [See](https://docs.docker.com/compose/install/)

3. Execute the following commands

``` bash
$ sudo docker-compose up --build
```

``` bash
$ sudo docker exec -it graskell ghci
```

4. Execute your haskell commands

_example_

- `:load Main`
- `kruskal graph`
- `kruskal anotherGraph`
- `scc graph`
- `scc anotherGraph`

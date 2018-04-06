# GrasKell

Sample Graph used in Main.hs

![](https://i0.wp.com/www.techiedelight.com/wp-content/uploads/2016/11/Kruskal-1.png)

## How to execute

1. Ensure you have Docker CE installed [See](https://docs.docker.com/install/)

2. Ensure you have Docker Compose installed [See](https://docs.docker.com/compose/install/)

3. Execute the following commands

``` bash
$ sudo docker-compose up --build -d
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

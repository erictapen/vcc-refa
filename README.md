Visit the most recently deployed live version at [https://erictapen.name/refa/](https://erictapen.name/refa/).

## Building and serving the site locally

```
$ nix develop
$ elm make --debug src/Main.elm  --output=assets/Main.min.js
```

Serving the site is then done with

```
$ nginx-refa
```

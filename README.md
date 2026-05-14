# Shiny_speMCA projectの使い方

このdocは、本プロジェクトの内容、使い方について記述する

## Project root
- ubuntu-miniの ~/ShinyApps/Shiny_speMCA

## アプリ本体

-　ui.R、server.R、global.Rにさん分割
-  app.R

##　Dockerのコンテナbuild

- libraryを追加した場合は、~/shiny-server-mgr の中のDockerfileに追記し、
　+ docker compose build
　+ docker compose up -d
　でコンテナをrebuildする

##　GitHub

```
https://github.com/419kfj/Shiny_speMCA_lite
```

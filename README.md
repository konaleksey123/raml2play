# Elbrus-play плагин

Особенности:

 - определять роуты в RAML формате, достаточно в conf создать файл *.raml (ссылка на реализацию - превоя строка в description, 
см. тесты для примера)
- поддерживаются все типы и ограничения из RAML 0.8
- даты в формате ISO, а не RFC!

## Использование

Для использования в проекте нужно:

 - добавить в project/plugins.sbt строчку `addSbtPlugin("bavadim" % "raml2play" % "0.0.1-SNAPSHOT")`
 - можно (но не обязательно) убрать строчку `addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3")`
 - в `build.sbt` добавить `enablePlugins(Raml2PlayPlugin)`


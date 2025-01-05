## .httr-oauth

Este fichero es con el que te identificas para conectarte con Dropbox. El bueno está en la maquina de Oracle en /home/ubuntu/.httr-oauth. NO LO PIERDAS NI LO MODIFIQUES BAJO NINGUN CONCEPTO (ahora Dropbox ya no admite este tipo de ficheros que se auto refrescan, te obliga a reidentificarte pasadas X horas manualmente, de ahí que sea muy importante no perderlo ni modificarlo). Es el que tienes que copiar dentro de shiny_meteo/ (y también subirlo a shinyapps.io). Verás que es un fichero que se autorefresca por lo que si haces un `ls -la` verás que la fecha del archivo es la última vez que se utilizase (no la fecha de creación del fichero como se podría pensar). En la máquina de Oracle esta fecha será reciente ya que es donde está el cron que se descarga el dato de AEMET y se sube a Dropbox todos los días.

Te puedes copiar el fichero de la máquina de Oracle a local con: `scp -i ./.ssh/oracle ubuntu@143.47.33.65:/home/ubuntu/.httr-oauth /Users/pcontreras/data/projects/shiny_meteo` (si no te consigue identificar prueba a reiniciar RStudio)

## .Renviron

Aqui dentro está el API KEY de AEMET para poder usar su API y tambien el token del bot de Telegram para poder recibir las alertas si hay fallos en el cron que se descarga el dato de AEMET y lo guarda en Dropbox. Este fichero también ha de estar dentro de shiny_meteo/ y en shinyapps.io.

## renv.lock

Este fichero guarda las versiones exactas de los paquetes utilizados así como la versión de R que se usa en el proyecto. No es recomendable que cambies las versiones de las librerias/paquetes ya que seguramente te descuadre los gráficos o algo deje de funcionar.

renv crea un entorno específico para cada proyecto, almacenando las versiones de los paquetes solo para ese proyecto. Los paquetes se instalan en una carpeta local dentro del directorio del proyecto, generalmente en `<project>/renv/library`. Si ejecutas `R` dentro de shiny_meteo usarás las versiones de los paquetes de renv.lock, si lo ejecutas fuera del proyecto estarás usando las versiones de los paquetes instalados globalmente en tu local.

## rsconnect::deployApp()

Si vas a desplegar de nuevo la app en shinyapps.io y has cambiado versiones de librerias en el renv.lock lo mejor es que subas la app con otro nombre para probar que todo funciona bien y no sobreescribas la que hay que sabes que funciona.

## AEMET api keys

Si quieres añadir más api keys de AEMET para evitar que los procesos se paren por el límite de peticiones por usuario, tienes que ejecutar `aemet_api_key(c(key1, key2, ..., key n), install=TRUE, overwrite=TRUE)`. Independiemente de que tambien tengas que añadir el API key nuevo al fichero .Renviron para la app de Shiny. Pero lo primero es para que quede instalado en las maquinas que ejecutan los procesos periodicos. De hecho en el fichero .Renviron con una sola valdría, porque la app de Shiny no hace consultas batch muy seguidas al servidor de AEMET

## Funcionamiento

Para cada estación existe un fichero con sufijo 'historical' que contiene todo el dato histórico de la estación. Este fichero se actualiza cada 6 meses con el script `aemet_api_hist.R` en un cron, añadiendo al histórico los últimos 6 meses de datos.

Como a veces AEMET modifica/corrige datos a pasado, en el script diario `aemet_api.R` que genera el fichero con el sufijo 'complete' (que es el que usa la app para pintar los gráficos) se juntan los datos de los últimos 4 días con los datos de los ficheros con sufijo 'last24h' + el dato de los últimos 12 meses importados por el API en el momento (por aquello de que AEMET puede corregir datos a pasado) + el dato histórico.

Como un día puede estar repetido al juntar los diferentes ficheros, se da prioridad al dato histórico, luego al dato de los últimos 12 meses y por último el de los últimos 4 días

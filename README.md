## .httr-oauth

Este fichero es con el que te identificas para conectarte con Dropbox. El bueno está en la maquina de Oracle en /home/ubuntu/.httr-oauth. NO LO PIERDAS NI LO MODIFIQUES BAJO NINGUN CONCEPTO (ahora Dropbox ya no admite este tipo de ficheros que se auto refrescan, te obliga a reidentificarte pasadas X horas manualmente, de ahí que sea muy importante no perderlo ni modificarlo). Es el que tienes que copiar dentro de shiny_meteo/ (y también subirlo a shinyapps.io). Verás que es un fichero que se autorefresca por lo que si haces un `ls -la` verás que la fecha del archivo es la última vez que se utilizase (no la fecha de creación del fichero como se podría pensar). En la máquina de Oracle esta fecha será reciente ya que es donde está el cron que se descarga el dato de AEMET y se sube a Dropbox todos los días.

Te puedes copiar el fichero de la máquina de Oracle a local con: `scp -i ./.ssh/oracle ubuntu@143.47.33.65:/home/ubuntu/.httr-oauth /Users/pcontreras/data/projects/shiny_meteo`

## .Renviron

Aqui dentro está el API KEY de AEMET para poder usar su API y tambien el del bot de Telegram para poder recibir las alertas si hay fallos en el cron que se descarga el dato de AEMET y lo guarda en Dropbox. Este fichero también ha de estar dentro de shiny_meteo/ y en shinyapps.io.

## renv.lock

Este fichero guarda las versiones exactas de los paquetes utilizados así como la versión de R que se usa en el proyecto. No es recomendable que cambies las versiones de las librerias/paquetes ya que seguramente te descuadre los gráficos o algo deje de funcionar.

renv crea un entorno específico para cada proyecto, almacenando las versiones de los paquetes solo para ese proyecto. Los paquetes se instalan en una carpeta local dentro del directorio del proyecto, generalmente en `<project>/renv/library`. Si ejecutas `R` dentro de shiny_meteo usarás las versiones de los paquetes de renv.lock, si lo ejecutas fuera del proyecto estarás usando las versiones de los paquetes instalados globalmente en tu local.

## rsconnect::deployApp()

Si vas a desplegar de nuevo la app en shinyapps.io y has cambiado versiones de librerias en el renv.lock lo mejor es que subas la app con otro nombre para probar que todo funciona bien y no sobreescribas la que hay que sabes que funciona.

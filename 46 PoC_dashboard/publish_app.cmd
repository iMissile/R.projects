@rem http://the.earth.li/~sgtatham/putty/0.67/htmldoc/Chapter5.html#pscp
@rem http://www.it.cornell.edu/services/managed_servers/howto/file_transfer/fileputty.cfm
@rem копируем файлы
pscp -pw F0rever! publish_app.cmd iot-rus@10.0.0.201:/home/iot-rus/ShinyApps/iot
pscp -pw F0rever! common_funcs.R iot-rus@10.0.0.201:/home/iot-rus/ShinyApps/iot
pscp -pw F0rever! ./shiny_06/app.R iot-rus@10.0.0.201:/home/iot-rus/ShinyApps/iot/shiny_05

@rem перезапускаем сервер
plink -v -ssh -pw F0rever! root@10.0.0.201 < publish_app.txt
@rem cd /etc/shiny-server/ 
@rem ls -a
@rem restart shiny-server
@rem exit
@rem

 

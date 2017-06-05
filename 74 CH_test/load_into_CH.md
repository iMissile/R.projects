[Яндекс открывает ClickHouse](https://habrahabr.ru/company/yandex/blog/303282/)

0. Запускаем клиент на локальной машине
clickhouse-client

1. В CH клиенте создаем таблицу, аналог mtcars

CREATE TABLE mtcars
(
    name  String,
    mpg Float32,
    cyl Float32,
    disp Float32,
    hp Float32,
    drat Float32,
    wt Float32,
    qsec Float32,
    vs Float32,
    am Float32,
    gear Int32,
    carb Int32
) ENGINE = Memory

Задание [движка](https://clickhouse.yandex/docs/ru/table_engines/index.html) является обязательным

2. Посмотрим на таблицы
SHOW TABLES

3. Загружаем данные в таблицу

Запускаем в консоли linux
@rem xz -v -c -d < ontime.csv.xz | clickhouse-client --query="INSERT INTO ontime FORMAT CSV"

cat mtcars.csv | clickhouse-client --query="INSERT INTO mtcars FORMAT CSV"

4. Посмотрим таблицу в клиенте
SELECT * FROM mtcars

5. Посмотрим таблицу в браузере на клиентской машине:
http://10.0.0.234:8123/?query=SELECT%20*%20FROM%20mtcars

3. Удаляем таблицу
DROP TABLE mtcars
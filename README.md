Бизнес-требования:
У нас есть коты и города
часть котов находится в городе, другая часть вне города (в дороге)
кот не может быть в двух городах одновременно
в одном городе могут быть одновременно несколько котов

АПИ:
поселить кота в город
выселить кота из города
узнать, находится ли данный кот в данном городе
узнать, в каком городе находится данный кот
узнать, какие коты находятся в данном городе

TODO: формальное описание АПИ:
аргументы, возвращаемые значения, возможные ошибки

Система распределенная, запрос клиента может попасть на любую реплику. Считаем, что впереди стоит довольно глупый балансировщик, который раскидывает запросы случайно, не учитывая клиентские сессии.
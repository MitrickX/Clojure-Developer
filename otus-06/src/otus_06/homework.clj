(ns otus-06.homework)

;; Загрузить данные из трех файлов на диске.
;; Эти данные сформируют вашу базу данных о продажах.
;; Каждая таблица будет иметь «схему», которая указывает поля внутри.
;; Итак, ваша БД будет выглядеть так:

;; cust.txt: это данные для таблицы клиентов. Схема:
;; <custID, name, address, phoneNumber>

;; Примером файла cust.txt может быть:
;; 1|John Smith|123 Here Street|456-4567
;; 2|Sue Jones|43 Rose Court Street|345-7867
;; 3|Fan Yuhong|165 Happy Lane|345-4533

;; Каждое поле разделяется символом «|». и содержит непустую строку.

;; prod.txt: это данные для таблицы продуктов. Схема
;; <prodID, itemDescription, unitCost>

;; Примером файла prod.txt может быть:
;; 1|shoes|14.96
;; 2|milk|1.98
;; 3|jam|2.99
;; 4|gum|1.25
;; 5|eggs|2.98
;; 6|jacket|42.99

;; sales.txt: это данные для основной таблицы продаж. Схема:
;; <salesID, custID, prodID, itemCount>.
;;
;; Примером дискового файла sales.txt может быть:
;; 1|1|1|3
;; 2|2|2|3
;; 3|2|1|1
;; 4|3|3|4

;; Например, первая запись (salesID 1) указывает, что Джон Смит (покупатель 1) купил 3 пары обуви (товар 1).

;; Задача:
;; Предоставить следующее меню, позволяющее пользователю выполнять действия с данными:

;; *** Sales Menu ***
;; ------------------
;; 1. Display Customer Table
;; 2. Display Product Table
;; 3. Display Sales Table
;; 4. Total Sales for Customer
;; 5. Total Count for Product
;; 6. Exit

;; Enter an option?


;; Варианты будут работать следующим образом

;; 1. Вы увидите содержимое таблицы Customer. Вывод должен быть похож (не обязательно идентичен) на

;; 1: ["John Smith" "123 Here Street" "456-4567"]
;; 2: ["Sue Jones" "43 Rose Court Street" "345-7867"]
;; 3: ["Fan Yuhong" "165 Happy Lane" "345-4533"]

;; 2. То же самое для таблицы prod.

;; 3. Таблица продаж немного отличается.
;;    Значения идентификатора не очень полезны для целей просмотра,
;;    поэтому custID следует заменить именем клиента, а prodID — описанием продукта, как показано ниже:
;; 1: ["John Smith" "shoes" "3"]
;; 2: ["Sue Jones" "milk" "3"]
;; 3: ["Sue Jones" "shoes" "1"]
;; 4: ["Fan Yuhong" "jam" "4"]

;; 4. Для варианта 4 вы запросите у пользователя имя клиента.
;;    Затем вы определите общую стоимость покупок для этого клиента.
;;    Итак, для Сью Джонс вы бы отобразили такой результат:
;; Sue Jones: $20.90

;;    Это соответствует 1 паре обуви и 3 пакетам молока.
;;    Если клиент недействителен, вы можете либо указать это в сообщении, либо вернуть $0,00 за результат.

;; 5. Здесь мы делаем то же самое, за исключением того, что мы вычисляем количество продаж для данного продукта.
;;    Итак, для обуви у нас может быть:
;; Shoes: 4

;;    Это представляет три пары для Джона Смита и одну для Сью Джонс.
;;    Опять же, если продукт не найден, вы можете либо сгенерировать сообщение, либо просто вернуть 0.

;; 6. Наконец, если выбрана опция «Выход», программа завершится с сообщением «До свидания».
;;    В противном случае меню будет отображаться снова.


;; *** Дополнительно можно реализовать возможность добавлять новые записи в исходные файлы
;;     Например добавление нового пользователя, добавление новых товаров и новых данных о продажах


;; Файлы находятся в папке otus-06/resources/homework

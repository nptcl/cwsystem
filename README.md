# cwsystem

Print cardinal or ordinal English numbers by The Conway-Wechsler System in Common Lisp.


## Function `radix-string`.

The function like a `(format nil "~R~ x)` but the argument can be a large number.

```
(cwsystem:radix-string 123)
-> "one hundred twenty-three"

;; minus
(cwsystem:radix-string -4)
-> "minus four"

;; ordinal number
(cwsystem:radix-string 20 nil)
-> "twentieth"

;; large number
(cwsystem:radix-string (ash 1 200))
-> "one novendecillion six hundred six octodecillion nine hundred thirty-eight septendecillion forty-four sedecillion two hundred fifty-eight quindecillion nine hundred ninety quattuordecillion two hundred seventy-five tredecillion five hundred forty-one duodecillion nine hundred sixty-two undecillion ninety-two decillion three hundred forty-one nonillion one hundred sixty-two octillion six hundred two septillion five hundred twenty-two sextillion two hundred two quintillion nine hundred ninety-three quadrillion seven hundred eighty-two trillion seven hundred ninety-two billion eight hundred thirty-five million three hundred one thousand three hundred seventy-six"
```


## Function `unit-string`.

The function returns a short scale name of large numbers.

```
(cwsystem:unit-string 0)
-> "thousand"

(cwsystem:unit-string 1)
-> "million"

(cwsystem:unit-string 2)
-> "billion"

(cwsystem:unit-string 789)
-> "novemoctogintaseptingentillion"

;; ordinal
(cwsystem:unit-string 345 nil)
-> "quinquadragintatrecentillionth"

(cwsystem:unit-string 1234567890)
-> "milliquattuortrigintaducentilliseptensexagintaquingentillinonagintaoctingentillion"
```


## License

[The Unlicense](LICENSE)


## Distribution

https://github.com/nptcl/cwsystem


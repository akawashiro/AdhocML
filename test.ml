let f = fun x -> x in f;;
fun x -> x;;
let f = fun x -> x in if f True then f 10 else 1;;
let rec f = fun x -> if 10 < x then 1 else  x * f (x + 1) in f 1;;
let f = 10 in f;;
2 * 2;;
19;;
let f = fun x -> fun y -> x + y in f;;

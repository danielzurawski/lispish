function is_prime(num) {return  (function(prime_over_two) { return ((num<2)?false:((2==num)?true:((0==(num%2))?false:prime_over_two(num, 3)))) })(function (num, factor) {return ((factor>Math.sqrt((num))) ? (true):(((0==(num%factor)) ? (false):(arguments.callee(num, (2+factor))))))})}
function pi() {return 3.1415926535}
function double(x) {return (x*2)}
function celcius_to_kelvin(cel) {return (cel+273.16)}

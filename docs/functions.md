```
>> let pow = fn(x, y) { x * y };
>> pow(2, 512)
1024
>> fn(x) { x / 100 }(10)
0.1
>> let Adder = fn(x) { fn(y) { x + y }; };
>> let add = Adder(208);
>> add(131);
339
>> let sub = fn(a, b) { a - b };
>> let applyFunc = fn(a, b, func) { func(a, b) };
>> applyFunc(100, 2, sub);
98
```

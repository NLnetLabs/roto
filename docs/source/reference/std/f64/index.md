# f64
`````{roto:type} f64
The 64-bit floating point type.
`````


````{roto:method} f64.abs(self: f64) -> f64
Computes the absolute value of self.
````

````{roto:method} f64.ceil(self: f64) -> f64
Returns the smallest integer greater than or equal to self.
````

````{roto:method} f64.floor(self: f64) -> f64
Returns the smallest integer greater than or equal to self.
````

````{roto:method} f64.is_finite(self: f64) -> bool
Returns true if this number is neither infinite nor NaN.
````

````{roto:method} f64.is_infinite(self: f64) -> bool
Returns true if this value is positive infinity or negative infinity, and false otherwise.
````

````{roto:method} f64.is_nan(self: f64) -> bool
Returns true if this value is NaN.
````

````{roto:method} f64.pow(self: f64, exp: f64) -> f64
Raises a number to a floating point power.
````

````{roto:method} f64.round(self: f64) -> f64
Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
````

````{roto:method} f64.sqrt(self: f64) -> f64
Returns the square root of a number.
````

````{roto:method} f64.to_string(self: f64) -> String
Convert this value into a `String`.
````


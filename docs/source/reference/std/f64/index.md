# f64
`````{roto:type} f64
The 64-bit floating point type
`````


````{roto:function} abs(x: f64) -> f64
Computes the absolute value of self.
````

````{roto:function} ceil(x: f64) -> f64
Returns the smallest integer greater than or equal to self.
````

````{roto:function} floor(x: f64) -> f64
Returns the smallest integer greater than or equal to self.
````

````{roto:function} is_finite(x: f64) -> bool
Returns true if this number is neither infinite nor NaN.
````

````{roto:function} is_infinite(x: f64) -> bool
Returns true if this value is positive infinity or negative infinity, and false otherwise.
````

````{roto:function} is_nan(x: f64) -> bool
Returns true if this value is NaN.
````

````{roto:function} pow(x: f64, y: f64) -> f64
Raises a number to a floating point power.
````

````{roto:function} round(x: f64) -> f64
Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
````

````{roto:function} sqrt(x: f64) -> f64
Returns the square root of a number.
````

````{roto:function} to_string(x: f64) -> String
Convert this value into a `String`
````


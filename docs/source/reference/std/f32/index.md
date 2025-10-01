# f32
`````{roto:type} f32
The 32-bit floating point type
`````


````{roto:function} abs(x: f32) -> f32
Computes the absolute value of self.
````

````{roto:function} ceil(x: f32) -> f32
Returns the smallest integer greater than or equal to self.
````

````{roto:function} floor(x: f32) -> f32
Returns the smallest integer greater than or equal to self.
````

````{roto:function} is_finite(x: f32) -> bool
Returns true if this number is neither infinite nor NaN.
````

````{roto:function} is_infinite(x: f32) -> bool
Returns true if this value is positive infinity or negative infinity, and false otherwise.
````

````{roto:function} is_nan(x: f32) -> bool
Returns true if this value is NaN.
````

````{roto:function} pow(x: f32, y: f32) -> f32
Raises a number to a floating point power.
````

````{roto:function} round(x: f32) -> f32
Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
````

````{roto:function} sqrt(x: f32) -> f32
Returns the square root of a number.
````

````{roto:function} to_string(x: f32) -> String
Convert this value into a `String`
````


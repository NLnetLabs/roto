# f32
`````{roto:type} f32
The 32-bit floating point type.
`````


````{roto:method} f32.abs(self: f32) -> f32
Computes the absolute value of self.
````

````{roto:method} f32.ceil(self: f32) -> f32
Returns the smallest integer greater than or equal to self.
````

````{roto:method} f32.floor(self: f32) -> f32
Returns the smallest integer greater than or equal to self.
````

````{roto:method} f32.is_finite(self: f32) -> bool
Returns true if this number is neither infinite nor NaN.
````

````{roto:method} f32.is_infinite(self: f32) -> bool
Returns true if this value is positive infinity or negative infinity, and false otherwise.
````

````{roto:method} f32.is_nan(self: f32) -> bool
Returns true if this value is NaN.
````

````{roto:method} f32.pow(self: f32, exp: f32) -> f32
Raises a number to a floating point power.
````

````{roto:method} f32.round(self: f32) -> f32
Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
````

````{roto:method} f32.sqrt(self: f32) -> f32
Returns the square root of a number.
````

````{roto:method} f32.to_string(self: f32) -> String
Convert this value into a `String`.
````


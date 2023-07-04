#[macro_export]
macro_rules! typedefconversion {
    ( 
        $( $type_no_data:ident ( $( $into_type:ident ),* ; $( $into_type_nest:ident ),* ) ),* ;
        $( $type_data:ident ( $( $into_type_data:ident ),* ; $( $into_type_nest_data:ident ),* ) ),* ;
        $( $no_conversion_no_data:ident ),* ;
        $( $no_conversion_data:ident ),*
    ) => {
        pub fn test_type_conversion(
            self,
            into_ty: TypeDef,
        ) -> bool {
            match self {
                $(
                    TypeDef::$type_no_data => {
                        return match into_ty {
                            $( 
                                TypeDef::$into_type => true,
                            )*
                            $(
                                TypeDef::$into_type_nest(_) => true,
                            )*
                            _ => false
                        };
                    }
                )*
                $(
                    TypeDef::$type_data(_) => {
                        return match into_ty {
                            $( 
                                TypeDef::$into_type_data => true,
                            )*
                            $(
                                TypeDef::$into_type_nest_data(_) => true,
                            )*
                            _ => false
                        };
                    }
                )*
                $(
                    TypeDef::$no_conversion_no_data => false,
                )*
                $(
                    TypeDef::$no_conversion_data(_) => false,
                )*
            }
        }
    }
}

#[macro_export]
macro_rules! createtoken {
    (
        $token_enum: ident;
        $( $token: ident = $value: literal ),*
    ) => {
        #[derive(Debug)]
        enum $token_enum {
            $( $token = $value, )*
        }

        impl From<usize> for $token_enum {
            fn from(val: usize) -> Self {
                match val {
                    $( $value => $token_enum::$token, )*
                    _ => panic!("Unknown token value: {}", val),
                }
            }
        }

        impl From<$token_enum> for usize {
            fn from(val: $token_enum) -> Self {
                match val {
                    $( $token_enum::$token => $value, )*
                }
            }
        }
    }
}

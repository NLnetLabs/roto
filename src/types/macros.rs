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

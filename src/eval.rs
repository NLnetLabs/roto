use log::trace;

use crate::ast::AcceptReject;
use crate::ast::FilterType;
use crate::ast::LogicalExpr;
use crate::ast::MatchActionExpr;
use crate::ast::MatchOperator;
use crate::ast::ShortString;
use crate::ast::TypeIdentField;
use crate::blocks::Scope;
use crate::compiler::compile::CompileError;
use crate::first_into_compile_err;
use crate::symbols::GlobalSymbolTable;
use crate::symbols::MatchActionType;
use crate::symbols::Symbol;
use crate::symbols::SymbolKind;
use crate::traits::Token;
use crate::types::builtin::BuiltinTypeValue;
use crate::types::enum_types::GlobalEnumTypeDef;
use crate::types::typedef::NamedTypeDef;
use crate::types::typedef::RecordTypeDef;

use super::ast;
use super::symbols;

use super::types::typedef::TypeDef;
use super::types::typevalue::TypeValue;

use std::convert::From;

impl<'a> ast::SyntaxTree {
    pub fn eval(
        &'a self,
        symbols: GlobalSymbolTable,
    ) -> Result<(), CompileError> {
        // At this level there's no difference between a a roto `filter` and a
        // `filter-map`. They are all FilterMap structs.
        let (filter_maps, global): (Vec<_>, Vec<_>) = self
            .expressions
            .iter()
            .partition(|e| matches!(e, ast::RootExpr::FilterMap(_)));

        let mut acc = vec![];
        for filter_map in &filter_maps {
            if let Ok(fm) = filter_map.get_filter_map() {
                if acc
                    .iter()
                    .any(|expr: &&ShortString| *expr == &fm.ident.ident)
                {
                    return Err(CompileError::from(format!(
                        "Duplicate FilterMap with name '{}'",
                        fm.ident.ident
                    )));
                }
                acc.push(&fm.ident.ident);
            } else {
                return Err(CompileError::from(
                    "Cannot accept FilterMap without a name",
                ));
            }
        }

        // First, evaluate all the non-filter-map expressions, so that they are
        // available to the filter_maps.

        // If the global symbol table does not exist, create it.
        let mut symbols_mut = symbols.borrow_mut();
        let global_scope = Scope::Global;

        let global_symbols = if symbols_mut.contains_key(&global_scope) {
            symbols_mut.get_mut(&global_scope).ok_or_else(|| {
                CompileError::from("Cannot find global scope.")
            })?
        } else {
            let mut global_table = symbols::SymbolTable::new(&global_scope);
            global_table.create_global_methods();

            symbols_mut.insert(global_scope.clone(), global_table);
            symbols_mut.get_mut(&global_scope).ok_or_else(|| {
                CompileError::from("Cannot find global scope.")
            })?
        };

        for expr in &global {
            match expr {
                ast::RootExpr::Rib(rib) => rib.eval(global_symbols)?,
                ast::RootExpr::Table(table) => table.eval(global_symbols)?,
                ast::RootExpr::OutputStream(stream) => {
                    stream.eval(global_symbols)?
                }
                ast::RootExpr::Ty(rt_assign) => {
                    rt_assign.eval(global_symbols)?
                }
                _ => {}
            };
        }

        // For each filter_map, create a new symbol table if it does not exist.
        for filter_map in &filter_maps {
            trace!("found filter_map: {:?}", filter_map);
            let filter_map_name = &filter_map.get_filter_map()?.ident.ident;
            let filter_map_scope = match filter_map.get_filter_map()?.ty {
                FilterType::Filter => Scope::Filter(filter_map_name.clone()),
                FilterType::FilterMap => {
                    Scope::FilterMap(filter_map_name.clone())
                }
            };

            if let std::collections::hash_map::Entry::Vacant(e) =
                symbols_mut.entry(filter_map_scope.clone())
            {
                e.insert(symbols::SymbolTable::new(&filter_map_scope));
                symbols_mut.get_mut(&global_scope).ok_or_else(|| {
                    CompileError::from("Cannot find global scope.")
                })?
            } else {
                symbols_mut.get_mut(&filter_map_scope).ok_or_else(|| {
                    CompileError::from(format!(
                        "Cannot find scope {}.",
                        filter_map_scope
                    ))
                })?
            };
        }
        drop(symbols_mut);

        // Now, evaluate all the define sections in modules, so that filter_maps
        // can use each other's types and variables.
        for filter_map in &filter_maps {
            if let ast::RootExpr::FilterMap(m) = filter_map {
                m.eval_define_header(symbols.clone())?;
            }
        }

        // Finally, evaluate all the filter_maps themselves.
        for filter_map in &filter_maps {
            if let ast::RootExpr::FilterMap(m) = filter_map {
                m.eval(symbols.clone())?;
            }
        }
        trace!("Evaluated successfully");

        Ok(())
    }
}

impl<'a> ast::Rib {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the RIB
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_variable(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            TypeValue::Unknown,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_variable(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::Rib,
            rec_type,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(())
    }
}

impl<'a> ast::Table {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the table
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_variable(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            TypeValue::Unknown,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_variable(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::Table,
            rec_type,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(())
    }
}

impl<'a> ast::OutputStream {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the table
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_variable(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            TypeValue::Unknown,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_variable(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::OutputStream,
            rec_type,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(())
    }
}

impl<'a> ast::RecordTypeAssignment {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        self.record_type.eval(
            self.ident.ident.clone(),
            symbols::SymbolKind::NamedType,
            symbols,
        )?;

        Ok(())
    }
}

impl<'a> ast::RibBody {
    fn eval(
        &'a self,
        parent_name: ast::ShortString,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<Vec<NamedTypeDef>, CompileError> {
        let mut kvs: Vec<NamedTypeDef> = vec![];

        for kv in self.key_values.iter() {
            match kv {
                ast::RibField::PrimitiveField(f) => {
                    kvs.push((
                        f.field_name.ident.as_str().into(),
                        Box::new(f.ty.clone().try_into()?),
                    ));
                }
                ast::RibField::RecordField(r) => {
                    let nested_record = ast::RecordTypeIdentifier::eval(
                        &r.1,
                        format!(
                            "{}.{}",
                            parent_name,
                            String::from(r.0.ident.as_str()).to_lowercase()
                        )
                        .as_str()
                        .into(),
                        symbols::SymbolKind::AnonymousType,
                        symbols,
                    )?;

                    kvs.push((
                        r.0.ident.as_str().into(),
                        Box::new(TypeDef::Record(RecordTypeDef::new(
                            nested_record,
                        ))),
                    ));
                }
                ast::RibField::ListField(l) => {
                    kvs.push((
                        l.0.ident.as_str().into(),
                        Box::new(TypeDef::List(Box::new(
                            l.1.inner_type.clone().try_into()?,
                        ))),
                    ));
                }
            }
        }

        Ok(kvs)
    }
}

impl<'a> ast::RecordTypeIdentifier {
    fn eval(
        &'a self,
        name: ast::ShortString,
        kind: symbols::SymbolKind,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<Vec<NamedTypeDef>, CompileError> {
        let mut kvs: Vec<NamedTypeDef> = vec![];

        for kv in self.key_values.iter() {
            match kv {
                ast::RibField::PrimitiveField(f) => {
                    kvs.push((
                        f.field_name.ident.as_str().into(),
                        Box::new(f.ty.clone().try_into()?),
                    ));
                }
                ast::RibField::RecordField(r) => {
                    let nested_record = ast::RecordTypeIdentifier::eval(
                        &r.1,
                        format!(
                            "{}.{}",
                            name,
                            String::from(r.0.ident.as_str()).to_lowercase()
                        )
                        .as_str()
                        .into(),
                        symbols::SymbolKind::AnonymousType,
                        symbols,
                    )?;

                    kvs.push((
                        r.0.ident.as_str().into(),
                        Box::new(TypeDef::Record(RecordTypeDef::new(
                            nested_record,
                        ))),
                    ));
                }
                ast::RibField::ListField(l) => {
                    kvs.push((
                        l.0.ident.as_str().into(),
                        Box::new(TypeDef::List(Box::new(
                            l.1.inner_type.clone().try_into()?,
                        ))),
                    ));
                }
            }
        }

        let record = TypeDef::Record(RecordTypeDef::new(kvs.clone()));
        symbols.add_variable(
            name,
            None,
            kind,
            record,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(kvs)
    }
}

impl ast::FilterMap {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
    ) -> Result<(), CompileError> {
        let filter_map_scope = match &self.ty {
            FilterType::FilterMap => {
                Scope::FilterMap(self.ident.ident.clone())
            }
            FilterType::Filter => Scope::Filter(self.ident.ident.clone()),
        };
        // Check the `with` clause for additional arguments.
        let with_kv: Vec<_> = self.with_kv.clone();
        let _with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_argument(
                    ty.clone().field_name.ident,
                    ty,
                    symbols::SymbolKind::Constant,
                    symbols.clone(),
                    &filter_map_scope,
                )
            })
            .collect::<Vec<_>>();

        // first, parse the define section, so that other sections in this
        // filter_map can use the defined variables.
        self.body.define.eval(
            self.ty,
            symbols.clone(),
            filter_map_scope.clone(),
        )?;

        let (term_sections, action_sections): (Vec<_>, Vec<_>) = self
            .body
            .expressions
            .iter()
            .partition(|s| matches!(s, ast::FilterMapExpr::Term(_t)));

        for (index, term_section) in term_sections.into_iter().enumerate() {
            if let ast::FilterMapExpr::Term(t) = term_section {
                match &first_into_compile_err!(t.body.scopes)?.operator {
                    // A regular term expression is basically a bunch
                    // of logical expressions.
                    MatchOperator::Match => {
                        t.eval(
                            index,
                            symbols.clone(),
                            filter_map_scope.clone(),
                        )?;
                    }
                    // A `match `enum_ident` with` expression is a match
                    // expression and will contain a logical expression per
                    // variant in a vec
                    MatchOperator::MatchValueWith(enum_ident) => {
                        t.eval_as_match_expression(
                            enum_ident,
                            index,
                            symbols.clone(),
                            filter_map_scope.clone(),
                        )?;
                    }
                    MatchOperator::Some => todo!(),
                    MatchOperator::ExactlyOne => todo!(),
                    MatchOperator::All => todo!(),
                }
            }
        }

        for (index, action) in action_sections.into_iter().enumerate() {
            if let ast::FilterMapExpr::Action(a) = action {
                a.eval(index, symbols.clone(), filter_map_scope.clone())?;
            }
        }

        if let Some(apply) = &self.body.apply {
            apply.eval(symbols, filter_map_scope)?;
        }

        Ok(())
    }

    fn eval_define_header(
        &self,
        symbols: symbols::GlobalSymbolTable,
    ) -> Result<(), CompileError> {
        // Check the `with` clause for additional arguments.
        let with_kv: Vec<_> = self.body.define.with_kv.clone();

        // The `with` clause of the `define` section acts as an extra
        // argument to the whole filter_map, that can be used as a extra
        // read-only payload.
        let scope = match self.ty {
            FilterType::FilterMap => {
                Scope::FilterMap(self.ident.ident.clone())
            }
            FilterType::Filter => Scope::Filter(self.ident.ident.clone()),
        };

        let _with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_argument(
                    ty.clone().field_name.ident,
                    ty,
                    symbols::SymbolKind::Argument,
                    symbols.clone(),
                    &scope,
                )
            })
            .collect::<Vec<_>>();

        Ok(())
    }
}

impl ast::Define {
    fn eval(
        &self,
        filter_type: FilterType,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
    ) -> Result<(), CompileError> {
        // The default input-argument is defined by the 'rx' keyword in the
        // `define` section. This the argument that holds the payload at
        // runtime.
        let rx_kind;
        let rx_type = match &self.body.rx_tx_type {
            ast::RxTxType::Split(rx_type, _tx_type) => {
                if filter_type.is_filter() {
                    return Err(CompileError::from("Filter does not accept a type for 'tx'. Specify only a 'rx' in the Define section for a filter"));
                }
                rx_kind = SymbolKind::SplitRxType;
                rx_type
            }
            ast::RxTxType::PassThrough(rx_tx_type) => {
                if filter_type.is_filter() {
                    return Err(CompileError::from("Filter does not accept a type for 'rx_tx'. Specify only a 'rx' in the Define section for a filter"));
                }
                rx_kind = SymbolKind::PassThroughRxTxType;
                rx_tx_type
            }
            ast::RxTxType::RxOnly(rx_type) => {
                if !filter_type.is_filter() {
                    return Err(CompileError::from("FilterMap is missing the 'tx' type. Specify both 'rx' and 'tx' variables, or a `rx_tx` variable in the Define section for a filter. Alternatively you may specify a filter"));
                }
                rx_kind = SymbolKind::PassThroughRxTxType;
                rx_type
            }
        };

        declare_argument(
            rx_type.field_name.ident.clone(),
            rx_type.clone(),
            rx_kind,
            symbols.clone(),
            &scope,
        )?;

        // The default output-argument is defined by the 'tx' keyword in the
        // 'define' section. This is the argument that will be created by
        // this filter-filter-map on each run. We start with an empty record of
        // the specified type.
        let tx_kind;
        match &self.body.rx_tx_type {
            ast::RxTxType::Split(_rx_type, tx_type) => {
                tx_kind = SymbolKind::SplitTxType;
                declare_argument(
                    tx_type.field_name.ident.clone(),
                    tx_type.clone(),
                    tx_kind,
                    symbols.clone(),
                    &scope,
                )?;
            }
            ast::RxTxType::PassThrough(rx_tx_type) => {
                assert!(check_type_identifier(
                    rx_tx_type.ty.clone(),
                    symbols.clone(),
                    &scope
                )
                .is_ok());
            }
            ast::RxTxType::RxOnly(rx_type) => {
                assert!(check_type_identifier(
                    rx_type.ty.clone(),
                    symbols.clone(),
                    &scope
                )
                .is_ok());
            }
        };

        for assignment in &self.body.assignments {
            // rhs part of the assignment can only be an Argument Expression.
            let s = ast::ValueExpr::eval(
                &assignment.1,
                symbols.clone(),
                scope.clone(),
                &[],
            )?;

            // we only allow typed record instances, an anonymous type would
            // be ambiguous, since we don't know the different contexts where
            // it will be used: the type inference may lead to a different
            // type in different contexts, and then the type wouldn't be
            // equal to itself, which doesn't sound good (pun!).
            if let Token::AnonymousRecord = s.get_token() {
                return Err(CompileError::from(
                    format!(
                        "Assignment to Anonymous Record type not allowed in `define` section for variable '{}'.",
                        assignment.0.ident
                )));
            }

            trace!("DECLARE VAR {} = {:#?}", assignment.0.ident, s);
            // lhs of the assignment represents the name of the variable or
            // constant.
            declare_variable_from_symbol(
                assignment.0.ident.clone(),
                s,
                symbols.clone(),
                &scope,
            )?;
        }

        Ok(())
    }
}

impl ast::TermSection {
    fn eval(
        &self,
        term_section_index: usize,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
    ) -> Result<(), CompileError> {
        // A regular term expressions starting with `match` only is a
        // collection of logical expressions in one block, but
        // wrapped in a vec (so with only one element). A scope is
        // a bunch of logical expressions separated by a `use`
        // statement.
        trace!("term symbol to evaluate :");
        trace!("{:#?}", self);

        let mut local_scope: Vec<Symbol> = vec![];
        let mut argument_type = TypeDef::Unknown;
        // There may be a local scope defined in a `with <ARGUMENT_ID>`
        // in the term header.
        if let Some(TypeIdentField { field_name, ty }) = self.with_kv.get(0) {
            // Does the supplied type exist in our scope?
            argument_type =
                check_type_identifier(ty.clone(), symbols.clone(), &scope)?;
            trace!("type {}", ty);

            // for now we only accept one `with` argument for a term-
            // section, so the index to the token is always 0.
            local_scope.push(symbols::Symbol::new(
                field_name.clone().ident,
                SymbolKind::Constant,
                argument_type.clone(),
                vec![],
                Token::TermArgument(term_section_index, 0),
            ))
        }

        trace!("local scope");
        trace!("{:#?}", local_scope);

        // We currently only look at the first scope
        let term_scopes = first_into_compile_err!(self.body.scopes)?;

        for term in term_scopes.match_arms.iter().map(|me| &me.1[0]) {
            let mut logical_formula = match &term {
                LogicalExpr::BooleanExpr(expr) => {
                    // Boolean expressions may actually be a (sub)term that
                    // isn't a boolean at this stage. We should be able to
                    // convert it into one, though, otherwise it's an
                    // error (and not false!).
                    let expr = ast::BooleanExpr::eval(
                        expr,
                        symbols.clone(),
                        &scope,
                        &local_scope,
                    )?;
                    if expr.get_type() == TypeDef::Bool
                        || expr
                            .get_type()
                            .test_type_conversion(TypeDef::Bool)
                    {
                        expr
                    } else {
                        return Err(CompileError::from(format!(
                            "Cannot convert value with type {} into Boolean",
                            expr.get_type()
                        )));
                    }
                }
                LogicalExpr::OrExpr(or_expr) => ast::OrExpr::eval(
                    or_expr,
                    symbols.clone(),
                    &scope,
                    &local_scope,
                )?,
                LogicalExpr::AndExpr(and_expr) => ast::AndExpr::eval(
                    and_expr,
                    symbols.clone(),
                    &scope,
                    &local_scope,
                )?,
                LogicalExpr::NotExpr(not_expr) => ast::NotExpr::eval(
                    not_expr,
                    symbols.clone(),
                    &scope,
                    &local_scope,
                )?,
            };

            // The return type of the logical formula is boolean, which is
            // in itself correct, but boring, because a TermSection always
            // has a bool as return type. We are repurposing this field to
            // hold the type of the `with` argument for this section. So that
            // that type can be used when invoking a code block for
            // retrieving that argument.
            logical_formula = logical_formula.set_type(argument_type.clone());
            add_logical_formula(
                self.ident.ident.clone(),
                term_section_index,
                logical_formula,
                symbols.clone(),
                &scope,
            )?;
        }
        Ok(())
    }

    fn eval_as_match_expression(
        &self,
        enum_ident: &ast::Identifier,
        term_section_index: usize,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
    ) -> Result<(), CompileError> {
        // A match expressions is a collection of logical expressions, one
        // per variant.
        let term_scopes = first_into_compile_err!(self.body.scopes)?;

        trace!("enum {}", enum_ident);

        // check if the enum makes sense as an AccessReceiver
        let mut enum_s = ast::AccessReceiver::Ident(enum_ident.clone())
            .eval(symbols.clone(), scope.clone(), &[])
            .map_err(|_| {
                CompileError::from(format!(
                    "Variable '{}' (supposedly an Enum) cannot be found in \
                    this scope.",
                    enum_ident
                ))
            })?;
        enum_s = enum_s
            .set_type(TypeDef::GlobalEnum(GlobalEnumTypeDef::BmpMessageType));
        enum_s = enum_s.set_kind(symbols::SymbolKind::GlobalEnum);
        trace!("symbol {:#?}", enum_s);

        // loop over all the variants for this enum. AccessReceiver::eval
        // checks for the existence of the identifier.
        for (variant, logic_exprs) in term_scopes.match_arms.iter() {
            if let Some(variant) = variant {
                trace!(
                    "{}({:?}) -> {:#?}",
                    variant.variant_id,
                    variant.data_field,
                    logic_exprs
                );

                if let TypeDef::GlobalEnum(e_num) = enum_s.get_type() {
                    let (variant_type_def, variant_token) =
                        e_num.get_props_for_variant(&variant.variant_id)?;

                    let local_scope = vec![symbols::Symbol::new(
                        variant.data_field.clone().ok_or(CompileError::from(
                            format!("Variant: '{}' has no data field. This is currently not allowed.", 
                            variant.variant_id)
                        ))?.ident,
                        symbols::SymbolKind::VariableAssignment,
                        variant_type_def.clone(),
                        vec![],
                        variant_token.clone(),
                    )];

                    // extract the logical expressions for this variant
                    let mut logic_args = vec![];
                    for logic_expr in logic_exprs {
                        match logic_expr {
                            LogicalExpr::BooleanExpr(expr) => {
                                // Boolean expressions may actually be a (sub)term that
                                // isn't a boolean at this stage. We should be able to
                                // convert it into one, though, otherwise it's an
                                // error (and not false!).
                                let expr = ast::BooleanExpr::eval(
                                    expr,
                                    symbols.clone(),
                                    &scope,
                                    &local_scope,
                                )?;
                                if expr.get_type() == TypeDef::Bool
                                    || expr.get_type().test_type_conversion(
                                        TypeDef::Bool,
                                    )
                                {
                                    logic_args.push(expr);
                                } else {
                                    return Err(CompileError::from(format!(
                                        "Cannot convert value with type {} \
                                        into Boolean",
                                        expr.get_type()
                                    )));
                                }
                            }
                            LogicalExpr::OrExpr(or_expr) => {
                                logic_args.push(ast::OrExpr::eval(
                                    or_expr,
                                    symbols.clone(),
                                    &scope,
                                    &local_scope,
                                )?);
                            }
                            LogicalExpr::AndExpr(and_expr) => {
                                logic_args.push(ast::AndExpr::eval(
                                    and_expr,
                                    symbols.clone(),
                                    &scope,
                                    &local_scope,
                                )?);
                            }
                            LogicalExpr::NotExpr(not_expr) => {
                                logic_args.push(ast::NotExpr::eval(
                                    not_expr,
                                    symbols.clone(),
                                    &scope,
                                    &local_scope,
                                )?);
                            }
                        }
                    }
                    trace!("logical expressions {:?}", logic_args);

                    let anon_term = vec![symbols::Symbol::new(
                        "anonymous_term".into(),
                        symbols::SymbolKind::Term,
                        TypeDef::Bool,
                        logic_args,
                        Token::AnonymousTerm,
                    )];

                    enum_s.add_arg(symbols::Symbol::new(
                        variant.data_field.clone().ok_or(CompileError::from(
                            format!("Variant: '{}' has no data field. This is currently not allowed.", 
                            variant.variant_id)
                        ))?.ident,
                        symbols::SymbolKind::EnumVariant,
                        variant_type_def,
                        anon_term,
                        variant_token,
                    ));
                }
            }
        }

        add_logical_formula(
            self.ident.ident.clone(),
            term_section_index,
            enum_s,
            symbols,
            &scope,
        )?;

        Ok(())
    }
}

// =========== ActionSection ================================================

impl ast::ActionSection {
    fn eval(
        &self,
        action_section_index: usize,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
    ) -> Result<(), CompileError> {
        let _symbols = symbols.borrow();

        // An action_section symbol has as its type not the return type, like
        // other symbols, but instead its the type of the locally defined
        // variable in the `with` statement. Used by pattern matches.
        let mut action_section_type = TypeDef::Unknown;

        let mut local_scope: Vec<Symbol> = vec![];
        // There may be a local scope defined in a `with <ARGUMENT_ID>`
        // in the term header.
        if let Some(TypeIdentField { field_name, ty }) = self.with_kv.get(0) {
            // Does the supplied type exist in our scope?
            action_section_type =
                check_type_identifier(ty.clone(), symbols.clone(), &scope)?;

            local_scope.push(symbols::Symbol::new(
                field_name.clone().ident,
                SymbolKind::Argument,
                action_section_type.clone(),
                vec![],
                Token::ActionArgument(action_section_index, 0),
            ))
        }

        trace!(
            "action section nunber {} {} with {:?}",
            action_section_index,
            self.ident,
            self.with_kv
        );
        trace!("local scope");
        trace!("{:#?}", local_scope);

        let mut action_exprs = vec![];
        for (i, kv) in self.with_kv.iter().enumerate() {
            action_exprs.push(symbols::Symbol::new(
                kv.field_name.ident.clone(),
                symbols::SymbolKind::Argument,
                TypeDef::try_from(kv.ty.clone())?,
                vec![],
                Token::ActionArgument(action_section_index, i),
            ))
        }

        for compute_expr in &self.body.expressions {
            // The Access Receiver may have an identifier, in which case it
            // may be the incoming or outgoing variable name.
            //
            // The incoming/outgoing payload variables are the only variables
            // that can be used in the 'action' section. The incoming payload
            // variable has either SymbolKind::SplitRxType/SplitTxType OR
            // PassthroughRxTxType as type.
            //
            // If the Access Receiver does not have an identifier it is
            // something global, in the context of an actions this can only be
            // a global method call.
            //
            // Method Calls on Roto Types are also allowed, e.g.
            // `String.format(..)`
            let ar_name = match compute_expr.get_receiver_ident() {
                Ok(name) => name,
                Err(_) => compute_expr
                    .access_expr
                    .get(0)
                    .ok_or(CompileError::Internal(format!(
                        "Cannot find access expr in: {:?}",
                        compute_expr.access_expr
                    )))?
                    .get_ident()?
                    .clone(),
            };

            let mut s = compute_expr.eval(
                Some(format!("action-{}", ar_name).as_str().into()),
                symbols.clone(),
                scope.clone(),
                &local_scope,
            )?;

            s = s.set_kind(SymbolKind::AccessReceiver);

            action_exprs.push(s);
        }

        drop(_symbols);

        let action_section = symbols::Symbol::new(
            self.ident.ident.clone(),
            symbols::SymbolKind::ActionSection,
            action_section_type,
            action_exprs,
            Token::ActionSection(action_section_index),
        );

        add_action_section(
            self.ident.ident.clone(),
            action_section,
            symbols.clone(),
            &scope,
        )?;

        Ok(())
    }
}

//============ ApplySection =================================================

impl ast::ApplySection {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
    ) -> Result<(), CompileError> {
        let mut _symbols = symbols.borrow_mut();
        let _filter_map_symbols =
            _symbols.get_mut(&scope).ok_or_else(|| {
                format!("No symbols found for filter-map {}", scope)
            })?;

        // There can only be one `apply` section in a filter_map, so we can
        // set the default action from the apply section for the whole
        // filter_map.
        if let Some(accept_reject) = self.body.accept_reject {
            _filter_map_symbols.set_default_action(accept_reject);
        } else {
            _filter_map_symbols.set_default_action(AcceptReject::Accept);
        }

        drop(_symbols);
        for a_scope in &self.body.scopes {
            let s = a_scope.eval(symbols.clone(), scope.clone())?;

            trace!("apply body symbol {:#?}", s);
            match s.get_kind() {
                symbols::SymbolKind::GlobalEnum => {
                    // for ma in s.get_args_owned() {
                    //     trace!("match action symbol {:?}", ma);
                    //     add_match_action(ma.get_name(), ma, symbols.clone(), &scope)?
                    // }
                    add_match_action(
                        s.get_name(),
                        s,
                        symbols.clone(),
                        &scope,
                    )?
                }
                symbols::SymbolKind::MatchAction(_ma) => add_match_action(
                    s.get_name(),
                    s,
                    symbols.clone(),
                    &scope,
                )?,
                _ => {
                    return Err(CompileError::Internal(format!(
                        "Cannot evaluate symbol {:#?}",
                        s
                    )));
                }
            };
        }

        Ok(())
    }
}

impl ast::ApplyScope {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.borrow();
        let filter_map_symbols = _symbols.get(&scope).ok_or_else(|| {
            format!("No symbols found for filter-map {}", scope)
        })?;

        // not doing anything with the actual ApplyScope (the use statement),
        // not sure whether it is going to be needed.
        let _s_name = self.scope.clone().map(|s| s.ident);

        match &self.match_action {
            MatchActionExpr::FilterMatchAction(fma) => {
                let term = fma.filter_ident.eval(
                    symbols.clone(),
                    scope.clone(),
                    &[],
                )?;
                let (_ty, token) = filter_map_symbols
                    .get_term_section_type_and_token(&term.get_name())?;

                let mut args_vec = vec![];
                for (action_expr, accept_reject) in &fma.actions {
                    if let Some(action_call) = action_expr.clone() {
                        // If there's one or more actions in the filter block we
                        // will store them as args in the vector.
                        let action_name = action_call
                            .eval(symbols.clone(), scope.clone(), &[])?
                            .get_name();

                        let (_ty, token) = filter_map_symbols
                            .get_action_section(&action_name)?;

                        let s = symbols::Symbol::new(
                            action_name,
                            symbols::SymbolKind::ActionCall,
                            TypeDef::AcceptReject(
                                accept_reject
                                    .unwrap_or(ast::AcceptReject::NoReturn),
                            ),
                            vec![],
                            token,
                        );
                        args_vec.push(s);
                    } else {
                        // If there's no Action mentioned in a filter block, we will
                        // create a MatchAction of type Empty, so that the compiler
                        // can invoke the right accept/reject commands. The action
                        // symbol gets to have the name of the term, but it is never
                        // inspected, since it doesn't exist in any symbol map.
                        let s = symbols::Symbol::new(
                            term.get_name(),
                            symbols::SymbolKind::ActionCall,
                            TypeDef::AcceptReject(accept_reject.ok_or_else(
                                || {
                                    CompileError::from(
                                "Encountered FilterMatchAction without \
                                Action or Accept-Reject expression.",
                            )
                                },
                            )?),
                            vec![],
                            Token::NoAction,
                        );
                        args_vec.push(s);
                    }
                }

                Ok(symbols::Symbol::new(
                    term.get_name(),
                    if fma.negate {
                        symbols::SymbolKind::MatchAction(
                            MatchActionType::Filter,
                        )
                    } else {
                        symbols::SymbolKind::MatchAction(
                            MatchActionType::Negate,
                        )
                    },
                    // The AcceptReject value from the Apply section does not end up
                    // here, instead it lives on the ApplyBody, and it is saved in the
                    // symboltable of the filter_map.
                    TypeDef::Unknown,
                    args_vec,
                    token,
                ))
            }
            MatchActionExpr::PatternMatchAction(pma) => {
                // this code is all very similar to `eval_as_match_expression`
                trace!("pattern match");
                trace!("{:#?}", pma);
                let enum_ident = pma.operator.get_ident()?;
                // check if the enum makes sense as an AccessReceiver
                let mut enum_s = ast::AccessReceiver::Ident(enum_ident.clone())
                    .eval(symbols.clone(), scope.clone(), &[])
                    .map_err(|_| {
                        CompileError::from(format!(
                            "Variable '{}' (supposedly an Enum) cannot be found in \
                            this scope.",
                            enum_ident
                        ))
                    })?;

                enum_s = enum_s.set_kind(symbols::SymbolKind::GlobalEnum);

                trace!("enum symbol");
                trace!("{:#?}", enum_s);

                // loop over all the variants for this enum. AccessReceiver::eval
                // checks for the existence of the identifier.
                for variant in pma.match_arms.iter() {
                    trace!(
                        "{}({:?}) -> {:#?}",
                        variant.variant_id,
                        variant.data_field,
                        variant.actions
                    );

                    if let TypeDef::GlobalEnum(e_num) = enum_s.get_type() {
                        let (variant_type_def, variant_token) = e_num
                            .get_props_for_variant(&variant.variant_id)?;

                        // create a local scope with the data field variable
                        // for this variant.
                        let local_scope = vec![symbols::Symbol::new(
                            variant.data_field.clone().ok_or(CompileError::from(
                                format!("Variant: '{}' has no data field. This is currently not allowed.", 
                                variant.variant_id)
                            ))?.ident,
                            symbols::SymbolKind::VariableAssignment,
                            variant_type_def.clone(),
                            vec![],
                            variant_token.clone(),
                        )];
                        trace!("-> local scope {:?}", local_scope);

                        // extract the action calls in this variant's body
                        let mut args_vec = vec![];

                        // If there was a guard defined for this variant then
                        // we're storing that as the first argument. We are
                        // only allowing one term as the actual guard (for
                        // now at least), so we're looking the ident as a
                        // term,
                        if let Some(term_call_expr) = &variant.guard {
                            trace!("Term Call Expr {:?}", term_call_expr);
                            let term = term_call_expr.eval(
                                symbols.clone(),
                                scope.clone(),
                                &local_scope,
                            )?;
                            let (_ty, _token) = filter_map_symbols
                                .get_term_section_type_and_token(
                                    &term.get_name(),
                                )?;
                            trace!(
                                "evaluated guard {:?} as term with {} and\
                             {:?}",
                                variant.guard,
                                _ty,
                                _token
                            );

                            args_vec.push(term);
                        }
                        for action in &variant.actions {
                            match action {
                                (Some(action_call), accept_reject) => {
                                    // If there's one or more action calls in
                                    // the filter block we will store them as
                                    // args in the vector.
                                    trace!(
                                        "eval action call in match arm {}",
                                        action_call.action_id
                                    );
                                    let action_s = action_call.eval(
                                        symbols.clone(),
                                        scope.clone(),
                                        &local_scope,
                                    )?;

                                    args_vec.push(action_s.set_type(
                                        TypeDef::AcceptReject(
                                            accept_reject.unwrap_or(
                                                ast::AcceptReject::NoReturn,
                                            ),
                                        ),
                                    ));
                                }
                                // There are no action calls in the variant
                                // body, but there is an AcceptReject
                                // expression (indicating an early return).
                                (None, Some(accept_reject)) => {
                                    let s = symbols::Symbol::new(
                                        variant.variant_id.clone().ident,
                                        symbols::SymbolKind::ActionCall,
                                        TypeDef::AcceptReject(*accept_reject),
                                        vec![],
                                        Token::NoAction,
                                    );
                                    args_vec.push(s);
                                }
                                // No action call, no AcceptReject either.
                                // It's a NOP and we're ignoring it.
                                (None, None) => {}
                            }
                        }
                        trace!(
                            "match_action expression arguments {:#?}",
                            args_vec
                        );

                        enum_s.add_arg(symbols::Symbol::new(
                            variant.data_field.clone().ok_or(CompileError::from(
                                format!("Variant: '{}' has no data field. This is currently not allowed.", 
                                variant.variant_id)
                            ))?.ident,
                            symbols::SymbolKind::EnumVariant,
                            variant_type_def,
                            args_vec,
                            variant_token,
                        ));
                    }
                }

                trace!("result enum eval map");
                trace!("{:#?}", enum_s);
                Ok(enum_s)
            }
        }
    }
}

// =========== Nested AST Nodes =============================================

// These are types that can be nested inside of other types, or are used
// recursively. They return the symbols that they create, unlike the types
// that go directly in the root of a SymbolTable.
// The caller needs to insert them in the right place in a entry in the symbol
// table.

//------------ ComputeExpr --------------------------------------------------

// An expression that computes a return value based on the AccessReceiver,
// (the `receiver` field), the root of the expression and its arguments
// (`args` field). Each argument is a dot-divided part of the expression.
// A compute expression may have an arbitrary number of arguments, e.g.
// the compute expression `my_var.a.b.c().d` will have 4 arguments and an
// access receiver 'my_var'.

// Access Receiver
//
// The access receiver is the data source that provides a combination of
// fields to be accessed and methods to be called. The access receiver will
// be encoded in the resulting symbol in its `kind` and `token` fields.
// The resulting symbol will always have a `kind` set to `AccessReceiver`
// and the type of the access receiever will be encoded in the token, e.g.
//
// Symbol {
//      name: 'my_var',
//      kind: SymbolKind::AccessReceiver,
//      args: [see below]
//      ...,
//      token: DataSource(1)
// }
//
// Arguments
//
// These are the expressions that are divided by dots, e.g. a.b.c().d, will
// turn up as:
//
// args: [ FieldAccessSymbolA, FieldAccessSymbolB, MethodCallSymbolC,
//         FieldAccessSymbolC, FieldAccessSymbolD ]
//

impl ast::ComputeExpr {
    pub(crate) fn eval(
        &self,
        // If no name is provided we use the ident of the access receiver
        name: Option<ShortString>,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[symbols::Symbol],
    ) -> Result<symbols::Symbol, CompileError> {
        // this ar_name is only for use in error messages, the actual name
        // for the symbol that will be created can be slightly different,
        // e.g. having a prefix 'action-'.
        let ar_name = self.get_receiver_ident().or_else(|_| {
            Ok::<ShortString, CompileError>(
                self.access_expr
                    .get(0)
                    .ok_or(CompileError::Internal(format!(
                        "Cannot find access expr in: {:?}",
                        self.access_expr
                    )))?
                    .get_ident()?
                    .clone(),
            )
        })?;

        let ar_s = self.get_receiver();

        trace!("->-> local scope {:?}", local_scope);
        // The evaluation of the Access Receiver
        let mut ar_symbol =
            // was it registered in the current (local) scope by the user?
            ar_s.eval(symbols.clone(), scope.clone(), local_scope)
                // Is it registered in the global scope by the user?
                .or_else(|_| ar_s.eval(symbols.clone(), Scope::Global, &[]))
                // Is it  a global enum or a variant of a global enum?
                .or_else(|_| {
                    GlobalEnumTypeDef::any_variant_as_symbol(
                        &ar_s.get_ident().ok_or(AccessReceiverError::Global)?.ident,
                    )
                })
                .map_err(|ar_err| match ar_err {
                    AccessReceiverError::Var => CompileError::from(format!(
                    "Cannot find variable '{}' in {} or in the global scope.",
                    ar_name, scope
                )),
                    AccessReceiverError::Global => CompileError::from(
                        format!("Cannot find global method '{}'", ar_name),
                    ),
                    AccessReceiverError::Arg => CompileError::from(format!(
                        "Cannot find Argument '{}' for scope {}",
                        ar_name, scope
                    )),
                })?;

        let ar_token = ar_symbol.get_token();
        let mut s = &mut ar_symbol;

        trace!("ACCESS EXPRESSION {:#?}", self.access_expr);

        // Use the type of the access receiver to put on the arguments.
        let mut ty = match ar_token {
            Token::Table(_) => TypeDef::Table(Box::new(s.get_type())),
            Token::Rib(_) => TypeDef::Rib((Box::new(s.get_type()), None)),
            Token::OutputStream(_) => {
                TypeDef::OutputStream(Box::new(s.get_type()))
            }
            _ => s.get_type(),
        };

        for a_e in &self.access_expr {
            match a_e {
                ast::AccessExpr::MethodComputeExpr(method_call) => {
                    trace!("MC symbol (s) {:#?}", s);
                    // trace!("All Symbols {:#?}", symbols.borrow().get(&scope));
                    trace!("method call {:#?} on type {}", method_call, ty);
                    trace!("local scope {:?}", local_scope);
                    let arg_s = method_call.eval(
                        // At this stage we don't know really whether the
                        // method call will be mutating or not, but we're
                        // setting the safe choice here (non-mutable).
                        symbols::SymbolKind::MethodCallbyRef,
                        ty,
                        symbols.clone(),
                        scope.clone(),
                        local_scope,
                    )?;
                    // propagate the type of this argument to a possible next one
                    ty = arg_s.get_type();
                    s.add_arg(arg_s);
                }
                ast::AccessExpr::FieldAccessExpr(field_access) => {
                    trace!("FA symbol (s) {:#?}", s);
                    trace!(
                        "all symbols in filter-map table {:#?}",
                        symbols.borrow().get(&scope)
                    );
                    let arg_s = field_access.eval(ty)?;
                    // propagate the type of this argument to a possible next one
                    ty = arg_s.get_type();
                    let i = s.add_arg(arg_s);
                    trace!("symbol -> {:#?}", s);
                    s = &mut s.get_args_mut()[i];
                }
            };
        }

        // The return type of a compute expression is propagated from the
        // last argument to its parent, the access receiver symbol.
        ar_symbol = ar_symbol.set_type(ty).set_token(ar_token);

        if let Some(name) = name {
            ar_symbol = ar_symbol.set_name(name);
        } else {
            ar_symbol = ar_symbol.set_name(self.get_receiver_ident()?);
        }

        trace!(
            "finished eval compute expression {:?}",
            ar_symbol.get_name()
        );
        Ok(ar_symbol)
    }
}

impl ast::MethodComputeExpr {
    pub(crate) fn eval(
        &self,
        // Parsed return type of the method call
        mut method_kind: symbols::SymbolKind,
        method_call_type: TypeDef,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[symbols::Symbol],
    ) -> Result<symbols::Symbol, CompileError> {
        // self is the call receiver, e.g. in `rib-rov.longest_match()`,
        // `rib-rov` is the receiver and `longest_match` is the method call
        // name. The actual method call lives in the `args` field.
        let arguments =
            self.args.eval(symbols.clone(), scope, local_scope)?;

        // we need to lookup the properties of the return type of the method
        // that the user wants to call, to see if it matches the arguments of
        // the supplied method call in the source code.
        let props = method_call_type.get_props_for_method(&self.ident)?;

        // If this is a "regular" method call, then we set the `consume` flag
        // from the props we retrieved to set the right MethodCall kind.
        if method_kind == SymbolKind::MethodCallbyRef {
            method_kind = match props.consume {
                false => SymbolKind::MethodCallbyRef,
                true => SymbolKind::MethodCallByConsumedValue,
            };
        }

        let parsed_args = arguments;

        if parsed_args.is_empty() && props.arg_types.is_empty() {
            return Ok(symbols::Symbol::new(
                self.ident.clone().ident,
                method_kind,
                props.return_type,
                vec![],
                props.method_token,
            ));
        }

        // early return if no arguments were supplied and the method doesn't
        // take any either.
        if parsed_args.len() != props.arg_types.len() {
            return Err(format!(
                "Method '{}' on type {:?} expects {} arguments, but {} were \
                provided.",
                self.ident,
                method_call_type,
                props.arg_types.len(),
                parsed_args.len()
            )
            .into());
        }

        let mut args = vec![];
        let _symbols = symbols.borrow();

        // go over the argument types that we got from the parsed arguments
        // in the the source code and compare those to the argument types
        // we got from the method definition.
        for (parsed_arg_type, expected_arg_type) in
            parsed_args.into_iter().zip(props.arg_types.iter())
        {
            // Compare the expected type with the type of the parsed value.
            // Either the types are the same, or the type of the parsed value
            // can be converted to the expected type, e.g. an IntegerLiteral
            // can be converted into a U8, I64, etc (as long as it fits).
            args.push(
                parsed_arg_type
                    .try_convert_type_value_into(expected_arg_type.clone())?,
            );
        }
        trace!("converted args {:?}", args);

        Ok(symbols::Symbol::new(
            self.ident.clone().ident,
            method_kind,
            props.return_type,
            args,
            props.method_token,
        ))
    }
}

#[derive(Debug)]
pub enum AccessReceiverError {
    Var,
    Global,
    Arg,
}

// This is a simple identifier, it may refer to a data source, a variable,
// a record field, the name of a type, or the name of a global method.
impl ast::AccessReceiver {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[Symbol],
    ) -> Result<symbols::Symbol, AccessReceiverError> {
        trace!("AccessReceiver {:#?}", self);
        let _symbols = symbols.clone();
        if let Some(search_ar) = self.get_ident() {
            // Is it the name of a builtin type?
            if let Ok(prim_ty) = TypeDef::try_from(search_ar.clone()) {
                if prim_ty.is_builtin() {
                    {
                        return Ok(symbols::Symbol::new(
                            search_ar.ident.clone(),
                            symbols::SymbolKind::AccessReceiver,
                            prim_ty,
                            vec![],
                            Token::BuiltinType(0),
                        ));
                    };
                }
            }

            // is it a local-scope-level argument?
            trace!("checking local scope for {}", search_ar.ident);
            trace!("local scope {:#?}", local_scope);
            if let Some(arg) = local_scope
                .iter()
                .find(move |s| s.get_name() == search_ar.ident)
            {
                trace!("local variable {} found", search_ar.ident);
                return Ok(symbols::Symbol::new(
                    search_ar.ident.clone(),
                    symbols::SymbolKind::AccessReceiver,
                    arg.get_type(),
                    vec![],
                    arg.get_token(),
                ));
            }

            // is it a filter-map-level argument?
            if let Some(Ok(arg)) = _symbols
                .borrow()
                .get(&scope)
                .map(|s| s.get_argument(&search_ar.ident))
            {
                let (_, type_def, token) = arg
                    .get_kind_type_and_token()
                    .map_err(|_| AccessReceiverError::Arg)?;
                return Ok(symbols::Symbol::new(
                    search_ar.ident.clone(),
                    symbols::SymbolKind::AccessReceiver,
                    type_def,
                    vec![],
                    token,
                ));
            }

            // Is it one of:
            // - a name of a data source
            // - the name of a built-in constant
            // - variable name thas was defined in the `with` statement or
            //   earlier on in the same define section.
            let ident = &[search_ar.clone()];
            let (_kind, ty, to, val) =
                get_props_for_scoped_variable(ident, symbols, scope)
                    .map_err(|_| AccessReceiverError::Var)?;
            // Additionally check if this is a Variable or a Constant.
            // Constants need their values to be preserved.
            match val {
                // It's a Constant, clone the (builtin-typed) value into the
                // symbol.
                Some(val) => Ok(symbols::Symbol::new_with_value(
                    search_ar.ident.clone(),
                    SymbolKind::AccessReceiver,
                    val,
                    vec![],
                    to,
                )),
                // It's a Variable, create a symbol with an empty value.
                None => Ok(symbols::Symbol::new(
                    search_ar.ident.clone(),
                    SymbolKind::AccessReceiver,
                    ty,
                    vec![],
                    to,
                )),
            }
        } else {
            // No Identifier, no AccessReceiver, this is for a globally
            // scoped method, which is None of our business anyway (it's the
            // caller's business).
            Err(AccessReceiverError::Global)
        }
    }
}

impl ast::LiteralExpr {
    fn eval(
        &self
    ) -> Result<symbols::Symbol, CompileError> {
        trace!("literal value {:?}", self);
        Ok(symbols::Symbol::new_with_value(
            "lit".into(),
            symbols::SymbolKind::Constant,
            self.try_into()?,
            vec![],
            Token::Constant(None),
        ))
    }
}

impl ast::LiteralAccessExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[Symbol]
    ) -> Result<symbols::Symbol, CompileError> {

        let mut ar_symbol = self.literal.eval()?;
        let mut ty = ar_symbol.get_type();

        let ar_token = ar_symbol.get_token();
        let mut s = &mut ar_symbol;

        for a_e in &self.access_expr {
            match a_e {
                ast::AccessExpr::MethodComputeExpr(method_call) => {
                    trace!("MC symbol (s) {:#?}", s);
                    trace!("method call {:#?} on type {}", method_call, ty);
                    trace!("local scope {:?}", local_scope);
                    
                    let arg_s = method_call.eval(
                        // At this stage we don't know really whether the
                        // method call will be mutating or not, but we're
                        // setting the safe choice here (non-mutable).
                        symbols::SymbolKind::MethodCallbyRef,
                        ty,
                        symbols.clone(),
                        scope.clone(),
                        local_scope,
                    )?;
                    // propagate the type of this argument to a possible next one
                    ty = arg_s.get_type();
                    s.add_arg(arg_s);
                }
                ast::AccessExpr::FieldAccessExpr(field_access) => {
                    trace!("FA symbol (s) {:#?}", s);
                    trace!(
                        "all symbols in filter-map table {:#?}",
                        symbols.borrow().get(&scope)
                    );
                    let arg_s = field_access.eval(ty)?;
                    // propagate the type of this argument to a possible next one
                    ty = arg_s.get_type();
                    let i = s.add_arg(arg_s);
                    trace!("symbol -> {:#?}", s);
                    s = &mut s.get_args_mut()[i];
                }
            };
        }

        // The return type of a literal access expression is propagated from
        // the last argument to its parent, the access receiver symbol.
        ar_symbol = ar_symbol.set_type(ty).set_token(ar_token);
        Ok(ar_symbol)
    }
}

impl ast::ValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[Symbol],
    ) -> Result<symbols::Symbol, CompileError> {
        match self {
            ast::ValueExpr::LiteralAccessExpr(lit) => lit.eval(symbols, scope, local_scope),
            // an expression ending in a a method call (e.g. `foo.bar()`).
            // Note that the evaluation of the method call will check for
            // the existence of the method.
            ast::ValueExpr::ComputeExpr(compute_expr) => {
                trace!("compute expr {:?}", compute_expr);
                compute_expr.eval(None, symbols, scope, local_scope)
            }
            ast::ValueExpr::RootMethodCallExpr(builtin_call_expr) => {
                let name: ShortString = builtin_call_expr.ident.clone().ident;
                let prim_ty =
                    TypeDef::try_from(builtin_call_expr.ident.clone())?;

                if prim_ty.is_builtin() {
                    builtin_call_expr.eval(
                        symbols::SymbolKind::BuiltInTypeMethodCall,
                        prim_ty,
                        symbols,
                        scope,
                        local_scope,
                    )
                } else {
                    Err(format!("Unknown built-in method call: {}", name))?
                }
            }
            ast::ValueExpr::AnonymousRecordExpr(rec) => {
                let rec_value = rec.eval(symbols, scope, local_scope)?;
                let type_def: Vec<_> = rec_value
                    .iter()
                    .map(|v| (v.get_name(), Box::new(v.get_type())))
                    .collect();
                Ok(symbols::Symbol::new(
                    "anonymous_record".into(),
                    symbols::SymbolKind::AnonymousType,
                    TypeDef::Record(RecordTypeDef::new(type_def)),
                    rec_value,
                    Token::AnonymousRecord,
                ))
            }
            ast::ValueExpr::TypedRecordExpr(rec) => {
                let (type_id, rec_value) =
                    rec.eval(symbols.clone(), scope.clone(), local_scope)?;

                // see if the type on the record was actually defined by the roto user
                let checked_ty = if let TypeDef::Record(fields) =
                    check_type_identifier(type_id.clone(), symbols, &scope)?
                {
                    fields
                } else {
                    RecordTypeDef::new(vec![])
                };

                // now check all individual fields to see if they match up,
                // meaning the (field_name, type) pairs match or can be made
                // to match by trying a type conversion on each field until
                // we fail, or succeed for all fields. The type with the
                // conversions is stored as the actual type.
                let mut checked_values = vec![];
                for field_s in rec_value {
                    let cur_ty =
                        checked_ty.iter().find(|v| v.0 == field_s.get_name());
                    if let Some(cur_ty) = cur_ty {
                        let field_name = field_s.get_name();
                        let field_ty = field_s.get_type();
                        checked_values.push(
                            field_s
                                .try_convert_type_value_into(
                                    *cur_ty.1.clone(),
                                )
                                .map_err(|_e| {
                                    CompileError::from(format!(
                                        "The field name '{}' has the wrong \
                                type. Expected '{}', but got '{}'",
                                        field_name, cur_ty.1, field_ty
                                    ))
                                })?,
                        );
                    } else {
                        return Err(CompileError::from(format!(
                            "The field \
                        name '{}' cannot be found in type '{}'",
                            field_s.get_name(),
                            type_id.ident
                        )));
                    }
                }

                Ok(symbols::Symbol::new(
                    type_id.ident,
                    symbols::SymbolKind::NamedType,
                    TypeDef::Record(checked_ty),
                    checked_values,
                    Token::TypedRecord,
                ))
            }
            ast::ValueExpr::PrefixMatchExpr(_) => todo!(),
            ast::ValueExpr::ListExpr(list_elm) => {
                let list_value = list_elm.eval(symbols, scope)?;
                let type_def =
                    first_into_compile_err!(list_value)?.get_type();

                Ok(symbols::Symbol::new(
                    "anonymous_list".into(),
                    symbols::SymbolKind::AnonymousType,
                    TypeDef::List(Box::new(type_def)),
                    list_value,
                    Token::List,
                ))
            }
        }
    }
}

impl ast::ArgExprList {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[symbols::Symbol],
    ) -> Result<Vec<symbols::Symbol>, CompileError> {
        let mut eval_args = vec![];
        for arg in &self.args {
            let parsed_arg =
                arg.eval(symbols.clone(), scope.clone(), local_scope)?;
            eval_args.push(parsed_arg);
        }
        Ok(eval_args)
    }
}

impl ast::FieldAccessExpr {
    fn eval(
        &self,
        field_type: TypeDef,
    ) -> Result<symbols::Symbol, CompileError> {
        trace!("field access on field type {:?}", field_type);
        trace!("self field names {:?}", self.field_names);

        // The type of a Field Access symbol is going to be the type of the
        // parent that we're indexing. The kind however indicates whether
        // we need to lazily or 'directly' retrieve the value for the
        // index(es).
        let symbol_kind = if let TypeDef::LazyRecord(_) = field_type {
            symbols::SymbolKind::LazyFieldAccess
        } else {
            symbols::SymbolKind::FieldAccess
        };

        // has_fields_chain preserves the TypeValue of the AST subtree,
        // if it was already set, i.e. in the case of a Constant or a variant
        // of a global enum.
        if let Ok((type_def, to, tv)) =
            field_type.has_fields_chain(&self.field_names)
        {
            trace!("token {:?}", to);
            let name = self.field_names.join(".");

            match tv {
                None => {
                    return Ok(symbols::Symbol::new(
                        name.as_str().into(),
                        symbol_kind,
                        type_def,
                        vec![],
                        to,
                    ))
                }
                // preserve the TypeValue if set.
                Some(tv) => {
                    return Ok(symbols::Symbol::new_with_value(
                        name.as_str().into(),
                        symbol_kind,
                        tv,
                        vec![],
                        to,
                    ))
                }
            }
        } else {
            Err(format!("Invalid field access expression: {:?}.", self)
                .into())
        }
    }
}

impl ast::ListValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
    ) -> Result<Vec<symbols::Symbol>, CompileError> {
        trace!("anonymous list");
        let mut s: Vec<symbols::Symbol> = vec![];
        for value in &self.values {
            let arg = value.eval(symbols.clone(), scope.clone(), &[])?;
            s.push(arg);
        }

        Ok(s)
    }
}

impl ast::AnonymousRecordValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[symbols::Symbol],
    ) -> Result<Vec<symbols::Symbol>, CompileError> {
        trace!("anonymous record");
        let mut s: Vec<symbols::Symbol> = vec![];
        for (key, value) in &self.key_values {
            let arg =
                value.eval(symbols.clone(), scope.clone(), local_scope)?;
            s.push(arg.set_name(key.ident.clone()));
        }

        Ok(s)
    }
}

impl ast::TypedRecordValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[symbols::Symbol],
    ) -> Result<(ast::TypeIdentifier, Vec<symbols::Symbol>), CompileError>
    {
        trace!("typed record");
        let mut s: Vec<symbols::Symbol> = vec![];
        for (key, value) in &self.key_values {
            let arg =
                value.eval(symbols.clone(), scope.clone(), local_scope)?;
            s.push(arg.set_name(key.ident.clone()));
        }

        Ok((self.type_id.clone(), s))
    }
}

// ActionCallExpr is a much simpler beast than ComputeExpr, it can only
// consist of an action-name and an optional local variable name(s) that
// is/are passed in into the action, e.g. `send_msg(pd_mesg)`. This is used
// in match arms.
impl ast::ActionCallExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[symbols::Symbol],
    ) -> Result<symbols::Symbol, CompileError> {
        trace!("action call expr {} with {:?}", self.action_id, self.args);
        let _symbols = symbols.borrow();

        let filter_map_symbols = _symbols.get(&scope).ok_or_else(|| {
            format!("No symbols found for filter-map {}", scope)
        })?;

        let (ty, to) =
            filter_map_symbols.get_action_section(&self.action_id.ident)?;

        let mut args = if let Some(args) = &self.args {
            args.eval(symbols.clone(), scope.clone(), local_scope)?
        } else {
            vec![]
        };

        // Check if the first argument of the call is the same, or can be
        // converted to the type of the ActionSection
        let args =
            vec![args.remove(0).try_convert_type_value_into(ty.clone())?];

        Ok(symbols::Symbol::new(
            self.action_id.ident.clone(),
            symbols::SymbolKind::ActionCall,
            ty,
            args,
            to,
        ))
    }
}

impl ast::TermCallExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &[symbols::Symbol],
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.borrow();

        let filter_map_symbols = _symbols.get(&scope).ok_or_else(|| {
            format!("No symbols found for filter-map {}", scope)
        })?;

        let (ty, to) = filter_map_symbols
            .get_term_section_type_and_token(&self.term_id.ident)?;

        let mut args = if let Some(args) = &self.args {
            args.eval(symbols.clone(), scope.clone(), local_scope)?
        } else {
            vec![]
        };
        let first_arg = args.remove(0);

        // Check if the first argument of the call is the same, or can be
        // converted to the type of the TermSection
        let args = if let Some(first_arg_type) =
            filter_map_symbols.get_type_of_argument(&self.term_id.ident, 0)
        {
            vec![first_arg.try_convert_type_value_into(first_arg_type)?]
        } else {
            vec![]
        };

        Ok(symbols::Symbol::new(
            self.term_id.ident.clone(),
            symbols::SymbolKind::TermCall,
            ty,
            args,
            to,
        ))
    }
}

//============ First-order Logic Evaluation (Terms) =========================

//------------ Logical Expression -------------------------------------------

// See ast::LogicalExpr for more information.

impl ast::LogicalExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        match self {
            LogicalExpr::BooleanExpr(expr) => {
                ast::BooleanExpr::eval(expr, symbols, &scope, local_scope)
            }
            LogicalExpr::OrExpr(or_expr) => {
                ast::OrExpr::eval(or_expr, symbols, &scope, local_scope)
            }
            LogicalExpr::AndExpr(and_expr) => {
                ast::AndExpr::eval(and_expr, symbols, &scope, local_scope)
            }
            LogicalExpr::NotExpr(not_expr) => {
                ast::NotExpr::eval(not_expr, symbols, &scope, local_scope)
            }
        }
    }
}

//------------ Boolean Expression -------------------------------------------

// A Boolean Expression is an expresion that takes an input with an arbitrary
// type and evaluates it into a boolean value, e.g. stand-alone variable of
// type boolean is a boolean expression.

impl ast::BooleanExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        match &self {
            ast::BooleanExpr::GroupedLogicalExpr(grouped_expr) => {
                grouped_expr.eval(symbols, scope, local_scope)
            }
            ast::BooleanExpr::BooleanLiteral(bool_lit) => {
                // Leaf node, needs a TypeValue in this case a boolean
                // value.
                Ok(symbols::Symbol::new_with_value(
                    "boolean_constant".into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::Bool(
                        bool_lit.0,
                    )),
                    vec![],
                    Token::Constant(None),
                ))
            }
            ast::BooleanExpr::CompareExpr(compare_expr) => {
                ast::CompareExpr::eval(
                    compare_expr,
                    symbols,
                    scope,
                    local_scope,
                )
            }
            ast::BooleanExpr::ComputeExpr(call_expr) => {
                // A Call Expression does not necessarily have to return a
                // boolean, as long as the compare expression it is nested in
                // has left and right hand-sides that return the same type.
                // Checking this can therefore not be done by the Call
                // Expression check here.
                let s = call_expr.eval(
                    // call_expr.get_receiver_ident()?,
                    None,
                    symbols,
                    scope.clone(),
                    local_scope,
                )?;
                Ok(s)
            }
            // like the call expression above, a literal, or a literal access
            // (a method/field access on a literal) may return a boolean.
            ast::BooleanExpr::LiteralAccessExpr(lit_access_expr) => {
                let s = lit_access_expr.eval(
                    symbols, scope.clone(), local_scope
                )?;
                Ok(s)
            }
            ast::BooleanExpr::ListCompareExpr(list_compare_expr) => {
                let s = list_compare_expr.as_ref().eval(symbols, scope)?;
                Ok(s)
            }
            ast::BooleanExpr::PrefixMatchExpr(_) => todo!(),
        }
    }
}

//----------------- Compare Expression --------------------------------------

// A Compare Expression is an expression of the form
// `<left handside symbol> <compare operator> <right handside symbol>`.

// The both outer symbols can hold an argument expression or groups of
// argument expressions. These are called an Compare Argument. In a Compare
// Expression all the Compare Arguments (both left and right) will have to
// evaluate to the same BUILTIN type and have to be leaf nodes. This means the
// user cannot compare complete lists and records to anything. The user can
// however use fields in lists and records.

// A Leaf Node is a symbol that has a value on the `value` field. Furthermore,
// a non-leaf node can have a nested leaf node in its `args` field. If the
// the `ty` field is not set, this is an indication that the symbol is not a
// not a leaf node. A node having neither a `ty` field filled, nor a `value`
// should not exist.

impl ast::CompareExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        // Process the left hand side of the compare expression. This is a
        // Compare Argument. It has to end in a leaf node, otherwise it's
        // an error.
        let left_s = self.left.eval(_symbols, scope, local_scope)?;
        let left_type = left_s.get_type();

        let mut right_s = self.right.eval(symbols, scope, local_scope)?;
        let right_type = right_s.get_type();

        // Either the left and right hand sides are of the same type OR the
        // right hand side value can be converted into a type of the left
        // hand side. For example, a comparison of PrefixLength and
        // IntegerLiteral will work in the form of `prefix.len() == 32;`, but
        // NOT reversed, i.e. `32 == prefix.len();` is INVALID.
        trace!("left_type {:#?} <-> right_type {:#?}", left_s, right_s);
        if left_type != right_type {
            right_s =
                right_s.try_convert_type_value_into(left_type.clone())?;
        }
        trace!("after conversion {} <-> {:?}", left_type, right_s);

        Ok(symbols::Symbol::new(
            "compare_expr".into(),
            symbols::SymbolKind::CompareExpr(self.op),
            TypeDef::Bool,
            vec![left_s, right_s],
            Token::NonTerminal,
        ))
    }
}

impl ast::CompareArg {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        match self {
            ast::CompareArg::GroupedLogicalExpr(expr) => {
                // This is a grouped expression that will return a boolean.
                let s = expr.eval(symbols, scope, local_scope)?;

                if s.get_type() == TypeDef::Bool {
                    Ok(s)
                } else {
                    Err("Cannot return Non-Boolean in ( )".to_string().into())
                }
            }
            ast::CompareArg::ValueExpr(expr) => {
                // A simple operator.
                trace!("COMPARE VALUE EXPRESSION {:#?}", expr);
                expr.eval(symbols, scope.clone(), local_scope)
            }
        }
    }
}

impl ast::AndExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        // An "And Expression" is a Boolean function, meaning it takes a
        // boolean as input and returns a boolean as output. That way
        // it can be composed into bigger logical expressions by combining
        // it with other boolean function, like "Or" or "Not".
        // The left and right hand in an "And Expression" must be leaf nodes,
        // meaning they have a value, not a type.
        let _symbols = symbols.clone();

        let left = self.left.eval(_symbols, scope, local_scope)?;
        let right = self.right.eval(symbols, scope, local_scope)?;

        is_boolean_function(&left, &right)?;

        Ok(symbols::Symbol::new(
            "and_expr".into(),
            symbols::SymbolKind::AndExpr,
            TypeDef::Bool,
            vec![left, right],
            Token::NonTerminal,
        ))
    }
}

impl ast::OrExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        let left = self.left.eval(_symbols, scope, local_scope)?;
        let right = self.right.eval(symbols, scope, local_scope)?;

        is_boolean_function(&left, &right)?;

        Ok(symbols::Symbol::new(
            "or_expr".into(),
            symbols::SymbolKind::OrExpr,
            TypeDef::Bool,
            vec![left, right],
            Token::NonTerminal,
        ))
    }
}

impl ast::NotExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols;

        let expr = self.expr.eval(_symbols, scope, local_scope)?;

        if expr.get_type() != TypeDef::Bool {
            return Err("Expression doesn't evaluate to a Boolean"
                .to_string()
                .into());
        };

        Ok(symbols::Symbol::new(
            "not_expr".into(),
            symbols::SymbolKind::NotExpr,
            TypeDef::Bool,
            vec![expr],
            Token::NonTerminal,
        ))
    }
}

impl ast::GroupedLogicalExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
        local_scope: &Vec<Symbol>,
    ) -> Result<symbols::Symbol, CompileError> {
        self.expr.eval(symbols, scope.clone(), local_scope)
    }
}

impl ast::ListCompareExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        // Process the left hand side of the compare expression. This is a
        // Compare Argument. It has to end in a leaf node, otherwise it's
        // an error.
        let left_s = self.left.eval(_symbols, scope.clone(), &[])?;
        let left_type = left_s.get_type();

        let right_s = self.right.eval(symbols, scope.clone(), &[])?;

        let mut l_args = vec![];
        if let TypeDef::List(_) = right_s.get_type() {
            l_args = right_s.get_args_owned();
        } else {
            l_args.push(right_s);
        }

        let mut args = vec![left_s];

        for s in l_args {
            let right_type = s.get_type();
            // Either the left and right hand sides are of the same type OR the
            // right hand side value can be converted into a type of the left
            // hand side. For example, a comparison of PrefixLength and
            // IntegerLiteral will work in the form of `prefix.len() == 32;`, but
            // NOT reversed, i.e. `32 == prefix.len();` is INVALID.
            // trace!("left_type {:#?} <-> right_type {:#?}", left_s, right_s);
            if left_type != right_type {
                args.push(s.try_convert_type_value_into(left_type.clone())?);
            } else {
                args.push(s);
            }
        }
        trace!("after conversion {} <-> {:?}", left_type, &args);

        Ok(symbols::Symbol::new(
            "list_compare_expr".into(),
            symbols::SymbolKind::ListCompareExpr(self.op),
            TypeDef::Bool,
            args,
            Token::NonTerminal,
        ))
    }
}

//============ Helper functions =============================================

fn check_type_identifier(
    ty: ast::TypeIdentifier,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<TypeDef, CompileError> {
    let symbols = symbols.borrow();
    // is it a builtin type?
    if let Ok(builtin_ty) = TypeDef::try_from(ty.clone()) {
        return Ok(builtin_ty);
    }

    // is it in the global table?
    let global_ty = symbols.get(&Scope::Global).and_then(|gt| {
        gt.get_variable(&ty.ident)
            .ok()
            .map(|s| (s.get_type(), s.get_kind()))
    });
    if let Some(ty) = global_ty {
        if ty.1 == symbols::SymbolKind::AnonymousType
            || ty.1 == symbols::SymbolKind::NamedType
        {
            return Ok(ty.0);
        }
    }

    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            // is it in the symbol table for this scope?
            let filter_map_ty = symbols
                .get(scope)
                .and_then(|gt| {
                    gt.get_variable(&ty.ident)
                        .ok()
                        .map(|s| (s.get_type(), s.get_kind()))
                })
                .ok_or(format!(
                    "No type named '{}' found in filter-map '{}'",
                    ty.ident, filter_map
                ));

            if let Ok(ty) = filter_map_ty {
                if ty.1 == symbols::SymbolKind::AnonymousType
                    || ty.1 == symbols::SymbolKind::NamedType
                {
                    return Ok(ty.0);
                }
            }
        }
        Scope::Global => {
            return Err(format!(
                "No type named '{}' found in global scope.",
                ty.ident
            )
            .into());
        }
    }

    // sorry, we don't have the type the user's asking for.
    Err(format!(
        "No type named '{}' found in scope '{}'",
        ty.ident.as_str(),
        scope
    )
    .into())
}

// This function checks if a variable exists in the scope of the filter_map,
// but not in the global scope (variables in the global scope are not
// allowed). The variables can be of form:
// <var_name>
// <var of type Record>[.<field>]+
//
// In the last case the whole form may live in the filter_map scope as an
// anonymous type (deducted from user-defined record-types), but in the case
// of a primitive type they live in the user-defined record-type itself.
//
// The last value in the return tuple is a builtin-typed value in case it's
// present in the symbol, this should only be the case with a Constant,
// containing a literal value.
fn get_props_for_scoped_variable(
    fields: &[ast::Identifier],
    symbols: GlobalSymbolTable,
    scope: Scope,
) -> Result<(SymbolKind, TypeDef, Token, Option<TypeValue>), CompileError> {
    // Implicit early return. Are there any actual fields? If not then we're
    // done, and there's nothing here.
    let first_field_name = &fields
        .first()
        .ok_or_else(|| "No name found for variable reference".to_string())?;

    let symbols = symbols.borrow();
    let search_str = fields.join(".");

    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            // 1. is the whole dotted name in the symbol table for this scope?
            return symbols
                .get(&Scope::Global)
                .and_then(|gt| {
                    gt.get_variable(&search_str.as_str().into())
                        .map(|s| s.get_props().ok())
                        .ok()
                        .flatten()
                })
                .map_or_else(
                    // No, let's go over the chain of fields to see if it's
                    // a previously defined variable, constant or data-source.
                    || {
                        let var_ty_to = symbols
                            .get(&scope)
                            .and_then(|gt| gt.get_symbol(first_field_name))
                            .map(|s| s.get_props())
                            .ok_or_else(|| {
                                format!(
                                    "___ No variable named '{}' found in \
                                filter-map '{}'",
                                    first_field_name, filter_map
                                )
                            })?;

                        let var_ty_to = var_ty_to?;
                        // This checks if `field` is present in the type
                        // variable rhs, if no field is present, it will
                        // return the whole type definition and a token
                        // FieldAcces([]).
                        // Note that checking if the rhs of this
                        // assignment completely matches the assigned
                        // type is not done here.
                        let field_ty = var_ty_to
                            .1
                            .has_fields_chain(&fields[1..])
                            .map_err(|err| {
                                trace!(
                                "{} on field '{}' for variable '{}' found in \
                                filter-map '{}'",
                                err, fields[1], fields[0].ident, filter_map
                            );
                                err
                            })?;

                        // return the type of the last field, but the token
                        // of the var/constant/data-source
                        Ok((
                            var_ty_to.0,
                            field_ty.0,
                            var_ty_to.2,
                            var_ty_to.3,
                        ))
                    },
                    // yes, it is:
                    Ok,
                );
        }
        // There is NO global scope for variables. All vars are always
        // in the namespace of a filter_map.
        Scope::Global => Err(format!(
            "=== No variable named '{}' found in global scope.",
            fields.join(".").as_str()
        )
        .into()),
    }
}

impl
    TryFrom<(
        ShortString,
        symbols::SymbolKind,
        TypeDef,
        Token,
        Option<TypeValue>,
    )> for symbols::Symbol
{
    type Error = CompileError;
    fn try_from(
        value: (
            ShortString,
            symbols::SymbolKind,
            TypeDef,
            Token,
            Option<TypeValue>,
        ),
    ) -> Result<Self, CompileError> {
        match value {
            (id, sk, ty, to, None) => {
                Ok(symbols::Symbol::new(id, sk, ty, vec![], to))
            }
            (id, sk, ty, to, Some(tv)) => {
                let s =
                    symbols::Symbol::new_with_value(id, sk, tv, vec![], to);
                if s.get_type() != ty {
                    return Err(CompileError::from(format!(
                        "Type and value do not match for {:?}",
                        s
                    )));
                }
                Ok(s)
            }
        }
    }
}

fn _declare_variable(
    name: ShortString,
    type_ident: ast::TypeIdentField,
    kind: symbols::SymbolKind,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.

    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            // Does the supplied type exist in our scope?
            let ty = check_type_identifier(type_ident.ty, _symbols, scope)?;

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols.get_mut(scope).ok_or(format!(
                "1 No filter-map named '{}' found.",
                filter_map
            ))?;

            filter_map.add_variable(
                type_ident.field_name.ident,
                Some(name),
                kind,
                ty,
                vec![],
                TypeValue::Unknown,
            )
        }
        Scope::Global => Err(format!(
            "Can't create a variable in the global scope (NEVER). \
                Variable '{}'",
            type_ident.field_name
        )
        .into()),
    }
}

fn declare_argument(
    name: ShortString,
    type_ident: ast::TypeIdentField,
    kind: symbols::SymbolKind,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.

    trace!("SCOPE {:?}", scope);
    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            // Does the supplied type exist in our scope?
            let ty = check_type_identifier(type_ident.ty, _symbols, scope)?;

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols.get_mut(scope).ok_or(format!(
                "2 No filter-map named '{}' found.",
                filter_map
            ))?;

            filter_map.add_argument(
                type_ident.field_name.ident,
                Some(name),
                kind,
                ty,
                vec![],
                TypeValue::Unknown,
            )
        }
        Scope::Global => Err(format!(
            "Can't create a variable in the global scope (NEVER). \
                Variable '{}'",
            type_ident.field_name
        )
        .into()),
    }
}

// This method stores the variable in the right place in the specified
// scope. The symbol that was passed in by the caller will be put in the
// `args` field of a newly created symbol. The new symbol will get the
// return type from the symbol that was passed in.
fn declare_variable_from_symbol(
    key: ast::ShortString,
    arg_symbol: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.
    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols.get_mut(scope).ok_or(format!(
                "No filter-map named '{}' found.",
                filter_map
            ))?;

            match arg_symbol.has_unknown_value() {
                // This is a variable, create an empty value on the symbol.
                true => {
                    let type_def = arg_symbol.get_recursive_return_type();

                    match arg_symbol.get_token() {
                        Token::TypedRecord | Token::AnonymousRecord => {
                            let fields = arg_symbol
                                .get_recursive_values_primitive(
                                    type_def.clone(),
                                )
                                .map_err(|e| {
                                    format!(
                                        "{} in type '{}'",
                                        e,
                                        arg_symbol.get_name()
                                    )
                                })?;
                            trace!("fields {:?}", fields);
                        }
                        _ => {}
                    }

                    let symbol = symbols::Symbol::new(
                        key,
                        symbols::SymbolKind::VariableAssignment,
                        type_def,
                        vec![arg_symbol],
                        Token::NonTerminal,
                    );

                    filter_map.move_var_or_const_into(symbol)
                }
                // This is a constant, move the value into the symbol we're
                // storing. This can only be a builtin-typed value.
                false => {
                    let symbol = symbols::Symbol::new_with_value(
                        key,
                        symbols::SymbolKind::Constant,
                        arg_symbol.get_value_owned(),
                        vec![],
                        Token::Constant(None),
                    );
                    filter_map.move_var_or_const_into(symbol)
                }
            }
        }
        Scope::Global => Err(format!(
            "Can't create a variable in the global scope (NEVER). \
                Variable '{}'",
            arg_symbol.get_name()
        )
        .into()),
    }
}

// fn declare_constant_from_symbol(
//     key: Option<ast::ShortString>,
//     arg_symbol: symbols::Symbol,
//     symbols: symbols::GlobalSymbolTable,
//     scope: &Scope,
// ) -> Result<(), CompileError> {
//     // There is NO global scope for variables.  All vars are all local to a
//     // filter_map.
//     match &scope {
//         Scope::FilterMap(filter_map) => {

//             let mut _symbols = symbols.borrow_mut();
//             let filter_map = _symbols
//                 .get_mut(scope)
//                 .ok_or(format!(No filter-map named '{}' found.", filter_map))?;

//             let name = arg_symbol.get_name();
//             let symbol = symbols::Symbol::new_with_value(
//                 "const".into(),
//                 symbols::SymbolKind::Constant,
//                 arg_symbol.get_value_owned(),
//                 vec![],
//                 Token::Constant,
//             );

//             filter_map.move_symbol_into(
//                 key.unwrap_or(name),
//                 symbol
//             )
//         }
//         Scope::Global => {
//             Err(format!(
//                 "Can't create a variable in the global scope (NEVER). Variable '{}'",
//                 arg_symbol.get_name()
//             )
//             .into())
//         }
//     }
// }

// Terms will be added as a vec of Logical Formulas to the `term` hashmap in
// a filter_map's symbol table. So, a term is one element of the vec.
fn add_logical_formula(
    key: ast::ShortString,
    // the index of the term section
    index: usize,
    symbol: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            drop(_symbols);

            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols.get_mut(scope).ok_or(format!(
                "No filter-map named '{}' found.",
                filter_map
            ))?;

            filter_map.add_logical_formula(key, index, symbol)
        }
        Scope::Global => Err(format!(
            "Can't create a (sub-)term in the global scope (NEVER). Term \
                '{}'",
            symbol.get_name()
        )
        .into()),
    }
}

fn add_action_section(
    name: ShortString,
    action_section: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<(), CompileError> {
    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols.get_mut(scope).ok_or(format!(
                "No filter-map named '{}' found.",
                filter_map
            ))?;

            let action = action_section.set_name(name.clone());

            filter_map.add_action_section(name, action)
        }
        Scope::Global => Err(format!(
            "Can't create an action section in the global scope (NEVER). Action '{}'",
            action_section.get_name()
        )
        .into()),
    }
}

fn add_match_action(
    name: ShortString,
    match_action: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<(), CompileError> {
    trace!("add match action {:?} with scope {:?}", name, scope);
    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols.get_mut(scope).ok_or(format!(
                "No filter-map named '{}' found.",
                filter_map
            ))?;

            match_action.get_token();

            // filter_map.move_match_action_into(Symbol::new(
            //     name,
            //     match_action.get_kind(),
            //     match_action.get_type(),
            //     match_action.get_args_owned(),
            //     Some(token),
            // ))
            // let quantifier = if let symbols::SymbolKind::MatchAction(kind) =
            //     match_action.get_kind()
            // {
            //     match kind {
            //         MatchActionType::FilterMatchAction => {
            //             symbols::MatchActionQuantifier::MatchesAny
            //         }
            //         MatchActionType::PatternMatchAction => {
            //             symbols::MatchActionQuantifier::MatchesVariant
            //         }
            //         MatchActionType::NegateMatchAction => {
            //             symbols::MatchActionQuantifier::MatchesAny
            //         }
            //         // MatchActionType::EmptyAction => {
            //         //     symbols::MatchActionQuantifier::MatchesAny
            //         // }
            //     }
            // } else {
            //     return Err(CompileError::from(format!(
            //         "Cannot select a fitting quantifier for match action {}",
            //         name
            //     )));
            // };

            let quantifier = match match_action.get_kind() {
                symbols::SymbolKind::MatchAction(MatchActionType::Filter) => {
                    symbols::MatchActionQuantifier::Any
                }
                symbols::SymbolKind::MatchAction(MatchActionType::Negate) => {
                    symbols::MatchActionQuantifier::Any
                }
                symbols::SymbolKind::GlobalEnum => {
                    symbols::MatchActionQuantifier::Variant
                }
                _ => {
                    return Err(CompileError::from(format!(
                    "Cannot select a fitting quantifier for match action {}",
                    name
                )));
                }
            };

            filter_map.move_match_action_into(match_action, quantifier)
        }
        Scope::Global => Err(format!(
            "Can't create a match action in the global scope (NEVER). Action \
            '{}'",
            match_action.get_name()
        )
        .into()),
    }
}

trait BooleanExpr
where
    Self: std::fmt::Debug,
{
    fn get_args(&self) -> &[symbols::Symbol];
    fn get_type(&self) -> TypeDef;
    fn get_builtin_type(&self) -> Result<TypeDef, CompileError>;
    fn get_token(&self) -> Token;
}

impl BooleanExpr for symbols::Symbol {
    fn get_args(&self) -> &[symbols::Symbol] {
        symbols::Symbol::get_args(self)
    }

    fn get_type(&self) -> TypeDef {
        symbols::Symbol::get_type(self)
    }

    fn get_token(&self) -> Token {
        self.get_token()
    }

    fn get_builtin_type(&self) -> Result<TypeDef, CompileError> {
        symbols::Symbol::get_builtin_type(self)
    }
}

// Since we're only accepting binary boolean functions, we only have to test
// a left and right side to comply.
fn is_boolean_function(
    left: &impl BooleanExpr,
    right: &impl BooleanExpr,
) -> Result<(), CompileError> {
    let left = (
        left.get_builtin_type()? == TypeDef::Bool,
        left.get_args().get(0).map(|a| a.get_value()),
    );
    let right = (
        right.get_builtin_type()? == TypeDef::Bool,
        right.get_args().get(0).map(|a| a.get_value()),
    );

    match (left, right) {
        ((false, None), (false, None)) => Err("Right and Left hand \
        expressions don't evaluate to boolean functions"
            .into()),
        ((_, _), (false, None)) => Err("Right hand expression doesn't \
        evaluate to a boolean function"
            .into()),
        ((false, None), (_, _)) => Err("Left hand expression doesn't \
        evaluate to a boolean function"
            .into()),
        // Only accept leaf-nodes for now. Can't think of a reason to accept these, but who knows.
        // ((_, None), (_, None)) => Err("not accepting non-leaf nodes as boolean function".into()),
        _ => Ok(()),
    }
}

// A boolean expression only accepts on expression, that should return a
// boolean value.
fn _is_boolean_expression(
    expr: &impl BooleanExpr,
) -> Result<(), CompileError> {
    if expr.get_type() == TypeDef::Bool {
        return Ok(());
    };

    if let Some(value) = expr.get_args().get(0).map(|a| a.get_value()) {
        if value.is_boolean_type() {
            return Ok(());
        };
    };

    Err("Expression doesn't evaluate to a Boolean"
        .to_string()
        .into())
}

fn _declare_variable_from_typedef(
    ident: &str,
    name: ast::ShortString,
    ty: TypeDef,
    kind: symbols::SymbolKind,
    _args: Option<ast::ArgExprList>,
    symbols: symbols::GlobalSymbolTable,
    scope: &Scope,
) -> Result<(), CompileError> {
    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.

    match &scope {
        Scope::FilterMap(filter_map) | Scope::Filter(filter_map) => {
            // drop(_symbols);

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols.get_mut(scope).ok_or(format!(
                "No filter-map named '{}' found.",
                filter_map
            ))?;

            filter_map.add_variable(
                ident.into(),
                Some(name),
                kind,
                ty,
                vec![],
                TypeValue::Unknown,
            )
        }
        Scope::Global => Err(format!(
            "Can't create a variable in the global scope (NEVER). \
                Variable '{}'",
            ident
        )
        .into()),
    }
}

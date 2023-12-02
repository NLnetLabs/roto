use log::trace;

use crate::compiler::compile::{
    compile_term, generate_code_for_token_value, CompileError, CompilerState,
};
use crate::symbols::{Symbol, SymbolKind};
use crate::traits::Token;
use crate::types::collections::{ElementTypeValue, List, Record};
use crate::types::lazyrecord_types::LazyRecordTypeDef;
use crate::types::typedef::TypeDef;
use crate::types::typevalue::TypeValue;
use crate::vm::{Command, CommandArg, CompiledCollectionField, OpCode, VmError, FieldIndex};

// This function is the heart of the compiler, all the recursion in the
// compilation process happens here. The other compile_* functions just
// trigger this function to recurse into the symbols map.
pub(crate) fn recurse_compile<'a>(
    symbol: &'a Symbol,
    mut state: CompilerState<'a>,
    // the token of the parent (the holder of the `args` field),
    // needed to retrieve methods from.
    mut parent_token: Option<Token>,
    // whether to increase the cur_mem_pos value in the CompilerState.
    // Setting this to false, allows for creating code recursively that
    // modifies the current memory position.
    inc_mem_pos: bool,
) -> Result<CompilerState<'a>, CompileError> {
    // Compute expression trees always should have the form:
    //
    // AccessReceiver (args) -> (MethodCall | FieldAccess)*
    //
    // so always starting with an AccessReceiver. Furthermore, a variable
    // reference or assignment should always bef an AccessReceiver.
    trace!(
        "recurse compile Symbol with name {} and token {:?}",
        symbol.get_name(),
        symbol.get_token()
    );
    let is_ar = symbol.get_kind() == SymbolKind::AccessReceiver;
    let token = symbol.get_token();
    let kind = symbol.get_kind();

    match token {
        // ACCESS RECEIVERS

        // A reference to a variable, seen anywhere but this can't be the
        // assignment itself, since `recurse_compile` is only used from the
        // level of the children of an assignment.
        Token::Variable(var_to) => {
            trace!("var_to {}", var_to);
            trace!("kind {:?}", symbol.get_kind());
            assert!(is_ar);
            assert!(symbol.get_kind() != SymbolKind::VariableAssignment);

            let var_ref = state
                .variable_ref_table
                .get_by_token_value(var_to)
                .ok_or_else(|| {
                    CompileError::Internal(format!(
                        "Cannot compile variable: '{}'",
                        symbol.get_name()
                    ))
                })?;

            trace!("var ref {:#?}", var_ref);

            // start a new record or increase the depth level.
            let var_type = &symbol.get_type();

            match var_type {
                TypeDef::Record(_rec_type) => {
                    match symbol.get_kind() {
                        SymbolKind::NamedType => {}
                        SymbolKind::AnonymousType => {}
                        kind => {
                            return Err(CompileError::from(format!(
                                "Type Record has invalid kind {:?}",
                                kind
                            )));
                        }
                    };
                }
                _ty => {
                    state.append_collection_to_record_tracker(
                        CompiledCollectionField::new(vec![].into()),
                    );
                }
            };

            // A referenced variable can only have zero or one child. Zero
            // children means that we're going to push all the fields of the
            // variable unto the stack.
            assert!(symbol.get_args().iter().count() < 2);
            if symbol.get_args().is_empty() {
                state.extend_commands(var_ref.get_accumulated_commands());
            }
        }
        // a user-defined argument (filter_map or term)
        Token::Argument(arg_to) => {
            assert!(is_ar);

            // if the Argument has a value, then it was set by the argument
            // injection after eval(), that means that we can just store
            // the (literal) value in the mem pos
            if let Some((_, arg, _)) = state
                .used_arguments
                .iter()
                .find(|(to, arg, _)| to == &token && !arg.has_unknown_value())
            {
                state.push_command(
                    OpCode::MemPosSet,
                    vec![
                        CommandArg::MemPos(state.cur_mem_pos),
                        CommandArg::ConstantIndex(arg.get_value().clone()),
                    ],
                );
            } else {
                state.push_command(
                    OpCode::ArgToMemPos,
                    vec![
                        CommandArg::Argument(arg_to),
                        CommandArg::MemPos(state.cur_mem_pos),
                    ],
                );
            }

            state.push_command(
                OpCode::PushStack,
                vec![CommandArg::MemPos(state.cur_mem_pos)],
            );

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }
        // The argument passed into an action in a 'with' statement. The
        // assumption is that the arguments live on the top of the stack at
        // the moment the action is called.
        Token::ActionArgument(_arg_index, _)
        | Token::TermArgument(_arg_index, _) => {
            assert!(is_ar);
        }
        // An enum variant mentioned in an arm of a match expression
        Token::Variant(_var_to) => {
            assert!(
                symbol.get_kind() == SymbolKind::EnumVariant
                    || symbol.get_kind() == SymbolKind::AccessReceiver
            );
            trace!("VARIANT {}", _var_to);
        }
        // rx instance reference
        Token::RxType(_) => {
            assert!(is_ar);
            match symbol.get_first_arg_checked() {
                Err(_) => {
                    state.push_command(
                        OpCode::PushStack,
                        vec![CommandArg::MemPos(0)],
                    );
                    return Ok(state);
                }
                Ok(first_arg) => {
                    trace!("commands for RxType named {}", symbol.get_name());
                    state.cur_record_field_name = Some(first_arg.get_name());
                    if let Some(v) = state.cur_partial_variable.as_mut() {
                        v.append_primitive(
                            crate::vm::CompiledPrimitiveField::new(
                                vec![
                                    Command::new(
                                        OpCode::PushStack,
                                        vec![CommandArg::MemPos(0)],
                                    ),
                                    Command::new(
                                        OpCode::StackOffset,
                                        vec![CommandArg::FieldIndex(
                                            first_arg.get_token().try_into()?,
                                        )],
                                    ),
                                ],
                                state.cur_record_field_index.clone(),
                            ),
                        )
                    } else {
                        state.extend_commands(vec![
                            Command::new(
                                OpCode::PushStack,
                                vec![CommandArg::MemPos(0)],
                            ),
                            Command::new(
                                OpCode::StackOffset,
                                vec![CommandArg::FieldIndex(
                                    first_arg.get_token().try_into()?,
                                )],
                            ),
                        ]);
                    };

                    // local recursion on the children of the first argument.
                    parent_token = Some(first_arg.get_token());
                    for arg in first_arg.get_args() {
                        trace!("locally recurse tx argument {:?}", arg);
                        state = recurse_compile(arg, state, parent_token, inc_mem_pos)?;
                        parent_token = Some(arg.get_token());
                    }
                    return Ok(state);
                }
            }
        }
        // tx instance reference
        Token::TxType => {
            assert!(is_ar);
            state
                .push_command(OpCode::PushStack, vec![CommandArg::MemPos(1)]);
        }
        // a constant value
        Token::Constant(_) => {
            let val = symbol.get_value();
            trace!("encode constant value {:?}", val);

            state.push_command(
                OpCode::PushStack,
                vec![CommandArg::ConstantValue(val.clone())],
            );

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }
        // Data sources
        Token::Table(_) | Token::Rib(_) | Token::OutputStream(_) => {
            assert!(is_ar);
        }
        // The AccessReceiver is a Built-in Type
        Token::BuiltinType(_b_to) => {
            assert!(is_ar);
        }
        // A Global Enum that is referenced with its Fully Qualified name,
        // e.g. AFI.IPV4. The Global Enum is the part before the dot ("AFI"),
        // for compilation it can be safely ignored. Mainly useful to
        // disambiguate variant of Global Enums that have the same name.
        Token::Enum(_) => {
            trace!("ENUM {:?}", symbol);
            trace!("ENUM VALUES {:?}", symbol.get_args());
            assert!(is_ar);
        }
        Token::ConstEnumVariant => {
            trace!("ENUM VARIANT VALUE {:?}", symbol.get_value());

            let val = symbol.get_value();

            // Push the result to the stack for an (optional) next Accessor
            // to be used.
            state.push_command(
                OpCode::PushStack,
                vec![
                    // CommandArg::MemPos(state.cur_mem_pos),
                    CommandArg::ConstantValue(
                        val.builtin_as_cloned_type_value()?,
                    ),
                ],
            );

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }

        // ARGUMENTS ON ACCESS RECEIVERS

        // Non-builtin methods can't be access receivers
        Token::Method(m_to) if !is_ar => {
            // First retrieve all arguments and the recursively compile
            // them. The result of each of them will end up on the stack
            // (when executed in the vm).
            //
            // The arguments will start out as a fresh recursion, that is to
            // say, without a parent. After the first argument we're passing
            // in the its token to its argument sibling.
            //
            // (parent, argument)  : (None, arg1), (Token(arg1), arg2) ->
            // (token(arg2), arg3), etc.
            let mut arg_parent_token = None;
            for arg in symbol.get_args() {
                state = recurse_compile(
                    arg,
                    state,
                    arg_parent_token,
                    inc_mem_pos,
                )?;
                arg_parent_token = Some(arg.get_token());
            }
            let parent_token = if let Some(parent_token) = parent_token {
                parent_token
            } else {
                return Err(CompileError::Internal(format!(
                    "Cannot compile method: {:?}",
                    symbol.get_name()
                )));
            };

            // This symbol is a method, but what is the parent?
            match parent_token {
                // The parent is a table, so this symbol is a method on a
                // table.
                Token::Table(t_to) => {
                    state.push_command(
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            CommandArg::DataSourceTable(t_to),
                            CommandArg::Method(m_to),
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    );
                }
                // The parent is a RIB, so this symbol is a method on a rib
                Token::Rib(r_to) => {
                    state.push_command(
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            CommandArg::DataSourceRib(r_to),
                            CommandArg::Method(m_to),
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    );
                }
                // The parent is a OutputStream, so this symbol is a
                // method on the OutputStream type
                Token::OutputStream(o_s) => {
                    state.push_command(
                        OpCode::PushOutputStreamQueue,
                        vec![
                            CommandArg::OutputStream(o_s),
                            CommandArg::Method(o_s),
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    );
                }
                // The parent is a built-in method, so this symbol is a
                // method on a built-in method.
                Token::BuiltinType(_b_to) => {
                    state.push_command(
                        OpCode::ExecuteTypeMethod,
                        vec![
                            CommandArg::Type(symbol.get_type()), // return type
                            CommandArg::Method(token.try_into()?), // method token
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    );
                }
                // The parent is a List
                Token::List => {
                    trace!("LIST PARENT ARGS {:#?}", symbol.get_args());
                }
                // The parent is a Record
                Token::AnonymousRecord => {
                    trace!(
                        "ANONYMOUS RECORD PARENT ARGS {:#?}",
                        symbol.get_args()
                    );
                }
                Token::TypedRecord => {
                    trace!(
                        "TYPED RECORD PARENT ARGS {:#?}",
                        symbol.get_args()
                    );
                }
                Token::Enum(_) => {
                    trace!("ENUM PARENT ARGS {:#?}", symbol.get_args());
                }
                Token::Variant(_) => {
                    trace!("VARIANT {:?}", symbol.get_args());
                }
                Token::ConstEnumVariant => {
                    trace!(
                        "CONST ENUM VARIANT PARENT ARGS {:#?}",
                        symbol.get_args()
                    );
                }
                // The parent is a Field Access, this symbol is one of:
                //
                // a Field Access, e.g. `my_field.method()`
                // a Method on a method, `my_method().my_method2()`
                // a method a user-defined var, or argument, or rx-type,
                // or tx type, e.g. `my_var.method()`
                // a method on a constant, e.g. `24.to_prefix_length()`
                Token::FieldAccess(_)
                | Token::Method(_)
                | Token::Variable(_)
                | Token::Argument(_)
                | Token::ActionArgument(_, _)
                | Token::TermArgument(_, _)
                | Token::RxType(_)
                | Token::TxType
                | Token::Constant(_) => {
                    match kind {
                        SymbolKind::MethodCallbyRef => {
                            // args: [ method_call, type, arguments,
                            //         return_type ]
                            state.push_command(
                                OpCode::ExecuteValueMethod,
                                vec![
                                    CommandArg::Method(token.try_into()?),
                                    CommandArg::Type(symbol.get_type()),
                                    CommandArg::Arguments(
                                        symbol
                                            .get_args()
                                            .iter()
                                            .map(|s| s.get_type())
                                            .collect::<Vec<_>>(),
                                    ),
                                    // argument types and number
                                    CommandArg::MemPos(state.cur_mem_pos),
                                ],
                            );
                        }
                        SymbolKind::MethodCallByConsumedValue => {
                            // args: [ method_call, type, arguments,
                            //         return_type ]
                            state.push_command(
                                OpCode::ExecuteConsumeValueMethod,
                                vec![
                                    CommandArg::Method(token.try_into()?),
                                    CommandArg::Type(symbol.get_type()),
                                    CommandArg::Arguments(
                                        symbol
                                            .get_args()
                                            .iter()
                                            .map(|s| s.get_type())
                                            .collect::<Vec<_>>(),
                                    ),
                                    // argument types and number
                                    CommandArg::MemPos(state.cur_mem_pos),
                                ],
                            );
                        }
                        kind => {
                            return Err(CompileError::Internal(format!(
                                "Invalid Method Call: '{}' with kind {:?}",
                                symbol.get_name(),
                                kind
                            )));
                        }
                    };
                }
                Token::TermSection(_) | Token::AnonymousTerm => {
                    return Err(CompileError::new(
                        "Invalid Data Source with \
                    (Anonymous)Term token "
                            .into(),
                    ));
                }
                Token::ActionSection(parent_to)
                | Token::MatchAction(parent_to) => {
                    return Err(CompileError::new(format!(
                        "Invalid data source: {:?} {:?}",
                        token, parent_to
                    )));
                }
                Token::NoAction => {
                    return Err(CompileError::from(
                        "Invalid data source: NoAction",
                    ))
                }
                Token::AnonymousEnum => {
                    return Err(CompileError::Internal(format!(
                        "Cannot compile Anonymous Enum inside method in {}",
                        symbol.get_name()
                    )));
                }
                Token::NonTerminal => {
                    return Err(CompileError::Internal(format!(
                        "Cannot compile method in {}",
                        symbol.get_name()
                    )));
                }
            };

            state.cur_mem_pos += 1;

            // Since we already have compiled in the arguments of this symbol
            // we will return here, to avoid doing it again.
            return Ok(state);
        }
        // built-in methods
        Token::Method(_m) => {
            assert!(is_ar);
        }
        // A symbol with Token::FieldAccess can be of the kind SymbolKind::
        // FieldAccess, or SymbolKind::LazyFieldAccess. The latter indicates
        // that the field access has to happen on a LazyRecord. FieldAccess
        // on a LazyRecord requires the compiler to make sure that a fresh
        // copy of that LazyRecord is on the stack for the LoadLazyValue
        // command to work on. Since a LazyRecord may be queried several
        // times in a (action/term) block, it may have been indexed already.
        Token::FieldAccess(ref fa) => {
            assert!(!is_ar);
            trace!("FieldAccess {:?}", fa);
            match parent_token {
                // This a match arm, a match arm can only have a
                // LazyFieldAccess as its kind.
                Some(Token::Variant(_var_to)) => {
                    assert_eq!(
                        symbol.get_kind(),
                        SymbolKind::LazyFieldAccess
                    );
                    trace!(
                        "FieldAccess {:?} for Variant w/ parent token {:?}",
                        fa,
                        parent_token
                    );

                    // args: [field_index_0, field_index_1, ...,
                    // lazy_record_type, variant_token, return type, store
                    // memory position]
                    let args = vec![
                        CommandArg::FieldIndex(FieldIndex::from(fa)),
                        CommandArg::Type(TypeDef::LazyRecord(
                            LazyRecordTypeDef::from(_var_to),
                        )),
                        CommandArg::Type(symbol.get_type()),
                        CommandArg::MemPos(state.cur_mem_pos),
                    ];

                    state.push_command(OpCode::LoadLazyFieldValue, args);

                    // Push the computed variant value from the memory
                    // position onto the stack.
                    // state.push_command(
                    //     OpCode::PushStack,
                    //     vec![CommandArg::MemPos(state.cur_mem_pos)],
                    // );

                    state
                        .push_command(OpCode::CondUnknownSkipToLabel, vec![]);
                }
                // The `with` argument of an Action can be of a regular
                // FieldAccess, or a LazyFieldAccess kind.
                Some(Token::ActionArgument(_, _))
                | Some(Token::TermArgument(_, _)) => {
                    trace!("name {}", symbol.get_name());
                    trace!(
                        "LazyFieldAccess {:?} with action argument {:?}",
                        fa,
                        parent_token
                    );

                    trace!("used arguments");
                    trace!("{:#?}", state.used_arguments);
                    let argument_s = state
                        .used_arguments
                        .iter()
                        .find(|(to, _, _)| Some(to) == parent_token.as_ref())
                        .map(|a| a.1)
                        .ok_or_else(|| {
                            CompileError::Internal(format!(
                                "Cannot compile argument: {}",
                                symbol.get_name()
                            ))
                        })?;

                    trace!("stored argument {:?}", argument_s);
                    trace!("state arguments {:#?}", state.used_arguments);
                    state.cur_mir_block.extend(
                        generate_code_for_token_value(
                            &state,
                            argument_s.get_token(),
                        ),
                    );

                    match symbol.get_kind() {
                        SymbolKind::LazyFieldAccess => {
                            let args = vec![
                                CommandArg::FieldIndex(FieldIndex::from(fa)),
                                CommandArg::Type(argument_s.get_type()),
                                CommandArg::Type(symbol.get_type()),
                                CommandArg::MemPos(state.cur_mem_pos),
                            ];

                            state.push_command(
                                OpCode::LoadLazyFieldValue,
                                args,
                            );

                            // state.push_command(
                            //     OpCode::PushStack,
                            //     vec![CommandArg::MemPos(state.cur_mem_pos)],
                            // );

                            state.cur_mem_pos += 1;
                        }
                        SymbolKind::FieldAccess => {
                            todo!();
                        }
                        _ => {
                            return Err(CompileError::from(format!(
                                "Invalid FieldAccess Kind in {:#?}",
                                symbol.get_name()
                            )));
                        }
                    };
                }
                // This is a regular field access, but only if we're in the
                // process of creating code for a variable assignment. This
                // code will be invoked from `compile_assignments`.
                _ if state.cur_partial_variable.is_some() => {
                    trace!("FIELD ACCESS PARENT TOKEN {:#?}", parent_token);
                    if let Some(var_refs) = &state.cur_partial_variable {
                        if let Ok(cmds) = var_refs
                            .get_commands_for_field_index(
                                fa.iter()
                                    .map(|i| usize::from(*i))
                                    .collect::<Vec<_>>()
                                    .as_slice(),
                            )
                        {
                            state.extend_commands(cmds);
                        }
                    }
                }
                // This is also regular field access, but only on a variable
                // that represents a record and when creating the variable
                // referencing code.
                Some(Token::Variable(v)) => {
                    if let Some(var_ref) =
                        state.variable_ref_table.get_by_token_value(v)
                    {
                        if let Ok(commands) = var_ref
                            .get_commands_for_field_index(
                                fa.iter()
                                    .map(|f| *f as usize)
                                    .collect::<Vec<_>>()
                                    .as_slice(),
                            )
                        {
                            state.extend_commands(commands);
                        };
                    }
                }
                // Field access on non-record types, like LazyRecord, Route,
                // etc.
                ref t => {
                    trace!("RESIDUAL FIELD ACCESS {:?}", t);
                    let mut args = vec![];
                    args.extend(
                        fa.iter()
                            .map(|t| CommandArg::FieldAccess(*t as usize))
                            .collect::<Vec<_>>(),
                    );

                    state.push_command(OpCode::StackOffset, args);
                }
            }
        }
        // A NamedTerm should be compiled with the `compile_term` method,
        // that will make sure it gets compiled and stored in the compiler
        // state `terms` hashmap, so that it can be inlined in multiple
        // places. It ending up here is an error.
        Token::TermSection(_) => {
            return Err(CompileError::new(
                "Found invalid Token of variant NamedTerm for compute \
                expression"
                    .into(),
            ));
        }
        // Anonymous terms are compiled in-line as a one off, only used as
        // blocks for a variant in a match expressions (at least for now).
        Token::AnonymousTerm => {
            trace!("TOKEN ANONYMOUS SYMBOL {:#?}", symbol);
            let sub_terms = symbol.get_args();

            let mut sub_terms = sub_terms.iter().peekable();
            while let Some(sub_term) = &mut sub_terms.next() {
                state = compile_term(sub_term, state)?;

                // Since these anonymous terms appear in a block, we're done
                // if we evaluate to `true`. But: we don't need that if this
                // is the last sub_term.
                if sub_terms.peek().is_some() {
                    state.push_command(OpCode::CondTrueSkipToEOB, vec![]);
                }
            }
            return Ok(state);
        }
        Token::MatchAction(_) => {
            return Err(CompileError::new(
                "Found invalid Token of variant MatchAction for compute \
                expression"
                    .into(),
            ));
        }
        Token::ActionSection(_) => {
            return Err(CompileError::new(
                "Found invalid Token of variant ActionSection for compute \
                expression"
                    .into(),
            ));
        }
        Token::NoAction => {
            trace!("TOKEN ACTION {:#?}", symbol);
            // This a match arm
            if let Some(Token::Variant(_var_to)) = parent_token {
                trace!("Unpack Variant w/ parent token {:?}", parent_token);

                // get the type of variant, stored in the parent `ty` field

                // args: [field_index_0, field_index_1, ...,
                // lazy_record_type, variant_token, return type, store
                // memory position]
                let args = vec![
                    CommandArg::FieldIndex(FieldIndex::new()),
                    CommandArg::Type(TypeDef::LazyRecord(
                        LazyRecordTypeDef::from(_var_to),
                    )),
                    CommandArg::Type(TypeDef::LazyRecord(
                        LazyRecordTypeDef::from(_var_to),
                    )),
                    CommandArg::MemPos(state.cur_mem_pos),
                ];

                state.push_command(OpCode::LoadLazyFieldValue, args);

                // state.push_command(
                //     OpCode::PushStack,
                //     vec![CommandArg::MemPos(state.cur_mem_pos)],
                // );

                state.push_command(OpCode::CondUnknownSkipToLabel, vec![]);
            };
        }
        // This record is defined without a type and used directly, mainly as
        // an argument for a method. The inferred type is unambiguous. This
        // token does NOT appear as a direct assignment, i.e. a variable
        // cannot be defined as a AnonymousRecord.

        // TODO: Should an AnonymousRecord be allowed to appear deeper in an
        // assignment, though? e.g. `a = Prefix.from({ address: IpAddress,
        // length: U8})`
        Token::AnonymousRecord => {
            assert!(!is_ar);

            // A new record increases the depth of the record current we are
            // tracking.
            state.inc_record_field_depth(symbol.get_name())?;

            if let Some(var) = state.cur_partial_variable.as_mut() {
                trace!("new collection {:?}", state.cur_record_field_name);
                trace!("compiled var {:?}", var);
                trace!("TYPE {:#?}", symbol.get_type());

                var.append_collection(CompiledCollectionField::new(
                    state.cur_record_field_index.clone(),
                ));
            }

            state.cur_record_field_index.push(0);
            state.cur_record_type =
                if let TypeDef::Record(rec_type) = symbol.get_type() {
                    Some(rec_type)
                } else {
                    None
                };

            // Re-order the args vec on this symbol to reflect the ordering
            // on the type definition for the record. The original ordering
            // is the order in which it was specified in the source code,
            // the resulting order is alphabetically on the names of the
            // fields. We're using a bit of a trick to re-order, since the
            // `symbol` variable is not mutable. So create a Vec<&Symbol>
            // from the original &[Symbol]. The vec can then be re-ordered
            // and we're writing the state in the order of the vec elements.
            let mut field_symbols =
                symbol.get_args().iter().collect::<Vec<_>>();
            field_symbols.sort_by_key(|a| a.get_name());
            trace!("field index {:?}", state.cur_record_field_index);
            trace!("ANONYMOUS RECORD FIELDS {:#?}", field_symbols);

            // Local recursion
            for arg in field_symbols {
                let new_field_name = arg.get_name();
                state.cur_record_field_name = Some(new_field_name.clone());
                if let Some(rec_type) = state.cur_record_type.clone() {
                    trace!("field name {:?}", new_field_name);
                    trace!("current index {:?}", state.cur_record_type);
                    if let Some(local_index) =
                        rec_type.get_index_for_field_name(&new_field_name)
                    {
                        trace!("next index {:?}", local_index);
                        if let Some(cur_index) =
                            state.cur_record_field_index.last_mut()
                        {
                            *cur_index = local_index;
                        }
                    }
                };

                state.cur_record_field_name = Some(arg.get_name());
                state = recurse_compile(arg, state, None, true)?;
            }

            state.cur_record_type =
                if let TypeDef::Record(rec_type) = symbol.get_type() {
                    Some(rec_type)
                } else {
                    None
                };

            return Ok(state);
        }
        // This is a record that appears in a variable assignment that creates
        // a record in the `Define` section or it is an argument to a method
        // call, e.g. `a = A { address: 192.0.2.0, length: /24 };` or
        // `mqtt.send(Message { msg: String.format('Withdrawal from {}`,
        // route.peer_as, asn: route.peer_as }`.
        Token::TypedRecord => {
            assert!(!is_ar);

            let mut field_symbols =
                symbol.get_args().iter().collect::<Vec<_>>();
            field_symbols.sort_by_key(|a| a.get_name());
            trace!("TYPED RECORD FIELDS {:#?}", symbol.get_args());

            // See if the supplied typedef lines up with the typedef of the
            // name of the type that was also supplied.
            // trace!("Checked Type {:#?}", symbol.get_type());
            let mut values = vec![];

            for v in symbol
                .get_recursive_values_primitive(symbol.get_type())? {
                values.push((v.0.clone(), v.2.clone().try_into().map_err(|e: VmError| CompileError::from(e.to_string()))?))
            }

            // trace!("values {:?}", values);
            // let unresolved_values = values
            //     .clone()
            //     .into_iter()
            //     .filter(|v| v.1 == TypeValue::Unknown)
            //     .collect::<Vec<_>>();
            // let unresolved_symbols = unresolved_values
            //     .into_iter()
            //     .map(|v| field_symbols.iter().find(|s| s.get_name() == v.0))
            //     .collect::<Vec<_>>();
            // trace!("unresolved symbols {:#?}", unresolved_symbols);
            let value_type = Record::new(values);
            // trace!("value_type {:?}", value_type);

            if symbol.get_type() != TypeValue::Record(value_type.clone()) {
                return Err(CompileError::from(format!(
                    "This record: {} is of type {}, but we got a record with \
                    type {}. It's not the same and cannot be converted.",
                    value_type,
                    symbol.get_type(),
                    TypeDef::Record(
                        field_symbols
                            .iter()
                            .map(|v| (v.get_name(), Box::new(v.get_type())))
                            .collect::<Vec<_>>()
                            .into()
                    )
                )));
            }

            // End of Type Check

            // A new record increases the depth of the record current we are
            // tracking.
            state.inc_record_field_depth(symbol.get_name())?;

            if let Some(var) = state.cur_partial_variable.as_mut() {
                trace!("new collection {:?}", state.cur_record_field_name);
                trace!("compiled var {:?}", var);
                trace!("TYPE {:#?}", symbol.get_type());

                var.append_collection(CompiledCollectionField::new(
                    state.cur_record_field_index.clone(),
                ));
            }

            state.cur_record_field_index.push(0);
            state.cur_record_type =
                if let TypeDef::Record(rec_type) = symbol.get_type() {
                    Some(rec_type)
                } else {
                    None
                };

            // Local recursion
            for arg in field_symbols {
                let new_field_name = arg.get_name();
                state.cur_record_field_name = Some(new_field_name.clone());
                if let Some(rec_type) = state.cur_record_type.clone() {
                    trace!("field name {:?}", new_field_name);
                    trace!("current index {:?}", state.cur_record_type);
                    if let Some(local_index) =
                        rec_type.get_index_for_field_name(&new_field_name)
                    {
                        trace!("next index {:?}", local_index);
                        if let Some(cur_index) =
                            state.cur_record_field_index.last_mut()
                        {
                            *cur_index = local_index;
                        }
                    }
                };

                state.cur_record_field_name = Some(arg.get_name());
                state = recurse_compile(arg, state, None, true)?;
            }

            state.cur_record_type =
                if let TypeDef::Record(rec_type) = symbol.get_type() {
                    Some(rec_type)
                } else {
                    None
                };

            return Ok(state);
        }
        // This is used in variable assignments where a var is assigned to
        // a list. On arrival here all the elements of the defined list will
        // be in the `args` fields. We are wrapping them all up in an actual
        // `List` and storing that in *one* memory position.
        // Anonymous Lists (lists that are defined and used immediately
        // outside of the `Define` section) don't take this code path. Those
        // appear in a ListCompareExpr and are already packed as a List.
        Token::List => {
            assert!(!is_ar);
            let mut values: Vec<ElementTypeValue> = vec![];

            for v in symbol.get_args() {
                values.push(v.get_value().clone().try_into().map_err(|e: VmError| CompileError::from(e.to_string()))?);
            }

            trace!("LIST VALUES {:?}", values);

            state.push_command(
                OpCode::PushStack,
                vec![CommandArg::List(List(values))],
            );

            return Ok(state);
        }
        Token::AnonymousEnum => {
            return Err(CompileError::Internal(format!(
                "Cannot compile Anonymous Enum in {:?}",
                symbol.get_name()
            )));
        }
        Token::NonTerminal => {
            return Err(CompileError::Internal(format!(
                "Cannot compile entity {:?}",
                symbol.get_name()
            )));
        }
    };

    // Arguments

    // The arguments are recursively compiled, similar (but not the same!) as
    // the argument compilation for methods. If the token of the current
    // symbol was Method(_) then the Token::Method match pattern above
    // already compiled the arguments and this section will be skipped.
    //
    // The argument compilation will *not* start with a fresh recursion.
    // Instead it will start with the token of the access receiver
    // and that will be passed as the parent token of the first recursion.
    // After that the parent token will be the token of the preceding
    // sibling of the current symbol:
    //
    // (parent, argument)  : (parent token, arg1), (Token(arg1), arg2) ->
    // (token(arg2), arg3), etc.

    trace!("parent token {:?}", parent_token);
    trace!("current token {:?}", symbol.get_token());

    if let Token::ActionArgument(_, _) = symbol.get_token() {
        parent_token = Some(symbol.get_token());
    } else {
        parent_token = if parent_token.is_some() {
            parent_token
        } else {
            Some(symbol.get_token())
        };
    }
    trace!("resulting token {:?}", parent_token);

    // tail recursion
    for arg in symbol.get_args() {
        state = recurse_compile(arg, state, parent_token, inc_mem_pos)?;
        parent_token = Some(arg.get_token());
    }

    Ok(state)
}

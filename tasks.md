- Make columns be 1-indexed
- In the to_literal_value, make the identifierliteral assert it has a `Literal::String`
- make advance advance a line on '\n'
- brea up parsing the different numbers
- switch out RwLock for parkinglot and .expect("rwlock is poisoned") => unwrap
- TypecheckingContext::new, use vec![val;num_val]
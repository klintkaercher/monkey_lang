
= TODO

Working on fixing Let and Return statements


I believe that `return` and `let` statements are still broken
because they're not parsing for deeper expressions?




= List of files with descriptions
== main.rs
== lib.rs
== ast.rs -> token
Defines all the forms of different statements and expressions that make up a program.
== token.rs -> leaf node
Defines all the tokens to be used in other parts of the program.
== parser.rs -> lexer, ast, token
Traverses the tokens. Parses out statements and expressions
This is the file doing most of the "work".
== lexer.rs -> token
Traverses the characters to produce tokens.

```rust
// Not used to dealing with these datatypes
let x: Box<dyn (Expression|Statement)> = Box::new();

// Here's an example of the conversion and downcasting.
let statement = (&*program.statements[0] as &dyn Any)
    .downcast_ref::<ExpressionStatement>()
    .expect("Couldn't parse as ExpressionStatement.");
```

module QUIRKY-SYNTAX
    
    // Arithmetic syntax
    syntax AExp     ::=     Int | Id
                        |   AExp AExp "/"                   [left, strict]
                        |   AExp AExp "+"                   [left, strict]
                        |   AExp AExp "-"                   [left, strict]
                        |   AExp AExp "*"                   [left, strict]
                        |   AExp AExp "%"                   [left, strict]
                        |   "(" AExp ")"                    [bracket]

    // Boolean syntax
    syntax BExp     ::=     Bool
                        |   AExp "<" AExp                   [seqstrict]
                        |   AExp ">" AExp                   [seqstrict]
                        |   AExp "<=" AExp                  [seqstrict]
                        |   AExp ">=" AExp                  [seqstrict]
                        |   "!" BExp                        [strict]
                        |   AExp "eq" AExp                  [strict]
                        |   BExp "and" BExp                 [left, strict(1)]
                        |   "(" BExp ")"                    [bracket]

    // Block structures
    syntax Block    ::=     "{" "}"
                        |   "{" Stmt "}"

    // Syntax statements
    syntax Stmt     ::=     Block
                        |   Id "=" AExp ";"                 [strict(2)]
                        |   "does" BExp
                                Block "if_not" Block        [strict(1)]
                        | "int" Ids ";"
                        | "dont_stop_if" "(" BExp ")" Block
                        | "function" "(" ")" Block
                        | "function" Id "(" ")" Block
                        > Stmt Stmt                         [left]
    
    // List syntax
    syntax Ids        ::= List{Id,","}                      [strict]
    syntax AExps      ::= List{AExp,","}                    [strict]
  
endmodule

module QUIRKY

  imports QUIRKY-SYNTAX

  // Result types
  syntax KResult ::= Int | Bool

  // Runtime environment
  configuration <T color="blue">
                  <k color="green"> $PGM:Stmt </k>
                  <state color="red"> .Map </state>
                </T>

  // Integer arithmetic
  rule <k> X:Id => I ...</k> <state>... X |-> I ...</state>
  rule I1:Int I2:Int / => I1 /Int I2  when I2 =/=Int 0
  rule I1:Int I2:Int + => I1 +Int I2
  rule I1:Int I2:Int - => I1 -Int I2
  rule I1:Int I2:Int * => I1 *Int I2
  rule I1:Int I2:Int % => I1 %Int I2

  // Boolean logic
  rule I1:Int < I2:Int => I1 <Int I2
  rule I1:Int > I2:Int => I1 >Int I2
  rule I1:Int <= I2:Int => I1 <=Int I2
  rule I1:Int >= I2:Int => I1 >=Int I2
  rule I1:Int eq I2:Int => I1 ==Int I2
  rule ! T:Bool => notBool T
  rule true and B => B
  rule false and _ => false

  // Block syntax
  rule {} => .   [structural]
  rule {S} => S  [structural]

  // Variable assignment
  rule <k> X = I:Int; => . ...</k> <state>... X |-> (_ => I) ...</state>

  // Statement evaluation
  rule S1 S2 => S1 ~> S2  [structural]

  // If-Then statement
  rule does true  S if_not _ => S
  rule does false _ if_not S => S

  // Looping construct
  rule dont_stop_if (B) S => does (B) {S dont_stop_if (B) S} if_not {}  [structural]

  // Environment variables
  rule <k> int (X:Id,Xs => Xs);...</k>
       <state> Rho (. => X |-> 0) </state>
    when notBool (X in keys(Rho))

  rule int .Ids; => .  [structural]

endmodule

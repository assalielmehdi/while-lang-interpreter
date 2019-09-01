# Small interpreter with **Haskell**

The main objective of this mini project is to make our hands dirty with the **Haskell** programming language and to make use of some functional programming features to build a very simple interpreter for the **While** language.

For simplicity, all terminals will be separated by blanks to avoid problems while lexing.

Below is the description of grammar that we will use.

```
Statment    ::= var := ArithExp StatmentAux
              | if BoolExp then { Statment } else { Statment } StatmentAux
              | while BoolExp do { Statment } StatmentAux

StatmentAux ::= ; Statment
              | epsilon

BoolExp     ::= true
              | false
              | BoolExp BoolOp BoolExp
              | ArithExp RelOp ArithExp
              | ( BoolExp )

ArithExp    ::= Term ArithExpAux

ArithExpAux ::= + Term ArithExpAux
              | - Term ArithExpAux
              | epsilon

Term        ::= Factor TermAux

TermAux     ::= * Factor TermAux
              | / Factor TermAux
              | epsilon

Factor      ::= var
              | int
              | ( ArithExp )

BoolOp      ::= and | or

RelOp       ::= > | <
```

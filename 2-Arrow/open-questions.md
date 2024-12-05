# Open questions

## Exercise 4
    What can you find out from the happy documentation about Happy’s
    handling of left-recursive and right-recursive grammars. How does this compare to the
    situation when using parser combinators? Include your answer in open-questions.md.

## Answer:
    Happy will state that there are shift/reduce conflicts if there is unhandled precedence detected. If the rule or the token has no specified precedence, the default is to shift, i.e. right recursive, but these conflicts will be reported. This is the same as parser combinators.

## Exercise 10
    Rules can be recursive. Note how recursion affects the size of the command
    stack during execution. Does it matter whether the recursive call is in the middle
    of a command sequence or at the very end of the command sequence?
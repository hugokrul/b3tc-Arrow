# Open questions

## Exercise 4
    What can you find out from the happy documentation about Happy’s
    handling of left-recursive and right-recursive grammars. How does this compare to the
    situation when using parser combinators? Include your answer in open-questions.md.

## Answer:
    if there is unhandled precedence detected, Happy will notify that there are shift/reduce conflicts. 
    You can also specify the precedence of rules using %left, %right or %nonassoc. 
    If the rule or the token has no specified precedence, the default is to shift, i.e. right recursive, but these conflicts will be reported. 
    This is the same as parser combinators.

## Exercise 10
    Rules can be recursive. Note how recursion affects the size of the command
    stack during execution. Does it matter whether the recursive call is in the middle
    of a command sequence or at the very end of the command sequence?

## Answer:
    Our program does not check for base cases. Thus if the recursion is infinite. The program will keep executing and won't fail.
    If the recursion is not infinite. The program will eventually go through (and delete) all the commands from the stack.
    Our program also adds commands to the stack dynamically, i.e., it doesn't first go through all the commands, makes a stack, and executes that stack. 
    If however the recursion is very deep and it takes a lot of iterations to execute the base case, the commands that happen after the recursive call will 
    keep on adding up and repeating. This way the stack can get very full very quick. If you have a tail-recursive call, all the commands will be executed 
    and deleted, the recursive call will happen, and the commands will be loaded in again. In conclusion, it is better to use tail-recursio. 
    This way you won't end up with a very big stack of commands that get copied everytime a recursive call in the middle of the command sequence gets executed.

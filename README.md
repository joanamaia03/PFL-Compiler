# Part 1
On the first part we started by defining the types and data. It was not difficult to decide the
types of the machine’s stack and state but it wasn’t obvious that we had to create a new data
definition, Param, for variables because the stack can contain not only integers but also
booleans.

When it comes to the functions, the most importante one was the run function, which runs a
list of instructions and returns an empty code list, a stack and the output values in the storage.
We decide to use the case method for better visual experience and a recursive method.
Because we had to deal with errors, we used if statements so it would be more organize.

The other functions were easier to implement but we just realized that we needed to have a
where clause on the conversion functions when the variables TT and FF were not being
converted to True and False on the final output.
# Parte 2
On the last part we also started by defining the data. We analysed each instruction in order to
choose the right data definition for them between data Aexp or data Bexp. Regarding the data
Stm, we chose the paramenters through the explanation of the assignement of part 2. One
important aspect is that during the making of this part we added more things to the data
definitions whenever we needed more.

For the lexer function we created a new data, such as the slides of the class, called Token.

First we made the compiler by dividing it into 3 auxiliar functions: compA, compiler for
aritmetic expressions ; compB, compiler for boolean expressions ; compStm, compiler for
statements. Then we use the compile function to concatenate the compStm function that will
also call the other auxiliar functions.

Finally we tried to make the most difficult function for us that was the parse function.

First, we created the lexer that converts a string into a list of tokens based on the lexer from
the slides.

Then we created many auxiliar functions for the parse of aritmetic and boolean expressions
that were also based on the ones from the slides.

Last but no leaste, we finish by creating another auxiliar parse function for statements (based
on the same logistics as the ones before) and calling it on the main parse function.
Note: We couldn’t test if the parse function works because we have an error on the calling of
the parseLoop function, that converts the output of the statement parse to a list of statements
so that the compiler can work.

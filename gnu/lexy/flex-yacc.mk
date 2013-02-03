lex = flex -Caer -8 --yylineno --bison-locations
# acc is for another compiler compiler
acc = bison -Wall -Werror -L C

##
## EPITECH PROJECT, 2018
## Makefile
## File description:
## make a file
##

NAME            =       imageCompressor

PACKAGE_NAME    =       imageCompressor-exe

STACK           =       stack

DIR_EXE         =       $(shell stack path --local-install-root )

all: $(NAME)

$(NAME):
		$(STACK) build
		cp $(DIR_EXE)/bin/$(PACKAGE_NAME) ./$(NAME)

clean:
		$(STACK) clean

fclean:
		$(RM) $(NAME)
		$(STACK) clean --full

re:	fclean all

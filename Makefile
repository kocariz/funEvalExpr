##
## EPITECH PROJECT, 2021
## B-FUN-500-BAR-5-1-funEvalExpr-oriol.moles-teiga
## File description:
## Makefile
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			 = 	funEvalExpr

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
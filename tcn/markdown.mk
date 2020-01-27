md2pdf := pandoc -f markdown -t latex \
		 -V geometry=a5paper -V lang=ru-RU -V babel-lang=russian

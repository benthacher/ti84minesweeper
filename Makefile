generate:
	spasm -I ./includes/ti83plus.inc minesweeper.asm ./bin/minesweeper.8xp

clean:
	rm ./bin/minesweeper.8xp

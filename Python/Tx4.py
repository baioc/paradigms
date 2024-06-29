# IMPORTS
import turtle


# CONSTANTES
WINDOW_SIZE = 340
try:
   GRID_SIZE = int(input("Grid size (default is 3): "))
   GRID_SIZE = 3 if GRID_SIZE < 1 else GRID_SIZE
except ValueError:
   GRID_SIZE = 3
OFFSET = WINDOW_SIZE / 30
TILE = (WINDOW_SIZE - 2*OFFSET) / GRID_SIZE


# FUNCOES
def map(x, in_min, in_max, out_min, out_max):
	"""Mapea valores entre diferentes intervalos, semelhante ao interlop do numpy."""
	return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min

def make_screen(width=600, height=600):
	"""Cria e retorna uma janela do Turtle Graphics."""
	screen = turtle.Screen()
	screen.setup(width, height)
	return screen

def make_cursor(color="black", width=10, speed=8, shape="arrow", is_pendown=False, is_hidden=False):
	"""Cria e retorna um cursor Turtle."""
	cursor = turtle.Turtle()
	cursor.color(color)
	cursor.width(width)
	cursor.speed(speed)
	cursor.shape(shape)
	cursor.pendown() if is_pendown else cursor.penup()
	cursor.hideturtle() if is_hidden else cursor.showturtle()
	return cursor

def draw_grid(x=0, y=0, size=3, tile=100, offset=10):
	"""
	Desenha uma grade NxN centralizada nas coordenadas dadas.

	@param size N
	@param tile Tamanho de cada unidade da grade
	@param offset Espacamento
	"""
	#criando cursor
	cursor = make_cursor(is_hidden=True)
	cursor.goto(x, y)

	#linhas horizontais
	for line in range(1, size):
		cursor.goto(x - size * tile / 2, y + tile * (size/2 - line))
		cursor.pendown()
		cursor.goto(x + size * tile / 2, y + tile * (size/2 - line))
		cursor.penup()
	
	#linhas verticais
	for column in range(1, size):
		cursor.goto(x + tile * (-size/2 + column), y + size * tile / 2)
		cursor.pendown()
		cursor.goto(x + tile * (-size/2 + column), y - size * tile / 2)
		cursor.penup()

def draw_cross(cursor, size=50):
	cursor.forward(size / 2)
	cursor.right(135)
	cursor.pendown()
	cursor.forward(size * (2**0.5))
	cursor.penup()
	cursor.left(135)
	cursor.forward(size)
	cursor.left(135)
	cursor.pendown()
	cursor.forward(size * (2**0.5))
	cursor.penup()
	cursor.right(135)
	cursor.forward(size / 2)

def check_grid(grid, tile=100):
	"""
	Confere se alguem ganhou, retorno nulo caso ninguem tenha ganho.
	"""
	size = len(grid)

	#checando cada linha
	for i, line in enumerate(grid):
		sum = 0

		for value in line:
			sum += value

		if abs(sum) == size:
			cursor = make_cursor("red", is_hidden=True)
			cursor.goto(-size * tile / 2, tile * (size/2 - 0.5 - i))
			cursor.pendown()
			cursor.forward(size * tile)
			return sum / size
	
	#checando cada coluna
	for j in range(size):
		sum = 0

		for i in range(size):
			sum += grid[i][j]

		if abs(sum) == size:
			cursor = make_cursor("red", is_hidden=True)
			cursor.goto(tile * (-size/2 + 0.5 + j), size * tile / 2)
			cursor.right(90)
			cursor.pendown()
			cursor.forward(size * tile)
			return sum / size
	
	#checando diagonais
	for diag in range(2): #referindo-se a diagonal principal e secundaria
		sum = 0

		for k in range(size):
			#print(k + diag * (size - 1 - 2*k)) #@debug para checar se a diagonal esta sendo percorrida nos dois sentidos
			sum += grid[k][k + diag * (size - 1 - 2*k)]

		if abs(sum) == size:
			cursor = make_cursor("red", is_hidden=True)
			cursor.goto(tile * size * (-0.5 + diag), size * tile / 2)
			cursor.right(45 + diag*90)
			cursor.pendown()
			cursor.forward(size * tile * (2**0.5))
			return sum / size
	
	return


# VARIAVEIS
screen = None
grid = None
player = None
current_player = 1 #variavel define de qual player eh a vez


# SETUP
def play(x, y):
	"""
	Funcao de Jogada, recebe as coordenadas do click na janela.
	"""
	screen.onscreenclick(None) #desabilita movimento durante a execucao da jogada

	#converte para valores inteiros discretos
	y = int(map(y, TILE * GRID_SIZE / 2, -TILE * GRID_SIZE / 2, 0, GRID_SIZE))
	x = int(map(x, -TILE * GRID_SIZE / 2, TILE * GRID_SIZE / 2, 0, GRID_SIZE))
	
	#garante que estara dentro dos indicies da matriz
	x = x if x <= GRID_SIZE - 1 else GRID_SIZE - 1
	y = y if y <= GRID_SIZE - 1 else GRID_SIZE - 1

	#cancela jogada se o espaco no tabuleiro ja estiver ocupado
	if grid[y][x] != 0:
		screen.onscreenclick(play)
		return
	
	#instancia a variavel que indica qual jogador esta com a vez
	global current_player

	#altera o valor na grid
	grid[y][x] = -(current_player * 2 - 3) #dessa forma player1 = 1 e player2 = -1
	#@debug da grid discreta
	"""for i in range(GRID_SIZE):
		print(grid[i])
	print("")"""

	#desenha
	player[current_player].goto(TILE * (-GRID_SIZE/2 + 0.5 + x), TILE * (GRID_SIZE/2 - 1 - y) + OFFSET)

	if (current_player % 2 == 0):
		draw_cross(player[current_player], TILE - 2*OFFSET)
	else:
		player[current_player].pendown()
		player[current_player].circle((TILE - 2*OFFSET) / 2)
		player[current_player].penup()

	#troca o player atual
	current_player = 2 if current_player == 1 else 1
	
	#confere se alguem ganhou
	hasWinner = check_grid(grid, TILE)
	if not hasWinner:
		screen.onscreenclick(play) #reabilita movimento para a proxima jogada
	else:
		winner = 2 if hasWinner == -1 else 1 #como check_grid retorna 1 ou -1, converte para os respectivos players 1 e 2
		input("Player %d wins! Press ENTER to play again." % winner)
		turtle.Screen().clear()
		setup()

def setup():
	"""Prepara o inicio do jogo."""
	global screen, grid, player, current_player
	screen = make_screen(WINDOW_SIZE, WINDOW_SIZE)
	grid = [[0 for j in range(GRID_SIZE)] for i in range(GRID_SIZE)]
	player = { 1 : make_cursor("green", 5, 8, "turtle"), 2 : make_cursor("blue", 5, 8, "turtle") }
	current_player = 1

	draw_grid(0, 0, GRID_SIZE, TILE, OFFSET)

	player[1].goto(-OFFSET, 0)
	player[2].goto(OFFSET, 0)
	player[2].left(180)

	screen.onscreenclick(play)


# JOGAR
setup()
screen.mainloop()

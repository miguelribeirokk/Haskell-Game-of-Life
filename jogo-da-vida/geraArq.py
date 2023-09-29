# Tamanho da matriz quadrada
import random

tamanho_matriz = 50

# Gere a matriz com pontos e hashtags
matriz = [['.' if random.random() < 0.5 else '#' for _ in range(tamanho_matriz)] for _ in range(tamanho_matriz)]


# Salve a matriz em um arquivo de texto
with open('jogo-da-vida/matriz2.txt', 'w') as arquivo:
    for linha in matriz:
        arquivo.write(''.join(linha) + '\n')
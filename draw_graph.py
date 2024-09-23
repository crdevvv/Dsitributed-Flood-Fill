import networkx as nx # per gestire la rete, incluso il layout
import matplotlib.pyplot as plt # per plottare la rete
import pickle # per salvare le posizioni del grafico e dei nodi
import argparse # parser, per passare il file di testo graph.txt che vogliamo all'inizio

parser = argparse.ArgumentParser()
parser.add_argument('graph', help='the graph to be used') # aggiunge l'argomento "graph" alla linea di comando
args = parser.parse_args() # prende gli argomenti del parser

# Legge il file in base al grafo selezionato da terminale
with open('c:/Program Files/Erlang OTP/usr/'+args.graph, 'r') as file:
    lines = file.readlines()

# Estrae il numero di nodi e il numero di archi
num_nodes, num_edges, graph_type, weight_type = lines[0].strip().split()

# Crea un grafo indiretto con networkx
G = nx.Graph()

# Aggiunge gli archi al grafo
for line in lines[1:]:
    node1, node2 = map(int, line.strip().split())
    G.add_edge(node1, node2)

# Genera e salva le posizioni nel file positions.pkl
pos = nx.spring_layout(G)
with open("positions.pkl", "wb") as f:
    pickle.dump(pos, f)

# Disegna il grafo (in loop, come un refresh che viene fatto ogni 100 millisecondi
# per rappresentare i cambiamenti di colore che vengono apportati nel grafo
while True :
    with open("positions.pkl", "rb") as f:
        pos = pickle.load(f) # carica le posizioni dal file
    # Apre il file dei nodi con il colori in sola lettura, altrimenti lo andrebbe a sovrascrivere
    with open('c:/Program Files/Erlang OTP/usr/NodeList.txt', 'r') as file:
        # Inizializza un dizionario per mappare colori con numero del nodo
        node_colors_dict = {}
        # legge ogni riga del file NodeList.txt
        for line in file:
            # Splitta la riga per ottenere il colore (assumendo che il formato sia 'numero colore')
            parts = line.strip().split()
            if len(parts) == 2:
                node_number = int(parts[0])
                color = parts[1]
                node_colors_dict[node_number]=color # mappa il colore al corrispettivo numero del nodo
    
    # disegna il grafo usando i colori corrispondenti di ogni nodo
    nx.draw(G, pos, with_labels=True, node_color=[node_colors_dict[node] for node in G.nodes()], edge_color='gray', node_size=500, font_size=10)
    plt.show(block=False) # mostra il plot senza bloccare l'esecuzione del codice
    plt.pause(0.1) # delay di 100 millisecondi tra un refresh e l'altro
    plt.clf() # clear figure pulisce il plot e con il nuovo ciclo, ridisegna il grafo
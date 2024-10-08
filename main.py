import ply.lex as lex
import ply.yacc as yacc
import tkinter as tk
from tkinter import scrolledtext, ttk, filedialog

# Palabras reservadas
reserved = {
    'program': 'PROGRAM',
    'if': 'IF',
    'else': 'ELSE',
    'fi': 'FI',
    'do': 'DO',
    'until': 'UNTIL',
    'while': 'WHILE',
    'read': 'READ',
    'write': 'WRITE',
    'float': 'FLOAT',
    'int': 'INT',
    'bool': 'BOOL',
    'and': 'AND',
    'or': 'OR',
    'true': 'TRUE',
    'false': 'FALSE',
    'then': 'THEN'
}

# Símbolos y otros tokens
tokens = [
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EXPONENT',
    'LT', 'LE', 'GT', 'GE', 'EQ', 'NEQ', 'ASSIGN',
    'SEMI', 'COMMA', 'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'ID', 'NUMBER'
] + list(reserved.values())

# Reglas de tokens para símbolos especiales
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EXPONENT = r'\^'
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='
t_EQ = r'=='
t_NEQ = r'!='
t_ASSIGN = r'='
t_SEMI = r';'
t_COMMA = r','
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'

# Números
def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value)
    return t

# Identificadores y palabras reservadas
def t_ID(t):
    r'[A-Za-z_][A-Za-z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t

# Ignorar espacios y tabulaciones
t_ignore = ' \t'

# Comentarios de una línea
def t_COMMENT(t):
    r'//.*'
    pass

# Comentarios de bloque
def t_COMMENT_BLOCK(t):
    r'/\*[\s\S]*?\*/'
    pass

# Nueva línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count('\n')

# Manejo de errores léxicos
def t_error(t):
    error_msg = f"Carácter ilegal '{t.value[0]}' en la línea {t.lineno}, posición {t.lexpos}\n"
    error_display.insert(tk.END, error_msg)
    t.lexer.skip(1)

# Crear el analizador léxico
lexer = lex.lex()

# Definir la gramática
def p_program(p):
    'program : PROGRAM LBRACE declarations statements RBRACE'
    p[0] = ('program', p[3], p[4])

def p_declarations(p):
    '''declarations : declarations declaration
                    | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1] + [p[2]]

def p_declaration(p):
    '''declaration : type ID declaration_list SEMI'''
    p[0] = ('declaration', p[1], [p[2]] + p[3])

def p_declaration_list(p):
    '''declaration_list : COMMA ID declaration_list
                        | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = [p[2]] + p[3]

def p_type(p):
    '''type : INT
            | FLOAT
            | BOOL'''
    p[0] = p[1]

def p_statements(p):
    '''statements : statements statement
                  | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1] + [p[2]]

def p_statement(p):
    '''statement : WRITE expression SEMI
                 | READ ID SEMI
                 | IF expression THEN LBRACE statements RBRACE ELSE LBRACE statements RBRACE FI
                 | DO LBRACE statements RBRACE UNTIL LPAREN expression RPAREN SEMI
                 | WHILE LPAREN expression RPAREN LBRACE statements RBRACE
                 | ID ASSIGN expression SEMI'''
    if p[1] == 'write':
        p[0] = ('write', p[2])
    elif p[1] == 'read':
        p[0] = ('read', p[2])
    elif p[1] == 'if':
        p[0] = ('if', p[2], p[5], p[9])
    elif p[1] == 'do':
        p[0] = ('do', p[3], p[7])
    elif p[1] == 'while':
        p[0] = ('while', p[3], p[6])
    else:
        p[0] = ('assign', p[1], p[3])

def p_expression(p):
    '''expression : expression PLUS term
                  | expression MINUS term
                  | expression LT term
                  | expression LE term
                  | expression GT term
                  | expression GE term
                  | expression EQ term
                  | expression NEQ term
                  | expression AND term
                  | expression OR term
                  | term'''
    if len(p) == 4:
        p[0] = ('binop', p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_term(p):
    '''term : term EXPONENT factor
            | term TIMES factor
            | term DIVIDE factor
            | factor'''
    if len(p) == 4:
        p[0] = ('binop', p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_factor(p):
    '''factor : NUMBER
              | ID
              | TRUE
              | FALSE
              | LPAREN expression RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_empty(p):
    'empty :'
    pass

# Manejo de errores sintácticos
def p_error(p):
    if p:
        error_msg = f"Error sintáctico en la línea {p.lineno}: Token inesperado '{p.value}'\n"
        error_display.insert(tk.END, error_msg)
    else:
        error_msg = "Error sintáctico en el EOF\n"
        error_display.insert(tk.END, error_msg)

# Crear el analizador sintáctico
parser = yacc.yacc()

# Tabla de símbolos
symbol_table = []

# Funciones de la interfaz gráfica
def analyze():
    # Reiniciar el contador de líneas y limpiar errores
    lexer.lineno = 1
    error_display.delete('1.0', tk.END)
    
    input_text = text_area.get("1.0", tk.END)
    lexer.input(input_text)
    tokens = list(lexer)
    display_tokens(tokens)
    
    result = parser.parse(input_text, lexer=lexer)
    display_symbol_table(tokens)  # Mostrar la tabla de símbolos incluso si hay errores
    display_syntax_tree(result if result else 'Errores en el análisis')  # Mostrar árbol aunque haya errores
    display_annotated_tree(result)  # Mostrar el árbol anotado

def display_tokens(tokens):
    for item in token_tree.get_children():
        token_tree.delete(item)
    for token in tokens:
        token_tree.insert('', 'end', values=(token.type, token.value, token.lineno, token.lexpos))

def display_syntax_tree(syntax_tree):
    tree_view.delete(*tree_view.get_children())
    if isinstance(syntax_tree, str):  # Si hubo errores
        tree_view.insert('', 'end', text=syntax_tree)
    else:
        display_tree_node(syntax_tree, "")


def display_annotated_tree(syntax_tree):
    annotated_tree.delete(*annotated_tree.get_children())
    if isinstance(syntax_tree, str):  # Si hubo errores
        annotated_tree.insert('', 'end', text=syntax_tree)
    else:
        display_annotated_node(syntax_tree, "")


def display_tree_node(node, parent_id=""):
    if isinstance(node, tuple):
        node_type = str(node[0])
        item_id = tree_view.insert(parent_id, 'end', text=node_type, open=True)  # Cambiado a True
        for child in node[1:]:
            display_tree_node(child, item_id)
    else:
        tree_view.insert(parent_id, 'end', text=f"Value: {node}", open=True)  # Cambiado a True

def display_annotated_node(node, parent_id=""):
    if isinstance(node, tuple):
        node_type = str(node[0])
        item_id = annotated_tree.insert(parent_id, 'end', text=f"Node: {node_type}", open=True)  # Cambiado a True
        for child in node[1:]:
            display_annotated_node(child, item_id)
    else:
        annotated_tree.insert(parent_id, 'end', text=f"Leaf: {node}", open=True)  # Cambiado a True

def display_symbol_table(tokens):
    for item in symbol_tree.get_children():
        symbol_tree.delete(item)
    for token in tokens:
        if token.type == 'ID':
            symbol_tree.insert('', 'end', values=(token.value, 'ID', token.lineno))

def open_file():
    file_path = filedialog.askopenfilename(filetypes=[("Text files", "*.txt")])
    if file_path:
        with open(file_path, 'r') as file:
            text_area.delete("1.0", tk.END)
            text_area.insert(tk.END, file.read())

def save_file():
    file_path = filedialog.asksaveasfilename(defaultextension=".txt", filetypes=[("Text files", "*.txt")])
    if file_path:
        with open(file_path, 'w') as file:
            file.write(text_area.get("1.0", tk.END))

# Crear la ventana principal
root = tk.Tk()
root.title("Analizador Léxico, Sintáctico y Semántico")

# Crear un Notebook para las pestañas
notebook = ttk.Notebook(root)
notebook.pack(fill='both', expand='yes', side=tk.LEFT)

# Pestaña de código
code_frame = ttk.Frame(notebook)
notebook.add(code_frame, text="Código")

# Pestaña de tabla de símbolos
symbol_frame = ttk.Frame(notebook)
notebook.add(symbol_frame, text="Tabla de Símbolos")

# Pestaña de tokens
token_frame = ttk.Frame(notebook)
notebook.add(token_frame, text="Tokens")

# Pestaña de árbol sintáctico
tree_frame = ttk.Frame(notebook)
notebook.add(tree_frame, text="Árbol Sintáctico")

# Crear área de texto para el código
text_area = tk.Text(code_frame, height=20)
text_area.pack(fill='both', expand=True, padx=10, pady=10)

# Botón para ejecutar el análisis
analyze_button = tk.Button(code_frame, text="Analizar", command=analyze)
analyze_button.pack()

# Crear área de texto para los errores, justo debajo del botón "Analizar"
error_display = scrolledtext.ScrolledText(code_frame, height=10)
error_display.pack(fill='both', expand=True, padx=10, pady=10)

# Tabla de tokens
token_tree = ttk.Treeview(token_frame, columns=("Token", "Valor", "Línea", "Posición"), show='headings')
token_tree.heading("Token", text="Token")
token_tree.heading("Valor", text="Valor")
token_tree.heading("Línea", text="Línea")
token_tree.heading("Posición", text="Posición")
token_tree.pack(fill='both', expand=True)

# Árbol sintáctico
tree_view = ttk.Treeview(tree_frame)
tree_view.pack(fill='both', expand=True)

# Árbol con anotaciones, colocado a la derecha
annotated_tree_frame = ttk.Frame(root)
annotated_tree_frame.pack(fill='both', expand=True, side=tk.RIGHT)

annotated_tree = ttk.Treeview(annotated_tree_frame)
annotated_tree.pack(fill='both', expand=True)

# Tabla de símbolos
symbol_tree = ttk.Treeview(symbol_frame, columns=("Nombre", "Tipo", "Línea"), show='headings')
symbol_tree.heading("Nombre", text="Nombre")
symbol_tree.heading("Tipo", text="Tipo")
symbol_tree.heading("Línea", text="Línea")
symbol_tree.pack(fill='both', expand=True)

# Crear menú
menu = tk.Menu(root)
root.config(menu=menu)

file_menu = tk.Menu(menu, tearoff=0)
menu.add_cascade(label="Archivo", menu=file_menu)
file_menu.add_command(label="Abrir", command=open_file)
file_menu.add_command(label="Guardar", command=save_file)
file_menu.add_separator()
file_menu.add_command(label="Salir", command=root.quit)

root.mainloop()
import ply.lex as lex
import ply.yacc as yacc
import tkinter as tk
from tkinter import scrolledtext, ttk, filedialog

# Palabras reservadas
reserved = {
    'program': 'PROGRAM',
    'if': 'IF',
    'else': 'ELSE',
    'end': 'END',
    'do': 'DO',
    'until': 'UNTIL',
    'while': 'WHILE',
    'read': 'READ',
    'write': 'WRITE',
    'float': 'FLOAT',
    'int': 'INT',
    'bool': 'BOOL',
    '&&': 'AND',
    '||': 'OR',
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

# Tabla de símbolos
symbol_table = {}

# Definir la gramática
def p_program(p):
    'program : PROGRAM LBRACE declarations statements RBRACE'
    p[0] = ('program', 
            ('declarations', p[3]) if p[3] else ('declarations', []),
            ('statements', p[4]) if p[4] else ('statements', [])
           )
    print("Program node:", p[0])  # Impresión de depuración

def p_declarations(p):
    '''declarations : declarations declaration
                    | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1] + [p[2]]

def p_declaration(p):
    '''declaration : type declaration_list SEMI'''
    for var in p[2]:
        symbol_table[var] = p[1]  # Agregar cada variable con su tipo a la tabla de símbolos

    p[0] = ('declaration', p[1], p[2])



def p_declaration_list(p):
    '''declaration_list : ID
                        | declaration_list COMMA ID'''
    if len(p) == 2:
        p[0] = [p[1]]  # Una sola variable, devuelvo una lista con un solo elemento
    else:
        p[0] = p[1] + [p[3]]  # Agregar más variables a la lista

def p_type(p):
    '''type : INT
            | FLOAT
            | BOOL'''
    p[0] = ('type', p[1])

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
                 | IF expression THEN LBRACE statements RBRACE ELSE LBRACE statements RBRACE END
                 | DO LBRACE statements RBRACE UNTIL LPAREN expression RPAREN SEMI
                 | WHILE LPAREN expression RPAREN LBRACE statements RBRACE
                 | ID ASSIGN expression SEMI'''
    if p[1] == 'write':
        p[0] = ('write', p[2])
    elif p[1] == 'read':
        p[0] = ('read', ('id', p[2]))
    elif p[1] == 'if':
        p[0] = ('if', p[2], ('then', p[5]), ('else', p[9]))
    elif p[1] == 'do':
        p[0] = ('do_until', ('body', p[3]), ('condition', p[7]))
    elif p[1] == 'while':
        p[0] = ('while', ('condition', p[3]), ('body', p[6]))
    else:
        # Verificar si la variable a la izquierda ya fue declarada
        if p[1] not in symbol_table:
            error_msg = f"Error: La variable '{p[1]}' no ha sido declarada en la línea {p.lineno(1)}\n"
            error_display.insert(tk.END, error_msg)
        p[0] = ('assign', ('id', p[1]), p[3])

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
    '''term : term TIMES factor
            | term DIVIDE factor
            | term EXPONENT factor
            | factor'''
    if len(p) == 4:
        if p[2] == '/':  # División
            # Verificar división por cero en tiempo de ejecución
            p[0] = ('binop', p[2], p[1], p[3], lambda x, y: None if y == 0 else x / y)
        elif p[2] == '*':
            p[0] = ('binop', p[2], p[1], p[3], lambda x, y: x * y)
        elif p[2] == '^':
            p[0] = ('binop', p[2], p[1], p[3], lambda x, y: x ** y)
    else:
        p[0] = p[1]


def p_factor(p):
    '''factor : NUMBER
              | ID
              | TRUE
              | FALSE
              | LPAREN expression RPAREN'''
    if len(p) == 2:
        if isinstance(p[1], (int, float)):
            p[0] = ('number', p[1])
        elif p[1] in ['true', 'false']:
            p[0] = ('boolean', p[1])
        else:
            p[0] = ('id', p[1])
    elif len(p) == 3:  # Caso para el menos unario
        p[0] = ('unary_minus', p[2])
    else:
        p[0] = ('group', p[2])
        



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
        
def evaluate(node):
    if isinstance(node, tuple):
        if node[0] == 'number':
            return node[1], str(node[1])
        elif node[0] == 'id':
            if node[1] in symbol_table:
                return symbol_table[node[1]]['value'], f"{node[1]}:{symbol_table[node[1]]['type']}"
            else:
                semantic_error(f"Variable '{node[1]}' no definida", node[1])
                return None, f"Error: {node[1]} no definida"
        elif node[0] == 'boolean':
            return node[1] == 'true', str(node[1])
        elif node[0] == 'binop':
            left_val, left_str = evaluate(node[2])
            right_val, right_str = evaluate(node[3])
            if left_val is None or right_val is None:
                return None, f"Error en {left_str} {node[1]} {right_str}"
            if node[1] == '+':
                return left_val + right_val, f"({left_str} + {right_str} = {left_val + right_val})"
            elif node[1] == '-':
                return left_val - right_val, f"({left_str} - {right_str} = {left_val - right_val})"
            elif node[1] == '*':
                return left_val * right_val, f"({left_str} * {right_str} = {left_val * right_val})"
            elif node[1] == '/':
                if right_val == 0:
                    semantic_error("División por cero", node[2])
                    return None, f"Error: División por cero ({left_str} / {right_str})"
                return left_val / right_val, f"({left_str} / {right_str} = {left_val / right_val})"
            elif node[1] == '^':
                return left_val ** right_val, f"({left_str} ^ {right_str} = {left_val ** right_val})"
        elif node[0] == 'unary_minus':
            val, str_rep = evaluate(node[1])
            return -val, f"(-{str_rep} = {-val})"
        elif node[0] == 'group':
            return evaluate(node[1])
        elif node[0] == 'program':
            evaluate(node[1])  # declaraciones
            return evaluate(node[2])  # sentencias
        elif node[0] == 'declarations':
            for decl in node[1]:
                evaluate(decl)
            return None, "Declaraciones procesadas"
        elif node[0] == 'declaration':
            var_type = node[1][1]
            for var in node[2]:
                symbol_table[var] = {'type': var_type, 'value': None}
            return None, f"Variables declaradas: {', '.join(node[2])} (Tipo: {var_type})"
        elif node[0] == 'statements':
            results = []
            for stmt in node[1]:
                result = evaluate(stmt)
                if result[0] is not None:
                    results.append(result[1])
            return None, "; ".join(results)
        elif node[0] == 'assign':
            value, value_str = evaluate(node[2])
            var_name = node[1][1]
            if var_name in symbol_table:
                symbol_table[var_name]['value'] = value
                return value, f"{var_name}:{symbol_table[var_name]['type']} = {value_str}"
            else:
                semantic_error(f"Variable '{var_name}' no definida", node[1])
                return None, f"Error: {var_name} no definida"
        elif node[0] == 'write':
            value, value_str = evaluate(node[1])
            print(value)  # O usa tu propia función de salida
            return value, f"write({value_str})"
    return None, "Nodo no evaluable"

def semantic_error(message, node):
    if hasattr(node, 'lineno'):
        lineno = node.lineno
    else:
        lineno = 'desconocida'
    error_msg = f"Error semántico en la línea {lineno}: {message}\n"
    error_display.insert(tk.END, error_msg)
    

# Crear el analizador sintáctico
parser = yacc.yacc()



# Funciones de la interfaz gráfica
def analyze():
    # Reiniciar el contador de líneas y limpiar errores
    lexer.lineno = 1
    error_display.delete('1.0', tk.END)
    
    symbol_table.clear()
    
    input_text = text_area.get("1.0", tk.END)
    lexer.input(input_text)
    tokens = list(lexer)
    display_tokens(tokens)
    
    result = parser.parse(input_text, lexer=lexer)
    
    if result:
        _, evaluation_result = evaluate(result)
        print("Resultado de la evaluación:", evaluation_result)
    
    # Mostrar la tabla de símbolos, árbol sintáctico, y errores
    display_symbol_table(tokens)
    display_syntax_tree(result if result else 'Errores en el análisis')
    display_annotated_tree(result)
    

def display_tokens(tokens):
    for item in token_tree.get_children():
        token_tree.delete(item)
    for token in tokens:
        token_tree.insert('', 'end', values=(token.type, token.value, token.lineno, token.lexpos))

def display_tree_node(node, parent_id=""):
    if isinstance(node, tuple):
        node_type = str(node[0])
        if node_type == 'binop':
            _, result_str = evaluate(node)
            text = f"Operation: {result_str}"
        elif node_type in ['number', 'boolean']:
            _, result_str = evaluate(node)
            text = f"{node_type.capitalize()}: {result_str}"
        elif node_type == 'id':
            var_name = node[1]
            if var_name in symbol_table:
                var_type = symbol_table[var_name]['type']
                var_value = symbol_table[var_name]['value']
                text = f"ID: {var_name} (Type: {var_type}, Value: {var_value})"
            else:
                text = f"ID: {var_name} (Undefined)"
        elif node_type == 'unary_minus':
            _, result_str = evaluate(node)
            text = f"Unary minus: {result_str}"
        elif node_type == 'assign':
            var_name = node[1][1]
            if var_name in symbol_table:
                var_type = symbol_table[var_name]['type']
                _, value_str = evaluate(node[2])
                text = f"Assign: {var_name} (Type: {var_type}) = {value_str}"
            else:
                text = f"Assign: {var_name} (Undefined) = {evaluate(node[2])[1]}"
        elif node_type == 'declaration':
            var_type = node[1][1]
            var_names = ', '.join(node[2])
            text = f"Declaration: {var_type} {var_names}"
        else:
            _, result_str = evaluate(node)
            text = f"{node_type}: {result_str}"
        
        item_id = tree_view.insert(parent_id, 'end', text=text, open=True)
        for child in node[1:]:
            if not callable(child):  # Skip lambda functions
                display_tree_node(child, item_id)
    elif isinstance(node, list):
        for item in node:
            display_tree_node(item, parent_id)
    else:
        tree_view.insert(parent_id, 'end', text=f"Value: {node}")

def display_annotated_node(node, parent_id=""):
    if isinstance(node, tuple):
        node_type = str(node[0])
        item_id = annotated_tree.insert(parent_id, 'end', text=f"Node: {node_type}", open=False)
        for child in node[1:]:
            display_annotated_node(child, item_id)
    else:
        annotated_tree.insert(parent_id, 'end', text=f"Leaf: {node}")

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
error_display = scrolledtext.ScrolledText(code_frame, height=10, wrap=tk.WORD)
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
scrollbar_tree = ttk.Scrollbar(tree_frame, orient="vertical", command=tree_view.yview)
tree_view.configure(yscrollcommand=scrollbar_tree.set)
scrollbar_tree.pack(side=tk.RIGHT, fill=tk.Y)
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
scrollbar_symbol = ttk.Scrollbar(symbol_frame, orient="vertical", command=symbol_tree.yview)
symbol_tree.configure(yscrollcommand=scrollbar_symbol.set)
scrollbar_symbol.pack(side=tk.RIGHT, fill=tk.Y)
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
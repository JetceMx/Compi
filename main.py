import ply.lex as lex
import ply.yacc as yacc
import tkinter as tk
from tkinter import scrolledtext, ttk, filedialog
from idlelib.colorizer import ColorDelegator
from idlelib.percolator import Percolator
import re

division_by_zero_reported = False

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

class CustomColorDelegator(ColorDelegator):
    def __init__(self):
        ColorDelegator.__init__(self)
        self.prog = re.compile(r'\b(?P<KEYWORD>program|if|else|end|do|until|while|read|write|float|int|bool|true|false|then)\b|'
                               r'(?P<OPERATOR>\+|-|\*|/|\^|<|<=|>|>=|==|!=|=|\|\||&&)|'
                               r'\b(?P<VARIABLE>[a-zA-Z_][a-zA-Z0-9_]*)\b|'
                               r'(?P<NUMBER>\d+(\.\d*)?|\.\d+)')

    def colorize(self, start, end):
        self.tag_remove("KEYWORD", start, end)
        self.tag_remove("OPERATOR", start, end)
        self.tag_remove("VARIABLE", start, end)
        self.tag_remove("NUMBER", start, end)
        
        for tag in ("KEYWORD", "OPERATOR", "VARIABLE", "NUMBER"):
            self.tag_configure(tag, foreground=self.colors.get(tag, "black"))
        
        self.tag_configure("KEYWORD", foreground="black")
        self.tag_configure("OPERATOR", foreground="red")
        self.tag_configure("VARIABLE", foreground="green")
        self.tag_configure("NUMBER", foreground="purple")
        
        for match in self.prog.finditer(self.text.get(start, end)):
            for key, value in match.groupdict().items():
                if value:
                    startIndex = match.start()
                    endIndex = match.end()
                    self.tag_add(key, f"{start}+{startIndex}c", f"{start}+{endIndex}c")

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
    if '.' in t.value:
        t.value = float(t.value)
    else:
        t.value = int(t.value)
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
    t.lexer.lineno += len(t.value)

# Manejo de errores léxicos
def t_error(t):
    error_msg = f"Error léxico en la línea {t.lineno}: Carácter ilegal '{t.value[0]}'\n"
    error_display.insert(tk.END, error_msg)
    t.lexer.skip(1)

# Crear el analizador léxico
lexer = lex.lex()

# Tabla de símbolos
symbol_table = {}
symbol_id_counter = 0
total_lines = 0


def update_symbol_table(name, token_type, line):
    global symbol_table, symbol_id_counter, total_lines
    if name not in symbol_table:
        symbol_id_counter += 1
        symbol_table[name] = {'id': symbol_id_counter, 'tipo': 'undefined', 'lineas': []}
    
    if token_type in ['int', 'float', 'bool'] or (symbol_table[name]['tipo'] in ['undefined', 'ID'] and token_type != 'ID'):
        symbol_table[name]['tipo'] = token_type
    
    # Solo agregamos líneas válidas (mayores que 0 y no mayores que el total de líneas)
    if 0 < line <= total_lines:
        symbol_table[name]['lineas'].append(line)
    else:
        print(f"DEBUG: Línea inválida detectada: {line} para {name}")
    
    
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
    print(f"Declaración en la línea {p.lineno(1)}")  # Añadir este print
    for var in p[2]:
        update_symbol_table(var, p[1][1], p.lineno(1))
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
        update_symbol_table(p[2], 'Variable', p.lineno(1))
        p[0] = ('read', ('id', p[2]))
    elif p[1] == 'if':
        p[0] = ('if', p[2], ('then', p[5]), ('else', p[9]))
    elif p[1] == 'do':
        p[0] = ('do_until', ('body', p[3]), ('condition', p[7]))
    elif p[1] == 'while':
        p[0] = ('while', ('condition', p[3]), ('body', p[6]))
    else:  # Asignación
        update_symbol_table(p[1], 'Variable', p.lineno(1))
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
            p[0] = ('boolean', p[1] == 'true')
        else:
            p[0] = ('id', p[1])
    else:
        p[0] = ('group', p[2])
        



def p_empty(p):
    'empty :'
    pass

# Manejo de errores sintácticos
def p_error(p):
    if p:
        # Calcular la línea real basada en el texto de entrada
        line = 1 + input_text[:p.lexpos].count('\n')
        column = p.lexpos - input_text.rfind('\n', 0, p.lexpos)
        error_msg = f"Error sintáctico en la línea {line}, columna {column}: Token inesperado '{p.value}'\n"
    else:
        error_msg = "Error sintáctico: Fin inesperado de entrada\n"
    error_display.insert(tk.END, error_msg)

def format_value(value):
    if value is None:
        return "None"
    if isinstance(value, bool):
        return str(value).lower()
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return f"{value:.2f}" if value % 1 != 0 else str(int(value))
    return str(value)
        
def evaluate(node, error_reported=False):
    global division_by_zero_reported

    if isinstance(node, tuple):
        if node[0] == 'number':
            return node[1], format_value(node[1])
        elif node[0] == 'id':
            if node[1] in symbol_table:
                if symbol_table[node[1]].get('tipo') == 'Variable':
                    error_msg = f"Error: Variable '{node[1]}' no declarada"
                    error_display.insert(tk.END, error_msg + "\n")
                    print(error_msg)  # Mantener la impresión en consola si es necesario
                    return None, error_msg
                value = symbol_table[node[1]].get('value')
                if value is None:
                    print(f"Advertencia: La variable '{node[1]}' no tiene un valor asignado.")
                    return None, f"{node[1]}:{symbol_table[node[1]].get('type', 'undefined')}=None"
                return value, f"{node[1]}:{symbol_table[node[1]].get('type', 'undefined')}={format_value(value)}"
            else:
                error_msg = f"Error: Variable '{node[1]}' no definida"
                error_display.insert(tk.END, error_msg + "\n")
                print(error_msg)  # Mantener la impresión en consola si es necesario
                return None, error_msg
        elif node[0] == 'boolean':
            return node[1] == 'true', str(node[1])
        if node[0] == 'binop':
            left_val, left_str = evaluate(node[2])
            right_val, right_str = evaluate(node[3])
            
            if left_val is None or right_val is None:
                return None, f"Error en {left_str} {node[1]} {right_str}"
            
            try:
                if node[1] == '/':
                    if right_val == 0:
                        if not division_by_zero_reported:
                            error_msg = f"Error: División por cero ({left_str} / {right_str})"
                            error_display.insert(tk.END, error_msg + "\n")
                            print(error_msg)
                            division_by_zero_reported = True
                        return None, "División por cero"
                    result = left_val / right_val
                elif node[1] == '+':
                    result = left_val + right_val
                elif node[1] == '-':
                    result = left_val - right_val
                elif node[1] == '*':
                    result = left_val * right_val
                elif node[1] == '^':
                    result = left_val ** right_val
                elif node[1] in ['<', '<=', '>', '>=', '==', '!=']:
                    result = eval(f"{left_val} {node[1]} {right_val}")
                else:
                    return None, f"Operador desconocido: {node[1]}"

                return result, f"({left_str} {node[1]} {right_str} = {format_value(result)})"

            except Exception as e:
                error_msg = f"Error en operación {node[1]}: {str(e)}"
                if not division_by_zero_reported:
                    error_display.insert(tk.END, error_msg + "\n")
                    print(error_msg)
                return None, error_msg
        elif node[0] == 'unary_minus':
            val, str_rep = evaluate(node[1])
            if val is None:
                return None, f"Error en unary_minus: {str_rep}"
            result = -val
            return result, f"(-{str_rep} = {format_value(result)})"
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
                var_type = symbol_table[var_name].get('type', 'undefined')
                try:
                    if value is None:
                        error_msg = f"Error: No se puede asignar None a {var_name}"
                        print(error_msg)
                        return None, error_msg
                    if var_type == 'int':
                        value = int(value)
                    elif var_type == 'float':
                        value = float(value)
                    elif var_type == 'bool':
                        value = bool(value)
                    symbol_table[var_name]['value'] = value
                    return value, f"{var_name}:{var_type}={format_value(value)}"
                except ValueError as e:
                    error_msg = f"Error: No se puede convertir '{value}' a {var_type}: {str(e)}"
                    print(error_msg)
                    return None, error_msg
            else:
                error_msg = f"Error: Variable '{var_name}' no definida"
                print(error_msg)
                return None, error_msg
        elif node[0] == 'write':
            value, value_str = evaluate(node[1])
            print(format_value(value))
            return value, f"write({value_str})"
        elif node[0] == 'if':
            condition_value, condition_str = evaluate(node[1])
            return condition_value, f"if condition: ({condition_str}) = {format_value(condition_value)}"
        elif node[0] == 'do_until':
            body, condition = node[1], node[2]
            body_result = evaluate(body)
            condition_value, condition_str = evaluate(condition)
            return None, f"do-until structure: body ({body_result[1]}), condition ({condition_str})"
        elif node[0] == 'while':
            condition, body = node[1], node[2]
            condition_value, condition_str = evaluate(condition)
            body_result = evaluate(body)
            return None, f"while structure: condition ({condition_str}), body ({body_result[1]})"
    return None, f"Nodo no evaluable: {node}"

def semantic_error(message, node):
    line = node.lineno if hasattr(node, 'lineno') else 'unknown'
    error_msg = f"Error semántico en la línea {line}: {message}\n"
    error_display.insert(tk.END, error_msg)
    print(error_msg)  # También imprimir en la consola para debugging
    

# Crear el analizador sintáctico
parser = yacc.yacc(debug=True)



# Funciones de la interfaz gráfica
def analyze():
    global input_text, symbol_table, symbol_id_counter, total_lines, division_by_zero_reported
    input_text = text_area.get("1.0", tk.END)
    division_by_zero_reported = False
    symbol_table.clear()
    symbol_id_counter = 0
    print("DEBUG: Tabla de símbolos reiniciada")
    
    input_text = text_area.get("1.0", tk.END)
    total_lines = input_text.count('\n') + 1
    print(f"DEBUG: Total de líneas en el código fuente: {total_lines}")
    
    lexer.lineno = 1
    
    
    lexer.input(input_text)
    tokens = list(lexer)
    
    for token in tokens:
        print(f"Token: {token.type}, Valor: {token.value}, Línea: {token.lineno}")
        if token.type == 'ID':
            update_symbol_table(token.value, token.type, token.lineno)
    print(symbol_table)
    
    display_tokens(tokens)
    
   
    result = parser.parse(input_text, lexer=lexer, tracking=True)
    print(f"DEBUG: Resultado del parsing: {result}")
 
    display_symbol_table()
    print(symbol_table)
    
    if result:
        error_display.delete('1.0', tk.END)  # Limpiar errores anteriores
        _, evaluation_result = evaluate(result)
        print("Resultado de la evaluación:", evaluation_result)
    
    display_syntax_tree(result if result else 'Errores en el análisis')

import logging
logging.basicConfig(
    level = logging.DEBUG,
    filename = "parselog.txt",
    filemode = "w",
    format = "%(filename)10s:%(lineno)4d:%(message)s"
)
log = logging.getLogger()
parser = yacc.yacc(debug=True, errorlog=log)
    

def display_tokens(tokens):
    for item in token_tree.get_children():
        token_tree.delete(item)
    for token in tokens:
        token_tree.insert('', 'end', values=(token.type, token.value, token.lineno, token.lexpos))

def display_tree_node(node, parent_id="", error_reported=False):
    if isinstance(node, tuple):
        node_type = str(node[0])
        try:
            if node_type == 'binop':
                result, result_str = evaluate(node)
                if result is None:
                    text = f"Error: {result_str}"
                else:
                    text = f"Operation: {result_str}"
            elif node_type == 'number':
                value, _ = evaluate(node)
                value_type = 'int' if isinstance(value, int) else 'float'
                text = f"Number ({value_type}): {format_value(value)}"
            elif node_type == 'boolean':
                value, _ = evaluate(node)
                text = f"Boolean: {str(value).lower()}"
            elif node_type == 'id':
                _, result_str = evaluate(node)
                text = f"ID: {result_str}"
            elif node_type == 'unary_minus':
                _, result_str = evaluate(node)
                text = f"Unary minus: {result_str}"
            elif node_type == 'assign':
                _, result_str = evaluate(node)
                text = f"Assign: {result_str}"
            elif node_type == 'declaration':
                var_type = node[1][1]
                var_names = ', '.join(node[2])
                text = f"Declaration: {var_type} {var_names}"
            elif node_type == 'do_until':
                body_str = evaluate(node[1])[1]
                condition_str = evaluate(node[2])[1]
                text = f"Do-Until: body ({body_str}), until ({condition_str})"
            elif node_type == 'while':
                condition_str = evaluate(node[1])[1]
                body_str = evaluate(node[2])[1]
                text = f"While: condition ({condition_str}), body ({body_str})"
            else:
                _, result_str = evaluate(node)
                text = f"{node_type}: {result_str}"
        except Exception as e:
            text = f"Error in {node_type}: {str(e)}"
        
        item_id = annotated_tree.insert(parent_id, 'end', text=text, open=True)
        for child in node[1:]:
            if not callable(child):  # Skip lambda functions
                display_tree_node(child, item_id)
    elif isinstance(node, list):
        for item in node:
            display_tree_node(item, parent_id)
    else:
        # Aquí es donde manejamos los valores simples (Value: x, y, z)
        if isinstance(node, (int, float)):
            data_type = 'int' if isinstance(node, int) else 'float'
            text = f"Value: {node} (Type: {data_type})"
        elif isinstance(node, str) and node in symbol_table:
            var_type = symbol_table[node].get('type', 'undefined')
            text = f"Value: {node} (Type: {var_type})"
        else:
            text = f"Value: {node}"
        annotated_tree.insert(parent_id, 'end', text=text)

def display_annotated_node(node, parent_id=""):
    if isinstance(node, tuple):
        node_type = str(node[0])
        item_id = annotated_tree.insert(parent_id, 'end', text=f"Node: {node_type}", open=False)
        for child in node[1:]:
            display_annotated_node(child, item_id)
    else:
        annotated_tree.insert(parent_id, 'end', text=f"Leaf: {node}")

def display_syntax_tree(syntax_tree):
    annotated_tree.delete(*annotated_tree.get_children())
    if isinstance(syntax_tree, str):  # Si hubo errores
        annotated_tree.insert('', 'end', text=syntax_tree)
    else:
        display_tree_node(syntax_tree, "", False)

def display_annotated_tree(syntax_tree):
    tree_view.delete(*tree_view.get_children())
    if isinstance(syntax_tree, str):  # Si hubo errores
        tree_view.insert('', 'end', text=syntax_tree)
    else:
        display_tree_node(syntax_tree, "")

def display_symbol_table():
    global total_lines
    for item in symbol_tree.get_children():
        symbol_tree.delete(item)
    for name, info in symbol_table.items():
        valid_lines = [line for line in info['lineas'] if 0 < line <= total_lines]
        invalid_lines = [line for line in info['lineas'] if line > total_lines]
        if invalid_lines:
            print(f"ADVERTENCIA: Líneas fuera de rango detectadas para {name}: {invalid_lines}")
        symbol_tree.insert('', 'end', values=(
            info['id'],
            name,
            info['tipo'],
            ', '.join(map(str, valid_lines))
        ))

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
root.title("Compilador By Jetce_Mx(Esau) y Wizardkiller117(Sebas)")

# Crear un Notebook para las pestañas
notebook = ttk.Notebook(root)
notebook.pack( side=tk.LEFT)

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
text_frame = tk.Frame(code_frame)
text_frame.pack(fill='both', expand=True, padx=10, pady=10)

text_area = scrolledtext.ScrolledText(text_frame, wrap=tk.NONE, undo=True)
text_area.pack(side=tk.RIGHT, fill='both', expand=True)

line_numbers = tk.Text(text_frame, width=4, padx=3, takefocus=0, border=0,
                       background='lightgrey', state='disabled')
line_numbers.pack(side=tk.LEFT, fill='y')

def update_line_numbers(event=None):
    line_numbers.config(state='normal')
    line_numbers.delete('1.0', tk.END)
    num_lines = text_area.get('1.0', tk.END).count('\n')
    for i in range(1, num_lines + 1):
        line_numbers.insert(tk.END, f"{i}\n")
    line_numbers.config(state='disabled')
    line_numbers.yview_moveto(text_area.yview()[0])

text_area.bind('<KeyRelease>', update_line_numbers)
text_area.bind('<MouseWheel>', update_line_numbers)

def on_text_scroll(*args):
    line_numbers.yview_moveto(args[0])
text_area.vbar.config(command=lambda *args: (text_area.yview(*args), on_text_scroll(*args)))

color_delegator = CustomColorDelegator()
Percolator(text_area).insertfilter(color_delegator)

update_line_numbers()

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
symbol_tree = ttk.Treeview(symbol_frame, columns=("ID", "Nombre", "Tipo", "Líneas"), show='headings')
symbol_tree.heading("ID", text="ID")
symbol_tree.heading("Nombre", text="Nombre")
symbol_tree.heading("Tipo", text="Tipo")
symbol_tree.heading("Líneas", text="Líneas")
symbol_tree.column("ID", width=10)  # Ajusta este valor según lo delgado que quieras
symbol_tree.column("Nombre", width=10)  # Puedes ajustar los valores de ancho
symbol_tree.column("Tipo", width=10)  # Ajusta a tu preferencia
symbol_tree.column("Líneas", width=200)  # Establece el ancho deseado
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